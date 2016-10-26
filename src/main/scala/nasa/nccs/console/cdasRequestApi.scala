package nasa.nccs.console

import java.io.PrintWriter
import java.nio.file.Paths

import nasa.nccs.esgf.process.TaskRequest
import nasa.nccs.esgf.utilities.numbers.GenericNumber

trait NodeProcessor {
  def attr( node: xml.Node, att_name: String ): String = { node.attribute(att_name) match { case None => ""; case Some(x) => x.toString }}
  def attrOpt( node: xml.Node, att_name: String ): Option[String] = node.attribute(att_name).map( _.toString )
}

trait WpsNode extends NodeProcessor {
  def toWps: String
  override def toString: String = toWps
}

trait WpsData extends WpsNode {}

object Fragment extends NodeProcessor {
  def apply( varNode: xml.Node, uid: String, domain: String ): Fragment = {
    val varname = attr( varNode, "variable" )
    val origin = attr( varNode, "origin" )
    val shape = attr( varNode, "shape" )
    val coll = attr( varNode, "coll" )
    new Fragment( uid, varname, origin, shape, coll, domain )
  }
}

class Fragment( val uid: String, val varname: String, val origin: String, val shape: String, val coll: String, val domain: String ) extends WpsData {
  def toWps: String = """{"uri":"%s","name":"%s:%s","domain":"%s"}""".format( "fragment:/"+key, varname, uid, domain )
  def key: String = Array( varname, coll, origin, shape ).mkString("|")
  override def toString: String = toWps
}

object Variable extends NodeProcessor {
  def apply( varNode: xml.Node, uid: String, domain: String ): Variable = {
    val id = attr( varNode, "id" )
    val colId = attr( varNode, "collection" )
    new Variable( uid, "collection:/" + colId, id, domain )
  }
}

class Variable( val uid: String, val uri: String="", val varname: String="", val domain_id: String="" ) extends WpsData {
  val fullName = List( varname, uid ).filterNot(_.isEmpty ).mkString(":")
  def toWps: String = """{"uri":"%s","name":"%s","domain":"%s"}""".format( uri, fullName, domain_id )
  override def toString: String = toWps
}

object Collection extends NodeProcessor {
  def apply( colNode: xml.Node ): Collection = {
    val id = attr( colNode, "id" )
    val path = attr( colNode, "path" )
    new Collection( id, "collection:/" + id, path )
  }
}

class Collection( val id: String, val uri: String, val path: String )  extends WpsData {
  def toWps: String = """{"id":"%s","uri":"%s","path":"%s"}""".format( id, uri, path )
  override def toString: String = toWps
}

class Axis(val id: String, val start: GenericNumber, val end: GenericNumber, val system: String ) extends WpsNode {

  def toWps: String = """"%s":{"start":%s,"end":%s,"system":"%s"}""".format( id, getBoundStr(start), getBoundStr(end), system )
  override def toString: String = """"%s":[%s,%s,"%s"]""".format( id, getBoundStr(start), getBoundStr(end), system )

  def getBoundStr( value: GenericNumber ): String = {
    system match {
      case x: String if(x.startsWith("ind")) => value.toInt.toString
      case x: String if(x.startsWith("val")) => value.toFloat.toString
      case x: String if(id.toLowerCase.startsWith("tim")) => value.toString
      case x => throw new Exception( "Unrecognized axis bound:" + x.toString )
    }
  }
}

class Domain( val id: String, val axisMap: Map[String,Axis] = Map.empty[String,Axis] )  extends WpsNode {
  lazy val axes = axisMap.values.toList
  def toWps: String =
    if( axes.length == 0 ) """{"name":"%s"}""".format(id)
    else """{"name":"%s",%s}""".format( id, axes.map(_.toWps).mkString(",") )

  override def toString: String = toWps
  def getAxis( id: String ): Option[Axis] = axisMap.get( id )
}


class Operation( val pkg: String, val kernel: String, val input_uids: Array[String], val args: Map[String,String], val result_id:String = "" )  extends WpsNode {
  def toWps(): String = """{"name":"%s","input":"%s", %s }""".format( kernel, input_uids.mkString(","), args.map { case (k,v) => s""""$k":"$v""""  }.mkString(", ") )
  override def toString: String = toWps
}


class CDASClientRequestManager {
  val server = getServerAddress
  val port = getServerPort
  val logger = new PrintWriter(new java.io.File( Paths.get( System.getProperty("user.home"), ".cdas", "cdshell.log" ).toString ))

  def log( msg: String )= { logger.write( msg + "\n" ); logger.flush }

  private def getBaseRequest(async: Boolean): String = """http://%s:%s/wps?request=Execute&service=cds2&status=%s""".format(server, port, async.toString)

  private def getResultRequest(id: String): String = """http://%s:%s/wps/results?id=%s""".format( server, port, id )

  private def getResultFileRequest(id: String): String = """http://%s:%s/wps/file?id=%s""".format( server, port, id )

  private def getCapabilities(identifier: String): String = """http://%s:%s/wps?request=getCapabilities&service=cds2&identifier=%s""".format( server, port, identifier )

  private def describeProcess(processId: String): String = """http://%s:%s/wps?request=describeProcess&service=cds2&identifier=%s""".format(server, port, processId)

  private def toDomainWps(domains: Domain*): Option[String] = if(domains.isEmpty) None else Some("""domain=[%s]""".format(domains.map(_.toWps).mkString(",")))

  private def toVariableWps(variables: WpsData*): Option[String] = if(variables.isEmpty) None else Some("""variable=[%s]""".format(variables.map(_.toWps).mkString(",")))

  private def toOperationWps(operations: Operation*): Option[String] = if(operations.isEmpty) None else Some("""operation=[%s]""".format(operations.map(_.toWps).mkString(",")))

  private def getDatainputs(domains: List[Domain], fragments: List[WpsData], operations: List[Operation]): String =
    "datainputs=" + Array( toDomainWps(domains:_*), toVariableWps(fragments:_*), toOperationWps(operations:_*) ).flatten.mkString("[",",","]")

  def getRequest(async: Boolean, identifier: String, domains: List[Domain], fragments: List[WpsData], operations: List[Operation]): String = {
    Array( getBaseRequest(async), "identifier="+identifier, getDatainputs( domains, fragments, operations) ).mkString("&").replaceAll("\\s+","")
  }

  def getServerPort: String = sys.env.getOrElse("CDAS_SERVER_PORT",9000).toString
  def getServerAddress: String = sys.env.getOrElse("CDAS_SERVER_ADDRESS","localhost").toString

  def submitRequest( request: String ): xml.Elem = try {
    log( "Request: " + request )
    val response = scala.io.Source.fromURL(request).mkString
    log( "Response: " + response )
    scala.xml.XML.loadString(response)
  } catch {
    case err: java.net.ConnectException => <error> { "Error connecting to analytics server: " + err.getMessage } </error>
    case error: Exception => <error> { "Error executing Request: " + error.getMessage } </error>
  }

  def requestCapabilities(identifier: String): xml.Elem = submitRequest( getCapabilities(identifier) )

  def submitRequest( async: Boolean, identifier: String, domains: List[Domain], fragments: List[WpsData], operations: List[Operation] ): xml.Elem = {
    val request = getRequest( async, identifier, domains, fragments, operations )
    log( "Submit Request: " + request )
    val response = scala.io.Source.fromURL(request).mkString
    log( "Received Response: " + response )
    scala.xml.XML.loadString(response)
  }

  def submitResultRequest( mtype: String, rid: String ): xml.Elem = {
    val request: String = mtype match {
      case x if x.toLowerCase.contains("netcdf") => getResultFileRequest( rid )
      case x if x.toLowerCase.contains("xml") => getResultRequest( rid )
      case x => throw new Exception( "Unrecognized Mime Type: " + mtype )
    }
    log( "Submit Request: " + request )
    val response = scala.io.Source.fromURL(request).mkString
    log( "Received Response: " + response )
    scala.xml.XML.loadString(response)
  }

  def submitTimedRequest( request: String, connectTimeout:Int =5000, readTimeout:Int =5000 ): String = {
    import java.net.{URL, HttpURLConnection}
    val connection = (new URL(request)).openConnection.asInstanceOf[HttpURLConnection]
    connection.setConnectTimeout(connectTimeout)
    connection.setReadTimeout(readTimeout)
    connection.setRequestMethod("GET")
    val inputStream = connection.getInputStream
    val content = scala.io.Source.fromInputStream(inputStream).mkString
    if (inputStream != null) inputStream.close
    content
  }

}

object localClientRequestManager extends CDASClientRequestManager() { }

