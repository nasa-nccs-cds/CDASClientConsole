package nasa.nccs.console
import nasa.nccs.esgf.process.TaskRequest
import nasa.nccs.esgf.utilities.numbers.GenericNumber

trait NodeProcessor {
  def attr( node: xml.Node, att_name: String ): String = { node.attribute(att_name) match { case None => ""; case Some(x) => x.toString }}
  def attrOpt( node: xml.Node, att_name: String ): Option[String] = node.attribute(att_name).map( _.toString )
}

trait WpsNode extends NodeProcessor {
  def toWps: String
}

trait WpsData extends WpsNode {}

object Variable extends NodeProcessor {
  def apply( varNode: xml.Node, uid: String, domain: String ): Variable = {
    val id = attr( varNode, "id" )
    val colId = attr( varNode, "collection" )
    new Variable( uid, "collection:/" + colId, id, domain )
  }
}

class Variable( val uid: String, val uri: String, val varname: String, val domain_id: String ) extends WpsData {
  def toWps: String = """{"uri":"%s","name":"%s:%s","domain":"%s"}""".format( uri, varname, uid, domain_id )
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
}

class Axis(val id: String, val start: GenericNumber, val end: GenericNumber, val system: String ) extends WpsNode {

  def toWps: String = """"%s":{"start":%s,"end":%s,"system":"%s"}""".format( id, getBoundStr(start), getBoundStr(end), system )

  def getBoundStr( value: GenericNumber ): String = {
    system match {
      case x: String if(x.startsWith("ind")) => value.toInt.toString
      case x: String if(x.startsWith("val")) => value.toFloat.toString
      case x: String if(id.toLowerCase.startsWith("tim")) => value.toString
      case x => throw new Exception( "Unrecognized axis bound:" + x.toString )
    }
  }
}

class Domain( val id: String, val axes: List[Axis] = List.empty[Axis] )  extends WpsNode {
  def toWps: String =
    if( axes.length == 0 ) """{"name":"%s"}""".format(id)
    else """{"name":"%s",%s}""".format( id, axes.map(_.toWps).mkString(",") )
}


class Operation( val pkg: String, val kernel: String, val input_uids: Array[String], val args: Map[String,String], val result_id:String = "" )  extends WpsNode {
  val identifier = pkg + "." + kernel
  def toWps(): String = """ {"name":"%s","input":}""".format( identifier )
}


class CDASClientRequestManager( val server: String ="localhost", val port: Int =9000 ) {

  private def getBaseRequest(async: Boolean): String = """http://%s:%s/wps?request=Execute&service=cds2&status=%s""".format(server, port, async.toString)

  private def getCapabilities(identifier: String): String = """http://%s:%s/wps?request=getCapabilities&service=cds2&identifier=%s""".format( server, port, identifier )

  private def describeProcess(processId: String): String = """http://%s:%s/wps?request=Execute&service=cds2&identifier=%s""".format(server, port, processId)

  private def toDomainWps(domains: Domain*): Option[String] = if(domains.isEmpty) None else Some("""domain=[%s]""".format(domains.map(_.toWps).mkString(",")))

  private def toVariableWps(variables: WpsData*): Option[String] = if(variables.isEmpty) None else Some("""variable=[%s]""".format(variables.map(_.toWps).mkString(",")))

  private def toOperationWps(operations: Operation*): Option[String] = if(operations.isEmpty) None else Some("""operation=[%s]""".format(operations.map(_.toWps).mkString(",")))

  private def getDatainputs(domains: List[Domain], variables: List[WpsData], operations: List[Operation]): String =
    "datainputs=" + Array( toDomainWps(domains:_*), toVariableWps(variables:_*), toOperationWps(operations:_*) ).flatten.mkString("[",",","]")

  def getRequest(async: Boolean, identifier: String, domains: List[Domain], variables: List[WpsData], operations: List[Operation]): String = {
    Array( getBaseRequest(async), "identifier="+identifier, getDatainputs( domains, variables, operations) ).mkString("&")
  }

  def submitRequest( request: String ): xml.Elem = scala.xml.XML.loadString( scala.io.Source.fromURL( request ).mkString )

  def requestCapabilities(identifier: String): xml.Elem = submitRequest( getCapabilities(identifier) )

  def submitRequest( async: Boolean, identifier: String, domains: List[Domain], variables: List[WpsData], operations: List[Operation] ): xml.Elem = {
    val request = getRequest( async, identifier, domains, variables, operations )
    println( "Generated Request: " + request )
    scala.xml.XML.loadString(scala.io.Source.fromURL(request).mkString)
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

