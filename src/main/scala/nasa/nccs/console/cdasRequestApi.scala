package nasa.nccs.console
import nasa.nccs.esgf.process.TaskRequest
import nasa.nccs.esgf.utilities.numbers.GenericNumber

class Variable( val uid: String, val uri: String, val varname: String, val domain_id: String ) {
  def toWps: String = """{ "uri": "%s", "name": "%s:%s", "domain": "%s" }""".format( uri, varname, uid, domain_id )
}

class Axis(val id: String, val start: GenericNumber, val end: GenericNumber, val system: String ) {

  def toWps: String = """ "%s": {"start" :%s, "end" :%s, "system": "%s"} """.format( id, getBoundStr(start), getBoundStr(end), system )

  def getBoundStr( value: GenericNumber ): String = {
    system match {
      case x: String if(x.startsWith("ind")) => value.toInt.toString
      case x: String if(x.startsWith("val")) => value.toFloat.toString
      case x: String if(id.toLowerCase.startsWith("tim")) => value.toString
      case x => throw new Exception( "Unrecognized axis bound:" + x.toString )
    }
  }
}

class Domain( val id: String, val axes: List[Axis] = List.empty[Axis] ) {
  def toWps: String = """{ "name": "%s", %s }""".format( id, axes.map(_.toWps).mkString(",") )
}


class Operation( val pkg: String, val kernel: String, val input_uids: Array[String], val args: Map[String,String], val result_id:String = "" ) {
  val identifier = pkg + "." + kernel
  def toWps(): String = """ { "name": "%s", "input":  } """.format( identifier )
}


class CDASClientRequestManager( val server: String ="localhost", val port: Int =9000 ) {

  private def getBaseRequest(async: Boolean): String = """http://%s:%s/wps?request=Execute&service=cds2&status=%s""".format(server, port, async.toString)

  private def getCapabilities(identifier: String): String = """http://%s:%s/wps?request=getCapabilities&service=cds2&identifier=%s""".format( server, port, identifier )

  private def describeProcess(processId: String): String = """http://%s:%s/wps?request=Execute&service=cds2&identifier=%s""".format(server, port, processId)

  private def toDomainWps(domains: Domain*): String = """domain=[%s]""".format(domains.map(_.toWps).mkString(","))

  private def toVariableWps(variables: Variable*): String = """ variable=[%s]""".format(variables.map(_.toWps).mkString(","))

  private def toOperationWps(operations: Operation*): String = """ operation=[%s]""".format(operations.map(_.toWps).mkString(","))

  private def getDatainputs(domains: List[Domain], variables: List[Variable], operations: List[Operation]): String =
    """datainputs=[%s,%s,%s]""".format( toDomainWps(domains:_*), toVariableWps(variables:_*), toOperationWps(operations:_*) )

  def getRequest(async: Boolean, identifier: String, domains: List[Domain], variables: List[Variable], operations: List[Operation]): String = {
    Array( getBaseRequest(async), identifier, getDatainputs( domains, variables, operations) ).mkString("&")
  }

  def submitRequest( request: String ): xml.Elem = scala.xml.XML.loadString( scala.io.Source.fromURL( request ).mkString )
  def requestCapabilities(identifier: String): xml.Elem = submitRequest( getCapabilities(identifier) )

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