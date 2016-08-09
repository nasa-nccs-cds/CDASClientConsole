package nasa.nccs.console

import ucar.nc2.time.CalendarDate
import com.googlecode.concurrentlinkedhashmap.ConcurrentLinkedHashMap
import nasa.nccs.esgf.utilities.numbers.GenericNumber
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

object cdasDomainManager {

  private val domainMap = new ConcurrentLinkedHashMap.Builder[String, Domain ].initialCapacity(100).maximumWeightedCapacity(10000).build()
  domainMap.put( "d0", new Domain( "d0" ) )
  domainMap.put( "d1", new Domain( "d1", Map( "lev" -> new Axis( "lev", 0, 0, "indices" ) ) ) )
  def getDomain(domId: String ): Option[Domain] = Option(domainMap.get(domId))
  def putDomain(domId: String, domain: Domain ) = domainMap.put(domId, domain)

  def createDomain( inputs: Vector[String] ) = {
    val domainId = "d" + domainMap.size
    val domainAxes = inputs zip List( "lat", "lon", "lev", "time" ) map { case ( input: String, id ) => createDomainAxis( id, input ) }
    val domain = new Domain( domainId, Map( domainAxes.flatten.map( axis => (axis.id -> axis ) ): _* ) )
    putDomain( domainId, domain )
    println( "Created Domain %s: %s".format( domainId, domain.toString ) )
  }

  def getDomains: IndexedSeq[Domain] = domainMap.values.toIndexedSeq

  def getDomainSelectionList( state: ShellState  ): Array[String] = {
    getDomains.map( (dc: Domain) => dc.id + ": " + dc.axes.mkString("{",", ","}") ).toArray
  }

  def createDomainAxis( id:String, input: String ): Option[Axis] = {
    println( s"Creating domain axis ($id): $input" )
    if(input.isEmpty) None else {
      val args = getArgs(input)
      val vtype = args(0) match {
        case x if x.startsWith("i") => "indices"
        case x if x.startsWith("v") => "values"
        case x => throw new Exception("Unrecognized value/index specification: " + x)
      }
      val b0 = getNumber( id, vtype, args(1) )
      val b1 = if (args.length < 3) b0 else getNumber( id, vtype, args(2) )
      Some(new Axis( id, b0, b1, vtype) )
    }
  }

  def domainAxisValidator( id: String )( input: String ): Option[String] = {
    if(input.isEmpty) return  None
    val args = getArgs(input)
    if( args.length < 2 ) return Some("Missing input")
    if( !(args(0).startsWith("i") || args(0).startsWith("v")) ) return Some("Unrecognized value/index specification: '%s'".format(args(0)) )
    args(0) match {
      case x if x.startsWith("i") =>
        if( !validInt( args(1) ) ) return Some("Invalid bounds index: '%s'".format(args(1)) )
        if( (args.length > 2) && !validInt( args(2) ) ) return Some("Invalid bounds index: '%s'".format(args(2)) )
      case x if x.startsWith("v") =>
        if( id.startsWith("tim") ) {
          if (!validTime(args(1))) return Some("Invalid ISO (YYYY-MM-DDThh:mm) date/time value: '%s'".format(args(1)) )
          if ((args.length > 2) && !validTime(args(2))) return Some("Invalid ISO (YYYY-MM-DDThh:mm) date/time value: '%s'".format(args(2)) )
        } else {
          if (!validFloat(args(1))) return Some("Invalid bounds value: '%s'".format(args(1)) )
          if ((args.length > 2) && !validFloat(args(2))) return Some("Invalid bounds value: '%s'".format(args(2)) )
        }
    }
    None
  }

  def defineDomainHandler: MultiStepCommandHandler = new MultiStepCommandHandler( "[d]omain", "Define new domain", Vector("Lon","Lat","Level","Time").map(_+" bounds: <[i]ndex/[v]alue>, <bound0>, (<bound1>) >> "),
        Vector("lat","lon","lev","time").map(domainAxisValidator(_) _ ), (vals,state) => { createDomain( vals ); state } )

  def selectDomainCommand( prompt: String = "Select domain(s)" ): ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[sd]omain", prompt, getDomainSelectionList, ( cids:Array[String], state ) => { state :+ Map( "domains" -> <array>{cids.map( cid => cid.split(':')(0).trim ) }</array> ) } )
  }

  def validFloat( input: String ): Boolean = try { input.toFloat; true } catch { case ex: Throwable => false }
  def validInt( input: String ): Boolean = try { input.toInt; true } catch { case ex: Throwable => false }
  def validTime( input: String ): Boolean = try { CalendarDate.parseISOformat(null,input); true } catch { case ex: Throwable => false }
  def getArgs( command: String ): Array[String] = command.replace(","," ").trim.split("\\s+")
  def getNumber( id: String, vtype: String, value: String ): GenericNumber = GenericNumber( if ( vtype.equals("indices") ) value.toInt else if(id.startsWith("tim")) value else value.toFloat)
}

//final class DomainSubsetCommand( name: String, val baseDomain: Domain, val axisIndex: Int = 0, val errMsg: String = "" ) extends CommandHandler(name,"Subsets an existing domain") {
//  val axes = Vector("lat","lon","lev","time")
//  lazy val axisId = axes.get(axisIndex)
//  lazy val domAxis: Option[Axis] = baseDomain.getAxis( axisId )
//
//  lazy val handler: CommandHandler = domAxis match {
//    case None => new CommandHandler()
//    case Some(axis) => axis.system match {
//      case sysVal if(sysVal.startsWith("ind")) => new CommandHandler(axis)
//      case sysVal if(sysVal.startsWith("val")) => new CommandHandler(axis)
//    }
//  }
//
//  def process(state: ShellState): ShellState = {
//    handlers.head.validate(state.history.last, state) match {
//      case None =>
//        val lastCommand = ( handlers.length == 1 )
//        val updatedState = if(!lastCommand) { state.updateHandler(new SequentialCommandHandler(name, description, handlers.tail, executor)) } else state
//        val processedState = updatedState.delegate(handlers.head)
//        if( lastCommand ) { executor(processedState).popHandler() } else { processedState }
//      case Some(errMsg) => state.updateHandler( new SequentialCommandHandler(name, description, handlers, executor, errMsg) )
//
//    }
//  }
//  def getPrompt( state: ShellState ) = {
//
//  }
//  override def toString = s"{$id:%s}".format( if(handlers.isEmpty) "" else handlers.head.id )
//}


