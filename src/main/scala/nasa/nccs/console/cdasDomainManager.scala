package nasa.nccs.console

import java.util
import ucar.nc2.time.CalendarDate
import com.googlecode.concurrentlinkedhashmap.ConcurrentLinkedHashMap
import nasa.nccs.cds2.loaders.Collections
import nasa.nccs.esgf.process.DomainAxis.Type._
import nasa.nccs.esgf.process.{DomainAxis, DomainContainer}
import nasa.nccs.esgf.utilities.numbers.GenericNumber
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

object cdasDomainManager {
  import DomainAxis.Type._

  private val domainMap = new ConcurrentLinkedHashMap.Builder[String, DomainContainer ].initialCapacity(100).maximumWeightedCapacity(10000).build()
  domainMap.put( "d0", DomainContainer.empty( "d0" ) )
  def getDomain(domId: String ): Option[DomainContainer] = Option(domainMap.get(domId))
  def putDomain(domId: String, domain: DomainContainer ) = domainMap.put(domId, domain)

  def createDomain( inputs: Vector[String] ) = {
    val domainId = "d" + domainMap.size
    val domainAxes = inputs zip List( X, Y, Z, T ) map { case ( input: String, atype ) => createDomainAxis( atype, input ) }
    val domain = new DomainContainer( domainId, domainAxes.flatten.toList )
    putDomain( domainId, domain )
    println( "Created Domain %s: %s".format( domainId, domain.toString ) )
  }

  def getDomains: IndexedSeq[DomainContainer] = domainMap.values.toIndexedSeq

  def getDomainSelectionList( state: ShellState  ): Array[String] = {
    getDomains.map( (dc: DomainContainer) => dc.name + ": " + dc.axes.mkString("{",", ","}") ).toArray
  }

  def createDomainAxis( atype: DomainAxis.Type.Value, input: String ): Option[DomainAxis] = {
    if(input.isEmpty) None else {
      val args = getArgs(input)
      val vtype = args(0) match {
        case x if x.startsWith("i") => "indices"
        case x if x.startsWith("v") => "values"
        case x => throw new Exception("Unrecognized value/index specification: " + x)
      }
      val b0 = getNumber( atype, vtype, args(1) )
      val b1 = if (args.length < 3) b0 else getNumber( atype, vtype, args(2) )
      Some(new DomainAxis( atype, b0, b1, vtype) )
    }
  }

  def domainAxisValidator( atype: DomainAxis.Type.Value )( input: String ): Option[String] = {
    if(input.isEmpty) return  None
    val args = getArgs(input)
    if( args.length < 2 ) return Some("Missing input")
    if( !(args(0).startsWith("i") || args(0).startsWith("v")) ) return Some("Unrecognized value/index specification: '%s'".format(args(0)) )
    args(0) match {
      case x if x.startsWith("i") =>
        if( !validInt( args(1) ) ) return Some("Invalid bounds index: '%s'".format(args(1)) )
        if( (args.length > 2) && !validInt( args(2) ) ) return Some("Invalid bounds index: '%s'".format(args(2)) )
      case x if x.startsWith("v") =>
        if( atype == T ) {
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
        Vector(X,Y,Z,T).map(domainAxisValidator(_) _ ), (vals,state) => { createDomain( vals ); state } )

  def selectDomainCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[sd]omain", "Select domain(s)", getDomainSelectionList, ( cids:Array[String], state ) => { state :+ Map( "domains" -> cids.map( _.split(':')(0).trim ) ) } )
  }

  def validFloat( input: String ): Boolean = try { input.toFloat; true } catch { case ex: Throwable => false }
  def validInt( input: String ): Boolean = try { input.toInt; true } catch { case ex: Throwable => false }
  def validTime( input: String ): Boolean = try { CalendarDate.parseISOformat(null,input); true } catch { case ex: Throwable => false }
  def getArgs( command: String ): Array[String] = command.replace(","," ").trim.split("\\s+")
  def getNumber( atype: DomainAxis.Type.Value, vtype: String, value: String ): GenericNumber = GenericNumber( if ( vtype.equals("indices") ) value.toInt else if(atype == T) value else value.toFloat)
}

/*
object CommandExecutables {
  private val domainMap = new ConcurrentLinkedHashMap.Builder[String, Map[String,String]].initialCapacity(100).maximumWeightedCapacity(10000).build()
  private var currentDomain: String = "d0"
  domainMap.put( currentDomain, Map.empty[String,String] )

  def getDomain( domId: String = currentDomain ): Option[Map[String,String]] = Option(domainMap.get(domId))
  def putDomain( domId: String, domain: Map[String,String] ) = domainMap.put( domId, domain )

  private val values: List[CommandExecutable] = List(

    new CommandExecutable("[t]est", "Test exe", "") {
      def execute(command: String, callIndex: Int ): Boolean = {
        println("---> Executing command: " + command + ", call index = " + callIndex );
        false
      }
    },
    new CommandExecutable("[he]lp", "Lists available commands", "") {
      def execute(command: String, callIndex: Int ): Boolean = {
        println( "------ Commands --------" )
        for( cmdExe <- CommandExecutables.getCommandsAlpha ) {
          println( "  --> %s %s: %s ".format( cmdExe.name, cmdExe.args, cmdExe.description) )
        }
        false
      }
    },
    new CommandExecutable("[ca]che", "Cache variable from NetCDF dataset", "<collection_id> <variable> <domain> <dataset_path>") {
      def execute(command: String, callIndex: Int ): Boolean = {
        println( "------ Commands --------" )
        for( cmdExe <- CommandExecutables.getCommandsAlpha ) {
          println( "  --> %s %s: %s ".format( cmdExe.name, cmdExe.args, cmdExe.description) )
        }
        false
      }
    },
    new CommandExecutable("[co]llections", "Collection Operations: [l]ist, [d]efine, [s]electCurrent", "<operation:(l/d/s)>") {
      def execute(command: String, callIndex: Int ): Boolean = {
        interactionHandler match {
          case None =>
            val cmdArgs = command.split(' ')
            val operation = if (cmdArgs.length > 1) {
              cmdArgs(1)
            } else {
              "list"
            }.toLowerCase.head
            operation match {
              case 'l' =>
                println("------ Collections --------")
                for (collId <- Collections.idSet) Collections.findCollection(collId) match {
                  case Some(collection) => println("  --> id: %s vars: (%s), url: %s, path: %s ".format(collId, collection.url, collection.vars.mkString(","), collection.path))
                  case None => Unit
                }
                false
              case 'd' =>
                interactionHandler = Some( new CommandInterationHandler(
                  List( "Collection id:", "Dataset url or file path:" ), ( responses: List[String] ) => Collections.addCollection( responses(0), responses(1) ) ) )
                true
              //              case 's' =>
              //                interactionHandler = Some( new CommandInterationHandler(
              //                  List( Collections.indexedCollectionList ), ( responses: List[String] ) => Collections.addCollection( responses(0), responses(1) ) ) )
              //                true
              case x =>
                println("Unrecognized <operation> argument: " + operation)
                false
            }
          case Some( handler ) =>
            handler.process( command )
        }
      }
    }
  )
  val getCommands: List[CommandExecutable] = values.sortWith(_.len > _.len)
  val getCommandsAlpha: List[CommandExecutable] = values.sortWith( _.key < _.key )
}

*/
