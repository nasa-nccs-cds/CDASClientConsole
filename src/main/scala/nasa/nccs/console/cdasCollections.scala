package nasa.nccs.console
import java.nio.file.{Files, Paths}
import nasa.nccs.caching.{ FragmentPersistence, collectionDataCache }
import nasa.nccs.cdapi.cdm.Collection
import nasa.nccs.cds2.loaders.Collections
import nasa.nccs.cdapi.kernels.ExecutionResults
import nasa.nccs.cds2.engine.CDS2ExecutionManager
import nasa.nccs.esgf.process.{DomainContainer, TaskRequest}

class CdasCollections( executionManager: CDS2ExecutionManager ) {
  val printer = new xml.PrettyPrinter(200, 3)

  def generateAggregation(inputs: Vector[String], state: ShellState ): ShellState = {
    if( inputs.forall( !_.isEmpty ) ) {
      val uri: String = "collection:/" + inputs(0).trim.toLowerCase
      val collection = Collections.addCollection( uri, inputs(1).trim() )
      collection.createNCML()
    }
    state
  }

  def primaryDomain( state: ShellState ): Option[DomainContainer] = state.getProp("domains") match {
    case Some(domids) =>
      if(domids.isEmpty) None else cdasDomainManager.getDomain(domids(0))
    case None => None
  }

  def cacheVariables( state: ShellState ): ShellState = {
    println( " cacheVariables, prop vals = " + state.props.values.map( _.mkString(",") ).mkString(" -- ") )
    val domain: DomainContainer = primaryDomain(state).getOrElse( DomainContainer.empty("d0") )
    state.getProp("variables")  match {
      case Some(varRecs) =>
        val results: Array[ExecutionResults] = varRecs.map( (varRec) => {
          val vtoks = varRec.split(':').map(_.trim)
          val varname = vtoks(1)
          val uri: String = "collection:/" + vtoks(0)
          val dataInputs = Map(
            "domain" -> List( domain.toDataInput ),
            "variable" -> List(Map("uri" -> uri, "name" -> varname, "domain" -> domain.name ))
          )
          executeTask(TaskRequest("util.cache", dataInputs))
        })
        state :+ Map( "results" -> Array.empty[String] )
      case None =>
        println( "No selected variables" )
        state
    }

//        variables.map(cid => Collections.findCollection(cid) match {
//
//        }
////    state.getProp("collections") match {
////      case Some( cids ) => cids.foreach( (cid) => {
////
////      }
////    }
//    state.getProp()
//    printf( " Cache Variables: " + inputs.mkString(",") )
////    val uri: String = "collection:/" + inputs(0)
////    val varnames = inputs(1).toLowerCase.trim.replace(","," ").split("\\s+")
////    val results: Array[ExecutionResults] = for( varname <- varnames ) yield {
////      val dataInputs = Map("variable" -> List(Map("uri" -> uri, "name" -> varname, "domain" -> inputs(2))))
////      executeTask( TaskRequest("util.cache", dataInputs) )
//    }
    state
  }

  def validCollectionId( exists: Boolean )( id: String ): Option[String] = {
    if(Collections.getCollectionKeys.contains(id)) {
      if(exists) None else Some( s"collection $id exists")
    } else {
      if(exists) Some( s"collection $id does not exist" ) else None
    }
  }
  def validDirecory( dir: String ): Option[String] = { if( Files.exists(Paths.get(dir.trim))) None else Some( s"Directory '$dir' does not exist" ) }
  def validDomainId( domId: String ): Option[String] = { None }
  def validVariables( vars: String ): Option[String] = { None }

  def aggregateDatasetCommand: MultiStepCommandHandler = {
    new MultiStepCommandHandler("[ag]gregate", "Create collection by defining aggregated dataset",
      Vector( "Enter collection id >>", "Enter path to dataset directory >>" ),
      Vector( validCollectionId(false) _, validDirecory ),
      generateAggregation
    )
  }

  def cacheFragmentCommand: SequentialCommandHandler = {
    new SequentialCommandHandler("[ca]che", "Cache variable[s] from a collection",
      Vector( selectCollectionsCommand, cdasDomainManager.selectDomainCommand, selectVariablesCommand  ),
      cacheVariables
    )
  }

  def getVariableList(state: ShellState): Array[String] = {
    state.getProp("collections")  match {
      case Some( collections ) => {
        collections.map(cid => Collections.findCollection(cid) match {
          case Some(collection) => collection.vars.map(v => collection.id + ": " + v).toArray
          case None => Array.empty[String]
        }).foldLeft(Array.empty[String]) { _ ++ _ }
      }
      case None => println( "++++ UNDEF Collections! "); Array.empty[String]
    }
  }

  def listCollectionsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[lc]ollections", "List collection metadata", (state) => Collections.getCollectionKeys, (cids:Array[String],state) => { cids.foreach( cid => printCollectionMetadata( cid ) ); state } )
  }
  def deleteCollectionsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[dc]ollections", "Delete specified collections", (state) => Collections.getCollectionKeys, (cids:Array[String],state) => { cids.foreach( cid => Collections.removeCollection( cid ) ); state } )
  }
  def selectCollectionsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[sc]ollections", "Select collection(s)", (state) => Collections.getCollectionKeys, ( cids:Array[String], state ) => state :+ Map( "collections" -> cids )  )
  }
  def selectVariablesCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[sv]ariables", "Select variables from selected collection(s)", getVariableList, (cids:Array[String],state) => { state :+ Map( "variables" -> cids ) } )
  }

  def listFragmentsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[lf]ragments", "List cached data fragments", (state) =>FragmentPersistence.getFragmentList, (fragEntries:Array[String],state) => { fragEntries.foreach( fragDesc => collectionDataCache.printFragmentMetadata( FragmentPersistence.contractKey(fragDesc) ) ); state } )
  }
  def deleteFragmentsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[df]ragments", "Delete specified data fragments from the cache", (state) => FragmentPersistence.getFragmentList, (cids:Array[String],state) => { cids.foreach( cid => collectionDataCache.deleteFragment( cid ) ); state } )
  }

  def executeTask( taskRequest: TaskRequest, runArgs: Map[String,String] = Map.empty[String,String] ): ExecutionResults = executionManager.blockingExecute(taskRequest, runArgs)
  def printCollectionMetadata( collectionId: String  ): Unit = println( printer.format( Collections.getCollectionXml( collectionId ) ) )
}

object collectionsConsoleTest extends App {
  val cdasCollections = new CdasCollections( new CDS2ExecutionManager(Map.empty) )
  val handlers = Array(
    cdasCollections.aggregateDatasetCommand,
    cdasCollections.cacheFragmentCommand,
    cdasCollections.listCollectionsCommand,
    cdasCollections.deleteCollectionsCommand,
    cdasCollections.listFragmentsCommand,
    cdasCollections.deleteFragmentsCommand,
    cdasDomainManager.defineDomainHandler,
    new HistoryHandler( "[hi]story",  (value: String) => println( s"History Selection: $value" )  ),
    new HelpHandler( "[h]elp", "Command Help" )
  )
  val shell = new CommandShell( new SelectionCommandHandler( "base", "BaseHandler", "cdas> ", handlers ) )
  shell.run
}

// nasa.nccs.console.collectionsConsoleTest

object collectionsTest extends App {
  val cid = "merra_1/hourly/aggtest"
  Collections.removeCollection( cid )
}