package nasa.nccs.console
import java.nio.file.{Files, Paths}

import nasa.nccs.cdapi.kernels.ExecutionResults
import nasa.nccs.utilities.Loggable

class CdasCollections( requestManager: CDASClientRequestManager ) extends Loggable {
  val printer = new xml.PrettyPrinter(200, 3)
  private var collections: Option[xml.Node] = requestCollections

//  def executeTask( taskRequest: TaskRequest, runArgs: Map[String,String] = Map.empty[String,String] ): ExecutionResults = requestManager.blockingExecute(taskRequest, runArgs)

  //  def generateAggregation(inputs: Vector[String], state: ShellState ): ShellState = {
//    if( inputs.forall( !_.isEmpty ) ) {
//      val uri: String = "collection:/" + inputs(0).trim.toLowerCase
//      val collection = Collections.addCollection( uri, inputs(1).trim() )
//      collection.createNCML()
//    }
//    state
//  }
  def updateCollections = if( !collections.isDefined ) { collections = requestCollections }
  def getCollections: Option[xml.Node] = { updateCollections; collections }
  def attr( node: xml.Node, att_name: String ): String = { node.attribute(att_name) match { case None => ""; case Some(x) => x.toString }}
  def attrOpt( node: xml.Node, att_name: String ): Option[String] = node.attribute(att_name).map( _.toString )

  def primaryDomain( state: ShellState ): Option[Domain] = state.getProp("domains") match {
    case Some(domids) =>
      if(domids.isEmpty) None else cdasDomainManager.getDomain(domids(0).text)
    case None => None
  }

  def aggregateDataset( inputs: Vector[String], state: ShellState ): ShellState = {
    println( " aggregateDataset, prop vals = " + state.props.values.map( _.mkString(",") ).mkString(" -- ") )
//    val path = inputs(1)
//    val uri: String = "collection:/" + inputs(0)
//    val dataInputs = Map( "variable" -> List(Map("uri" -> uri, "path" -> path )) )
//    executeTask(TaskRequest("util.cache", dataInputs))
//    collections = None
    state :+ Map( "results" -> <empty/> )
  }

    def cacheVariables( state: ShellState ): ShellState = {
    println( " cacheVariables, prop vals = " + state.props.values.map( _.mkString(",") ).mkString(" -- ") )
//    val domain: Domain = primaryDomain(state).getOrElse( new Domain("d0") )
//    state.getProp("variables")  match {
//      case Some(varRecs) =>
//        val results: Array[ExecutionResults] = varRecs.map( (varRec) => {
//          val vtoks = varRec.split(':').map(_.trim)
//          val varname = vtoks(1)
//          val uri: String = "collection:/" + vtoks(0)
//          val dataInputs = Map(
//            "domain" -> List( domain ),
//            "variable" -> List( new Variable( uri,varname,domain.id) )
//          )
//          executeTask(TaskRequest("util.cache", dataInputs))
//        })
        state :+ Map( "results" -> <empty/> )
//      case None =>
//        println( "No selected variables" )
//        state
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
//    state
//  }

  def validCollectionId( exists: Boolean )( id: String ): Option[String] = {
    if( true /*Collections.getCollectionKeys.contains(id) */ ) {
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
      aggregateDataset
    )
  }

  def cacheFragmentCommand: SequentialCommandHandler = {
    new SequentialCommandHandler("[ca]che", "Cache variable[s] from a collection",
      Vector( selectCollectionsCommand, cdasDomainManager.selectDomainCommand, selectVariablesCommand  ),
      cacheVariables
    )
  }

  def requestVariableList(state: ShellState): Array[String] = {
    state.getProp("collections")  match {
      case Some( collections ) =>
        collections.child.flatMap( cnode => cnode.attribute("id") match {
          case Some( id_node ) =>
            Some( xmlToString(requestManager.requestCapabilities("variables!" + id_node.toString ) ) )
          case None => None
        } ).toArray
      case None => println( "++++ UNDEF Collections! "); Array.empty[String]
    }
  }

  def xmlToString( node: xml.Node, indent: String=" " ): String =
    Array( indent + node.label +  node.attributes.mkString("(",",","): ") + node.text, node.child.filterNot(_.label.startsWith("#")).map( cnode => xmlToString(cnode,indent+"\t") ) ).mkString("\n")

  def requestCollections: Option[xml.Node] = {
    requestManager.requestCapabilities("collections") match {
      case response if( response.label == "error" ) => logger.error( response.text ); None
      case response => Some(response)
    }
  }

  def requestCollectionsList(): Array[String] = getCollections match {
    case None => Array.empty[String]
    case Some(response) => response.child.filterNot(_.label.startsWith("#")).map(cNode => cNode.toString.replace('\n',' ')).toArray
  }

  def requestFragmentList(): Array[String] = {
    val response = requestManager.requestCapabilities("fragment")
    if( response.label == "error" ) { logger.error( response.text ); Array.empty[String] }
    else response.child.filterNot(_.label.startsWith("#")).map(cNode => cNode.toString.replace('\n',' ')).toArray
  }

  def removeCollection( cid: String ) = println( "Remove Collection: " + cid)
  def removeFragment( fid: String ) = println( "Remove Fragment: " + fid)
  def printCollectionMetadata( collectionId: String  ): Unit = println( collectionId ) // printer.format( getCollection( collectionId ) ) )
  def printFragmentMetadata( fragDesc: String  ): Unit = println( fragDesc )

  def listCollectionsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[lc]ollections", "List collection metadata", (state) => requestCollectionsList, (cids:Array[String],state) => { cids.foreach( cid => printCollectionMetadata(cid)); state } )
  }
  def deleteCollectionsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[dc]ollections", "Delete specified collections", (state) => requestCollectionsList, (cids:Array[String],state) => { cids.foreach( cid => removeCollection( cid ) ); state } )
  }
  def selectCollectionsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[sc]ollections", "Select collection(s)", (state) => requestCollectionsList, ( cids:Array[String], state ) => state :+ Map( "collections" -> <collections> { cids.map( cid => xml.XML.loadString(cid)) } </collections> )  )
  }
  def selectVariablesCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[sv]ariables", "Select variables from selected collection(s)", requestVariableList, (cids:Array[String],state) => { state :+ Map( "variables" -> <variables> { cids.map( cid => xml.XML.loadString(cid)) } </variables> ) } )
  }

  def listFragmentsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[lf]ragments", "List cached data fragments", (state) =>requestFragmentList, (fragEntries:Array[String],state) => { fragEntries.foreach( fragDesc => printFragmentMetadata( fragDesc ) ); state } )
  }
  def deleteFragmentsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[df]ragments", "Delete specified data fragments from the cache", (state) => requestFragmentList, (fids:Array[String],state) => { fids.foreach( fid => removeFragment( fid )  ); state } )
  }

}

object collectionsConsoleTest extends App {
  val cdasCollections = new CdasCollections( new CDASClientRequestManager( ) )
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
