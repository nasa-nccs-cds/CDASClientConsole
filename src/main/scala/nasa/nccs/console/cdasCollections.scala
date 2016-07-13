package nasa.nccs.console
import java.nio.file.{Files, Paths}

import nasa.nccs.cdapi.kernels.ExecutionResults
import nasa.nccs.utilities.Loggable

class CdasCollections( requestManager: CDASClientRequestManager ) extends Loggable {
  val printer = new xml.PrettyPrinter(200, 3)
  private var _collections: Option[xml.Node] = requestCollections

//  def executeTask( taskRequest: TaskRequest, runArgs: Map[String,String] = Map.empty[String,String] ): ExecutionResults = requestManager.blockingExecute(taskRequest, runArgs)

  //  def generateAggregation(inputs: Vector[String], state: ShellState ): ShellState = {
//    if( inputs.forall( !_.isEmpty ) ) {
//      val uri: String = "collection:/" + inputs(0).trim.toLowerCase
//      val collection = Collections.addCollection( uri, inputs(1).trim() )
//      collection.createNCML()
//    }
//    state
//  }
  def updateCollections = if( !_collections.isDefined ) { _collections = requestCollections }
  def getCollections: Option[xml.Node] = { updateCollections; _collections }

  def getCollectionMap: Map[String,Collection] = {
    updateCollections
    val collections: Seq[Collection] = _collections match {
      case None => Seq.empty[Collection]
      case Some(collNodes) => collNodes.child.map( cNode => Collection(cNode) )
    }
    Map( collections.map( col => (col.id -> col ) ):_* )
  }
  def attr( node: xml.Node, att_name: String ): String = { node.attribute(att_name) match { case None => ""; case Some(x) => x.toString }}
  def attrOpt( node: xml.Node, att_name: String ): Option[String] = node.attribute(att_name).map( _.toString )

  def updateCollections( new_collections: Seq[xml.Node] ) = {
    _collections = _collections match {
      case None => Some(<collections>  {new_collections} </collections>)
      case Some(collections) => Some(<collections> { collections.child ++ new_collections } </collections>)
    }
  }

  def primaryDomain( state: ShellState ): Option[Domain] = state.getProp("domains") match {
    case Some(domids) =>
      if(domids.isEmpty) None else cdasDomainManager.getDomain(domids(0).text)
    case None => None
  }

  def aggregateDataset( inputs: Vector[String], state: ShellState ): ShellState = {
    println( " aggregateDataset, inputs = " + inputs.mkString(", ") )
    val id = inputs(0)
    val path = inputs(1)
    val uri: String = "collection:/" + id
    val collection = new Collection(id,uri,path)
    val results = localClientRequestManager.submitRequest( false, "util.agg", List.empty[Domain], List(collection), List.empty[Operation] )
    updateCollections( for( result <- results.child; collection <- result.child ) yield collection )
    state :+ Map( "results" -> results )
  }

    def cacheVariables( state: ShellState ): ShellState = {
      println( " cacheVariables, prop vals = " + state.props.values.map( _.mkString(",") ).mkString(" -- ") )
      val domid = state.props.get("domains") match {
        case None => "d0"
        case Some( dom_array ) => dom_array.text.replace(',',' ').split("\\s+")(0)
      }
      cdasDomainManager.getDomain( domid ) match {
        case None => state
        case Some( domain) =>
          val variables: Seq[Variable] = state.props.get("variables") match {
            case None => Seq.empty[Variable]
            case Some( varNodes ) => for((varNode,index)<-varNodes.child.zipWithIndex; variable=Variable(varNode,"v"+index,domid); if(!variable.varname.isEmpty)) yield variable
          }
          if( variables.isEmpty ) state
          else state :+ Map( "results" -> localClientRequestManager.submitRequest(false, "util.cache", List(domain), variables.toList, List.empty[Operation]) )
      }
    }

  def validCollectionId(exists: Boolean)(id: String): Option[String] = getCollections match {
    case Some(collNode) =>
      if ( collNode.child.find( node => ( attr(node,"id") == id ) ).isDefined ) {
        if (exists) None else Some(s"collection $id exists")
      } else {
        if (exists) Some(s"collection $id does not exist") else None
      }
    case None => None
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
      case Some( collections ) => {
        collections.child.flatMap( cnode => {
          cnode.attribute("id") match {
            case Some( coll_id ) => Some( cnode.text.replace(',',' ').split("\\s+").filter(!_.isEmpty).map( id => <variable id={id} collection={coll_id}/>.toString ) )
            case None => None
        }} ).foldLeft(Array.empty[String])(_ ++ _) }
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

  def removeCollections( cids: Array[String] ) = {
    val collections = getCollectionMap
    val cList = cids.flatMap( cid => collections.get(cid) ).toList
    val results = localClientRequestManager.submitRequest( false, "util.dcol", List.empty[Domain], cList, List.empty[Operation] )
    _collections = None
  }
  def removeFragment( fid: String ) = println( "Remove Fragment: " + fid)
  def printCollectionMetadata( collectionId: String  ): Unit = println( collectionId ) // printer.format( getCollection( collectionId ) ) )
  def printFragmentMetadata( fragDesc: String  ): Unit = println( fragDesc )

  def listCollectionsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[lc]ollections", "List collection metadata", (state) => requestCollectionsList, (cids:Array[String],state) => { cids.foreach( cid => printCollectionMetadata(cid)); state } )
  }
  def deleteCollectionsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[dc]ollections", "Delete specified collections", (state) => requestCollectionsList, (cids:Array[String],state) => { removeCollections( cids ); state } )
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
