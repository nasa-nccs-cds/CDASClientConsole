package nasa.nccs.console
import java.nio.file.{Files, Paths}
import nasa.nccs.utilities.Loggable

class CdasControlCenter( requestManager: CDASClientRequestManager ) extends Loggable {
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

  def getCollectionNodeMap: Map[String,xml.Node] = {
    updateCollections
    val cNodes: Seq[xml.Node] = _collections match {
      case None => Seq.empty[xml.Node]
      case Some(collNodes) => collNodes.child
    }
    Map( cNodes.map( cNode => ( attr(cNode,"id") -> cNode ) ):_* )
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

  def aggregateDataset( multiple: Boolean )( inputs: Vector[String], state: ShellState ): ShellState = {
    println( " aggregateDataset, inputs = " + inputs.mkString(", ") )
    val id = inputs(0)
    val path = inputs(1)
    val uri: String = "collection:/" + id
    val collections = List(new Collection(id,uri,path))
    val ident = if( multiple ) "util.magg" else  "util.agg"
    val results = localClientRequestManager.submitRequest( true, ident, List.empty[Domain], collections, List.empty[Operation] )
    _collections = None
    state :+ Map( "results" -> results )
  }

  def cacheVariables(state: ShellState): ShellState = {
    println(" cacheVariables, prop vals = " + state.props.values.map(_.mkString(",")).mkString(" -- "))
    val domid = state.props.get("domains") match {
      case None => "d0"
      case Some(dom_array) => dom_array.text.replace(',', ' ').split("\\s+")(0)
    }
    cdasDomainManager.getDomain(domid) match {
      case None => state
      case Some(domain) =>
        val variables: Seq[Variable] = state.props.get("variables") match {
          case None => Seq.empty[Variable]
          case Some(varNodes) => for ((varNode, index) <- varNodes.child.zipWithIndex; variable = Variable(varNode, "v" + index, domid); if (!variable.varname.isEmpty)) yield variable
        }
        if (variables.isEmpty) state
        else state :+ Map("results" -> localClientRequestManager.submitRequest(true, "util.cache", List(domain), variables.toList, List.empty[Operation]))
    }
  }

  def getResult( state: ShellState ): ShellState = {
    state.props.get("access") match {
      case None => state
      case Some( access_method ) =>
        state.props.get("result") match {
          case None => state
          case Some( resultsElem ) =>
            val vars: List[Variable] = resultsElem.child.filter(_.label=="result").map( rnode => attr (rnode, "id") ).map( rid => new Variable (rid) ).toList
            println ("Submitting request for result, id = util.gres:" + access_method.text + ", vars = " + vars.mkString(",") )
            val result = localClientRequestManager.submitRequest (false, "util.gres:" + access_method.text, List.empty[Domain], vars, List.empty[Operation] )
            state :+ Map ("result" -> result)
        }
    }
  }

  def removeResults( resultNodes: Array[String] ) = {
    val rids = resultNodes.map( elemStr => xml.XML.loadString(elemStr) ).filter(_.label=="result").map( n => attr(n,"id") )
    println( "Remove results: " + rids.mkString(",") )
    val variables: Array[WpsData] = for(rid <- rids) yield new Variable(rid,"",rid,"")  //  def toFragKey =  "%s|%s|%s|%s".format( varname, collectionUrl, origin.mkString(","), shape.mkString(","))  TODO: Finish this :
    val results = localClientRequestManager.submitRequest( false, "util.dres", List.empty[Domain], variables.toList, List.empty[Operation] )
    _collections = None
  }

  def printVariableMetadata( state: ShellState ): ShellState = {
    state.props.get("variables") match {
      case None => Unit
      case Some( varNodes ) =>
        val varIdList = varNodes.child.filterNot( varNode => attr(varNode,"id").isEmpty ).map( varNode => attr(varNode,"id").trim + "!" + attr(varNode,"collection").trim ).mkString(",")
        println( printer.format(  requestMetadata( "variables", varIdList.trim ) ) )
    }
    state
  }
  def exeOperation( state: ShellState ): ShellState = {
    println( " exeOperation, prop vals = " + state.props.values.map( _.mkString(",") ).mkString(" -- ") )
    val domid = state.props.get("domains") match {
      case None => "d0"
      case Some( dom_array ) => dom_array.text.replace(',',' ').split("\\s+")(0)
    }
    cdasDomainManager.getDomain( domid ) match {
      case None => state
      case Some( domain) =>
        val fragments: Seq[Fragment] = state.props.get("fragments") match {
          case None => Seq.empty[Fragment]
          case Some( fragNodes ) => for((fragNode,index)<-fragNodes.child.zipWithIndex; fragment=Fragment(fragNode,"v"+index,domid); if !fragment.varname.isEmpty) yield fragment
        }
        println( " -->> fragments = " + fragments.map(_.toString).mkString(",") )
        if( fragments.isEmpty ) state
        else {
          val operations: Seq[(String,String)] = state.props.get("operations") match {
            case None => Seq.empty[(String,String)]
            case Some( opNodes ) => for((opNode,index)<-opNodes.child.zipWithIndex; mod=attr(opNode,"module"); mname=attr(opNode,"name"); if !mod.isEmpty; if !mname.isEmpty; operation=(mod, mod + "." + mname) ) yield operation
          }
          println( " -->> operations = " + operations.toString )
          if( operations.isEmpty ) state
          else {
            val axes: String = state.props.get("axes") match {
              case None => ""
              case Some(axesNode) => axesNode.text.replaceAll("[^xyztXYZT]", "")
            }
            val results = <results> {
              for( (opSpec, opIndex) <- operations.zipWithIndex ) yield {
                val input_uids: Array[String] = fragments.map(_.uid).toArray
                val op = new Operation(opSpec._1, opSpec._2, input_uids, Map("axes" -> axes), "r" + opIndex)
                localClientRequestManager.submitRequest(true, "CDS.workflow", List(domain), fragments.toList, List(op))
              }
              } </results>
            println( results.toString() )
            state :+ Map("results" -> results )
          }
        }
    }
  }

  def validCollectionId(exists: Boolean)(id: String): Option[String] = getCollections match {
    case Some(collNode) =>
      if( id.isEmpty ) { None }
      else if ( collNode.child.find( node => ( attr(node,"id") == id ) ).isDefined ) {
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
      aggregateDataset(false)
    )
  }

  def clearCache( state: ShellState ): ShellState = {
    val ident = "util.clearCache"
    val results = localClientRequestManager.submitRequest( true, ident, List.empty[Domain], List.empty[Collection], List.empty[Operation] )
    state :+ Map( "results" -> results )
  }

  def clearCacheCommand: CommandHandler = {
    new CommandHandler("[cc]ache", "Delete all data fragments in cache") {
      def process(state: ShellState): ShellState = {
        if( state.history.last.toLowerCase.startsWith("y") ) { clearCache(state); }
        state.popHandler()
      }
      def getPrompt(state: ShellState): String = "Clear all cached data fragments? (y/n) >>"
    }
  }

  def aggregateMultipleDatasetsCommand: MultiStepCommandHandler = {
    new MultiStepCommandHandler("[mag]gregate", "Create collections by defining multiple aggregated datasets",
      Vector( "Enter base collection id >>", "Enter path to directory containing datasets>>" ),
      Vector( validCollectionId(false) _, validDirecory ),
      aggregateDataset(true)
    )
  }

  def getResultCommand: SequentialCommandHandler = {
    new SequentialCommandHandler("[gr]esult", "Get result of analytics operation",
      Vector( selectResultCommand,
        new ListSelectionCommandHandler("[sa]ccess", "Select access method", (state) => Array( "Display as xml", "Save to NetCDF file"),
          ( methods, state ) => state :+ Map( "access" ->
            <access> {
              methods.head match {
                case x if x.startsWith("Disp") => "xml";
                case x if x.startsWith("Save") => "netcdf";
                case x => ""
              }} </access>)
        )
      ),
      getResult
    )
  }

  def cacheFragmentCommand: SequentialCommandHandler = {
    new SequentialCommandHandler("[ca]che", "Cache variable[s] from a collection",
      Vector( selectCollectionsCommand, cdasDomainManager.selectDomainCommand(), selectVariablesCommand  ),
      cacheVariables
    )
  }


  def exeOperationCommand: SequentialCommandHandler = {
    new SequentialCommandHandler("[ex]ecute", "Execute analytics operation",
      Vector( selectFragmentsCommand, cdasDomainManager.selectDomainCommand("Select operation domain (will be intersected with fragment domain)"), selectOperationsCommand, selectAxesCommand  ),
      exeOperation
    )
  }

  def listVariablesCommand: SequentialCommandHandler = {
    new SequentialCommandHandler("[lv]ariables", "List Variables",
      Vector( selectCollectionsCommand, selectVariablesCommand ),
      printVariableMetadata
    )
  }

  def get_array_element( array: Array[String], index: Int, default: String="" ): String =
    try{ clean(array(index)) } catch { case ex: ArrayIndexOutOfBoundsException => default }

  def requestVariableList(state: ShellState): Array[String] = {
    val collMap = getCollectionNodeMap
    state.getProp("collections")  match {
      case Some( collections ) => {
        collections.child.flatMap( cidnode => {
          attrOpt(cidnode,"id") match {
            case Some( cid ) => collMap.get( cid ) match {
              case Some( cnode ) => Some( cnode.text.split(';').filter(!_.isEmpty).map( varStr => {
                  val vs = varStr.split(':');
                  <variable id={get_array_element(vs,0)} dims={get_array_element(vs,1)} units={get_array_element(vs,3)} desc={get_array_element(vs,2)} collection={clean(cid)}/>.toString
              } )  )
              case None => None
              }
            case None => None
        }} ).foldLeft(Array.empty[String])(_ ++ _) }
      case None => println( "++++ UNDEF Collections! "); Array.empty[String]
    }
  }

  def clean( value: String ): String = value.replace("\n","")

  def xmlToString( node: xml.Node, indent: String=" " ): String =
    Array( indent + node.label +  node.attributes.mkString("(",",","): ") + node.text, node.child.filterNot(_.label.startsWith("#")).map( cnode => xmlToString(cnode,indent+"\t") ) ).mkString("\n")

  def requestCollections: Option[xml.Node] = {
    requestManager.requestCapabilities("collections") match {
      case response if( response.label == "error" ) => logger.error( response.text ); None
      case response => Some(response)
    }
  }
  def requestMetadata( mdtype: String, idList: String ): xml.Node = {
    requestManager.requestCapabilities( mdtype + ":" + idList ) match {
      case response if( response.label == "error" ) => <error message={response.text}></error>
      case response => response
    }
  }

  def requestCollectionsList(state: ShellState): Array[String] = getCollections match {
    case None => Array.empty[String]
    case Some(response) => response.child.filterNot(_.label.startsWith("#")).map( cNode => attr(cNode,"id") + ": " + attr(cNode,"title") ).sortWith((n0,n1)=>(n0<n1)).toArray
  }

  def listCapabilities( capability: String ): Array[String] = {
    val response = requestManager.requestCapabilities(capability)
    if( response.label == "error" ) { logger.error( response.text ); Array.empty[String] }
    else capability match  {
      case cap: String if cap.toLowerCase.startsWith("op") =>
//        val nodes: xml.NodeSeq = response \\ "Process" \ "Identifier"
        val nodes: xml.NodeSeq = response \\ "kernel"
        nodes.map( (node: xml.Node)  => node.toString.replace('\n',' ') ).toArray
      case cap: String if cap.toLowerCase.startsWith("re") =>
        logger.info( "  >>>>>-----> Response: " + response.mkString(",") )
//        val nodes: xml.NodeSeq = (response \\ "ProcessOutputs" \\ "Output" \\ "Reference").flatMap( _ \ "@href")
        val nodes: xml.NodeSeq = ( response \\ "result" )
        nodes.map( (node: xml.Node)  => node.toString.replace('\n',' ') ).toArray
      case _ =>
        response.child.filterNot(_.label.startsWith("#")).map( node => node.toString.replace('\n',' ') ).toArray
    }
  }

  def requestOperationsList( state: ShellState ): Array[String] = listCapabilities("operation")
  def requestFragmentList( state: ShellState ): Array[String]  = listCapabilities("fragment")
  def requestResultList( state: ShellState ): Array[String]  = listCapabilities("result")
  def requestJobList( state: ShellState ): Array[String]  = listCapabilities("job")

  def removeCollections( selectedCols: Array[String] ) = {
    val collectionMap = getCollectionMap
    val cids = selectedCols.map( colId => colId.split(":").head )
    println( "Collection keys -> " + collectionMap.keys.mkString(",") )
    val cList = cids.flatMap( cid => collectionMap.get( cid ) ).toList
    println( "  ------> RemoveCollection( " + cids(0) + " ) -> " + cList.mkString(",") )
    val results = localClientRequestManager.submitRequest( false, "util.dcol", List.empty[Domain], cList, List.empty[Operation] )
    _collections = None
  }

  def removeFragments( fragXmls: Array[String] ) = {
    println( "Remove Fragments: " + fragXmls.mkString(",") )
    val frags = fragXmls.map( fragXml => xml.XML.loadString(fragXml) )
    val variables: Array[WpsData] = for((frag, index) <- frags.zipWithIndex) yield
      new Variable( "v"+index, "collection:/"+attr(frag, "coll"),attr(frag,"variable"), attr(frag,"origin")+"|"+attr(frag,"shape") )
    val results = localClientRequestManager.submitRequest( false, "util.dfrag", List.empty[Domain], variables.toList, List.empty[Operation] )
    _collections = None
  }

  def removeFragment( fid: String ) = println( "Remove Fragment: " + fid)

  def extractAttribute( input: String, attrId: String = "id" ): String = {
    input.trim match {
      case xmlStrVal if xmlStrVal.startsWith("<") => attr( xml.XML.loadString( xmlStrVal ), attrId )
      case attrStrVal => attrStrVal
    }
  }

  def printCollectionMetadata( collectionStr: String  ): Unit =
    println( printer.format( requestMetadata( "collections", extractAttribute(collectionStr) ) ) )

  def printFragmentMetadata( fragDesc: String  ): Unit = println( fragDesc )

  def listCollectionsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[lc]ollections", "List collection metadata", requestCollectionsList,
      (collections,state) => { collections.foreach( collID => printCollectionMetadata(collID.split(":").head)); state } )
  }
  def deleteCollectionsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[dc]ollections", "Delete specified collections", requestCollectionsList,
      (collections,state) => { removeCollections( collections ); state } )
  }
  def selectCollectionsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[sc]ollections", "Select collection(s)", requestCollectionsList,
      ( collections, state ) => state :+ Map( "collections" -> <collections> { collections.map( collID => <collection id={collID.split(":").head}/> ) } </collections> )  )
  }
  def selectOperationsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[so]peration", "Select operation(s)", requestOperationsList,
      ( operations, state ) => state :+ Map( "operations" -> <operations> { operations.map( operation => xml.XML.loadString(operation)) } </operations> )  )
  }
  def selectAxesCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[sa]xes", "Select axes", (state) => Array("x","y","z","t"),
      ( axes, state ) => state :+ Map( "axes" -> <axes> { axes.mkString(",") } </axes> )  )
  }
  def listOperationsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[lo]peration", "List operation(s)", requestOperationsList,
      (operations, state) => state :+ Map( "operations" -> <operations> { operations.map( operation => xml.XML.loadString(operation)) } </operations> )  )
  }
  def selectVariablesCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[sv]ariables", "Select variables from selected collection(s)", requestVariableList,
      (cids, state) => { state :+ Map( "variables" -> <variables> { cids.map( cid => xml.XML.loadString(cid)) } </variables> ) } )
  }
  def selectFragmentsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[sf]ragments", "Select data fragments from cache", requestFragmentList,
      (frags, state) => { state :+ Map( "fragments" -> <fragments> { frags.map( frag => xml.XML.loadString(frag)) } </fragments> ) } )
  }
  def listFragmentsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[lf]ragments", "List cached data fragments",
      requestFragmentList, (fragEntries, state) => { fragEntries.foreach( fragDesc => printFragmentMetadata( fragDesc ) ); state } )
  }
  def deleteFragmentsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[df]ragments", "Delete specified data fragments from the cache",
      requestFragmentList, (fragments,state) => { removeFragments( fragments ); ; state } )
  }
  def listResultsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[lr]esults", "List execution results", requestResultList,
      (resultIds, state) => printStateProp("result")(getResult(setResultAccessState("xml", resultIds, state))))
  }
  def listJobsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[lj]obs", "List executing task requests",
      requestJobList, (jobIds, state) => { jobIds.foreach( jobId => println( jobId )); state } )
  }
  def selectResultCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[sr]esult", "Select execution result",
      requestResultList, (resultIds, state) => state :+ Map( "result" -> <results> { resultIds.map( elemStr => xml.XML.loadString(elemStr) ) } </results> ) )
  }
  def getResultFileCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[gr]esult", "Get result as NetCDF file", requestResultList,
      (resultIds, state) =>  printStateProp("result")( getResult( setResultAccessState("netcdf",resultIds, state)) ) )
  }
  def deleteResultsCommand: ListSelectionCommandHandler = {
    new ListSelectionCommandHandler("[dr]esults", "Delete specified execution results",
      requestResultList, (resultIds, state) => { removeResults( resultIds ); state } )
  }
  def setResultAccessState( rtype: String, resultIds: Array[String], state: ShellState ): ShellState  =
    state :+ Map( "access" -> <access> { rtype } </access>, "result" -> <results> { resultIds.map( elemStr => xml.XML.loadString(elemStr) ) } </results> )

  def printStateProp( elemId: String)( state: ShellState ): ShellState = {
    state.getProp(elemId) match {
      case Some( elem ) => println( printer.format(elem) )
      case None => println( "Missing state property: " + elemId )
    }
    state
  }
}

object cdasShellManager extends App {
  val cdasControl = new CdasControlCenter( new CDASClientRequestManager( ) )
  val handlers = Array(
    cdasControl.aggregateDatasetCommand,
    cdasControl.clearCacheCommand,
    cdasControl.aggregateMultipleDatasetsCommand,
    cdasControl.cacheFragmentCommand,
    cdasControl.listCollectionsCommand,
    cdasControl.deleteCollectionsCommand,
    cdasControl.listFragmentsCommand,
    cdasControl.deleteFragmentsCommand,
    cdasControl.listOperationsCommand,
    cdasControl.listResultsCommand,
    cdasControl.listJobsCommand,
    cdasControl.deleteResultsCommand,
    cdasControl.getResultFileCommand,
    cdasDomainManager.defineDomainHandler,
    cdasControl.exeOperationCommand,
    cdasControl.listVariablesCommand,
    new HelpHandler( "[h]elp", "Command Help" )
  )
  val shell = new CommandShell( new SelectionCommandHandler( "base", "BaseHandler", "cdas> ", handlers ) )
  shell.run
}
