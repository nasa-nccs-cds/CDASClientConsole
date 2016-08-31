package nasa.nccs.console
import java.io.{Console, PrintWriter, StringWriter}
import scala.annotation.tailrec

object ParseHelp {
  def isInt( value: String ): Boolean = try { value.toInt; true } catch { case t: Throwable => false }
}

abstract class CommandHandler( val name: String, val description: String ) {
  val id: String = extractMatchId
  def process( state: ShellState ): ShellState
  def getPrompt( state: ShellState ): String
  def matches( command: String ): Boolean = command.toLowerCase.startsWith(id)
  def validate( command: String, state: ShellState ): Option[String] = None
  def extractMatchId = name.toLowerCase.split('[').last.split(']').head
  def help: String = { s" * '$name': $description"}
  def getArgs( command: String ) = command.trim.replace(","," ").split("\\s+")
  def assertBounds( ival: Int, b0: Int, b1: Int ): Unit = if( (ival<b0) || (ival>b1) ) throw new Exception( "Index out of bounds" )
  override def toString = s"{$id}"
}

class ShellState( val handlerStack: Vector[CommandHandler], val history: Vector[String]= Vector.empty[String], val props: Map[String,xml.Node]=Map.empty[String,xml.Node] ) {
  def pushHandler( handler: CommandHandler ): ShellState = {
//    println( " push <<<< topHandler: %s".format( handlerStack.map(_.toString).mkString("{ ",", "," }") ) )
    val handlers: Vector[CommandHandler] = handlerStack :+ handler
//    println( " push >>>> topHandler: %s".format( handlers.map(_.toString).mkString("{ ",", "," }") ) )
    new ShellState( handlers, history, props )
  }
  def updateHandler( handler: CommandHandler ): ShellState = {
//    println( " update <<<< topHandler: %s".format( handlerStack.map(_.toString).mkString("{ ",", "," }") ) )
    val handlers: Vector[CommandHandler] = handlerStack.dropRight(1) :+ handler
//    println( " update >>>> topHandler: %s".format( handlers.map(_.toString).mkString("{ ",", "," }") ) )
    new ShellState( handlers, history, props )
  }
  def popHandler( preserveBase: Boolean = true ): ShellState = {
//    println( " pop <<<< topHandler: %s".format( handlerStack.map(_.toString).mkString("{ ",", "," }") ) )
    val handlers: Vector[CommandHandler] = if( (handlerStack.length > 1) || !preserveBase ) handlerStack.dropRight(1) else handlerStack
//    println( " pop>>>> topHandler: %s".format( handlers.map(_.toString).mkString("{ ",", "," }") ) )
    new ShellState( handlers, history, props )
  }
  def handleCommand( command: String ): ShellState = {
    val passedProps = if( handlerStack.size > 1 ) props else Map.empty[String,xml.Node]
    processResults(handlerStack.last.process(new ShellState(handlerStack, history :+ command.trim, passedProps)))
  }

  def processResults(state: ShellState): ShellState = state.getProp("results") match {
    case Some(rnode) => println(rnode.toString); state :- "results"
    case None => state
  }
  def delegate( handler: CommandHandler ): ShellState = {  handler.process( pushHandler(handler) )  }
  def getPrompt = { handlerStack.last.getPrompt( this ) }
  def getTopCommand = history.last
  def popTopCommand = new ShellState( handlerStack, history.dropRight(1), props )
  def getTopHandler = handlerStack.last
  def getStackStr = handlerStack.map( _.id ).mkString( "( ",", "," )")
  def sameHandler( handler: ShellState ): Boolean = { getTopHandler == handler.getTopHandler }
  def getProp( name: String ): Option[xml.Node] = props.get( name )
  def :+ ( new_props: Map[String,xml.Node] ): ShellState = new ShellState( handlerStack, history, props ++ new_props )
  def :- ( prop_key: String ): ShellState = new ShellState( handlerStack, history, props - prop_key )
}

class CommandShell( val baseHandler: CommandHandler) {
  protected val console = System.console()

  @tailrec
  private def execute( state: ShellState ): Unit = {
    assert( console != null, "Can't get a console on this system!" )
    val command: String = console.readLine( state.getPrompt )
    if( !quitRequested(command) ) execute( state.handleCommand( command ) )
  }

  protected def quitRequested( command: String ): Boolean = command.toLowerCase().startsWith("quit")
  def run = execute( new ShellState( Vector(baseHandler) ) )
}

final class MultiStepCommandHandler( name: String, description: String, val prompts: Vector[String], val validators: Vector[(String)=>Option[String]], val executor: (Vector[String],ShellState) => ShellState, val length: Int = -1  )
  extends CommandHandler(name,description) {
  val _length = if( length > 0 ) length else prompts.length

  def process( state: ShellState ): ShellState = {
    val command = state.getTopCommand
    validators.head( command ) match {
      case None =>
        if ( prompts.length > 1 ) state.updateHandler( new MultiStepCommandHandler(name, description, prompts.tail, validators.tail, executor, _length ) )
        else                      executor( state.history.takeRight(_length), state ).popHandler()
      case Some( errorMsg ) =>
        val new_prompts = s"Input error: $errorMsg, please try again: " +: prompts.drop(1)
        state.popTopCommand.updateHandler( new MultiStepCommandHandler(name, description, new_prompts, validators, executor, _length ) )
    }
  }
  def getPrompt( state: ShellState ) = prompts.head
}

final class SequentialCommandHandler( name: String, description: String, val handlers: Vector[CommandHandler], val executor: (ShellState) => ShellState, val errMsg: String = ""  )
  extends CommandHandler(name,description) {

  def process(state: ShellState): ShellState = {
    handlers.head.validate(state.history.last, state) match {
      case None =>
        val lastCommand = ( handlers.length == 1 )
        val updatedState = if(!lastCommand) { state.updateHandler(new SequentialCommandHandler(name, description, handlers.tail, executor)) } else state
        val processedState = updatedState.delegate(handlers.head)
        if( lastCommand ) { executor(processedState).popHandler() } else { processedState }
      case Some(errMsg) => state.updateHandler( new SequentialCommandHandler(name, description, handlers, executor, errMsg) )

    }
  }
  def getPrompt( state: ShellState ) = errMsg + ( if(handlers.isEmpty) "cdas>" else handlers.head.getPrompt( state ) )
  override def toString = s"{$id:%s}".format( if(handlers.isEmpty) "" else handlers.head.id )
}

final class ConditionalCommandHandler( name: String, description: String, val stateKey: String, val handlers: Map[String,CommandHandler], val errMsg: String = ""  )
  extends CommandHandler(name,description) {

  def getHandler(state: ShellState): CommandHandler = {
    state.getProp(stateKey) match {
      case None => throw new Exception( "Improper state; Missing key: " + stateKey )
      case Some( commandKey ) => handlers.find( matches(commandKey.text) _ ) match {
        case None => throw new Exception( "Improper state; Missing handler for key: " + commandKey.text )
        case Some( (key, handler) ) => handler
      }
    }
  }

  def matches( commandKey: String )( item: (String, CommandHandler) ): Boolean = item._1.toLowerCase.startsWith(commandKey.toLowerCase())

  def process(state: ShellState): ShellState = {
    val handler = getHandler(state)
    handler.validate(state.history.last, state) match {
      case None => state.delegate(handler)
      case Some(errMsg) => state.updateHandler( new ConditionalCommandHandler(name, description, stateKey, handlers, errMsg) )
    }
  }
  def getPrompt( state: ShellState ) = errMsg + getHandler(state).getPrompt( state )
  override def toString = s"{$id:%s}".format( if(handlers.isEmpty) "" else handlers.values.map(_.id).mkString("-") )
}

final class ListSelectionCommandHandler( name: String, description: String, val getChoices:(ShellState) => Array[String],
                                         val executor: (Array[String],ShellState) => ShellState, var errorState: Boolean = false) extends CommandHandler(name,description) {
  def getSelectionList(state: ShellState): String = getChoices(state).zipWithIndex.map { case (v, i) => s"\t $i: $v" } mkString ("\n")

  def process( state: ShellState): ShellState = {
    val command = state.getTopCommand
    if (command.isEmpty) { state.popHandler() }
    else try {
      val args = getArgs(command)
      // printf( "Processing args: " + args.mkString(",") )
      executor( args.map( arg => getChoices(state)(arg.toInt) ), state ).popHandler()
    } catch {
      case t: Throwable =>
        println( t.getMessage )
        state.updateHandler( new ListSelectionCommandHandler(name, description, getChoices, executor, true) )
    }
  }

  override def validate( command: String, state: ShellState ): Option[String] = {
    if( command.isEmpty ) None
    else try{ getArgs(command).foreach( sval => assertBounds( sval.toInt, 0, getChoices(state).length-1 )  ); None } catch { case ex: Throwable => Some("Entry error: %s\n".format(ex.getMessage)) }
  }

  def getPrompt( state: ShellState ) = if (errorState) "   Invalid entry, please try again: " else s"$description:\n%s\n > Enter index(es) of choice(s): ".format( getSelectionList(state: ShellState) )
}

class SelectionCommandHandler( name: String, description: String, val prompt: String, val cmd_handlers: Array[CommandHandler] ) extends CommandHandler(name,description) {
  val handlers = cmd_handlers.sortWith( (c0,c1) => c0.id.length > c1.id.length )
  def getPrompt( state: ShellState ) = prompt
  def process( state: ShellState ):  ShellState = {
    handlers.find( _.matches( state.getTopCommand ) ) match { case Some(handler) => state.pushHandler( handler ); case None => state.popHandler() }
  }
  override def help: String = { s"Commands: \n" + handlers.map( _.help ).mkString("\n") }
}

class HelpHandler(name: String, description: String ) extends CommandHandler(name,description)  {
  def process( state: ShellState ): ShellState = { state.popHandler() }
  def getPrompt( state: ShellState ): String = state.handlerStack.head.help + "\n"
}

final class HistoryHandler( name: String, val executor: (String) => Unit, var errorState: Boolean = false ) extends CommandHandler( name, "Displays and (optionally) executes commands from the shell history" )  {
  def process( state: ShellState): ShellState = {
    val choices: Vector[String] = state.history.zipWithIndex.map { case (v, i) => s"\t $i: $v" }
    val command = state.getTopCommand
    if (command.isEmpty) { state.popHandler() }
    else try {
      executor(choices(command.toInt));
      state.popHandler()
    } catch {
      case t: Throwable => state.updateHandler( new HistoryHandler(name, executor, true) )
    }
  }
  def getPrompt( state: ShellState ): String = {
    val selectionList: String = state.history.zipWithIndex.map { case (v, i) => s"\t $i: $v" } mkString ("\n")
    if (errorState) "   Invalid entry, please try again: " else s"Command History:\n$selectionList\n > Enter index to execute: "
  }
}
