package desk

import java.io.{InputStream, PrintStream}
import java.nio.file.Path
import scala.util.{Failure, Success, Try}
import sun.misc.{Signal, SignalHandler}

// Settings hands the terminal to a short child process: the key screen, which is Desk itself with
// --keys, or one owner step of the vault. The child owns the terminal until it exits. Desk never
// reads what the child prints. The files the child leaves are its result.
object Child:
  val notInstalled = "Owner tooling is not installed"
  private val notStarted = "The step did not start"
  private val networks = Set("mainnet", "testnet")

  def keyScreen(args: Main.Args, java: String, jar: String): Seq[String] =
    Seq(java, "-XX:+DisableAttachMechanism", "-jar", jar, "--keys") ++
      args.config.toSeq.flatMap(Seq("--config", _)) ++
      args.stateDirectory.toSeq.flatMap(Seq("--state", _))

  private def absolute(env: Map[String, String], name: String): Option[String] =
    env.get(name).filter(value => Try(Path.of(value).isAbsolute).getOrElse(false))

  // The four vault entry points Desk calls. Nothing else runs.
  private def ownerStep(words: Seq[String]): Boolean = words match
    case Seq("--json", "owner", "status", "--network", network)           => networks(network)
    case Seq("owner", "new" | "resume" | "approve", "--network", network) => networks(network)
    case Seq("owner", "approve", "--network", network, "--config", path)  =>
      networks(network) && path.nonEmpty
    case _ => false

  def vault(env: Map[String, String], words: Seq[String]): Either[String, Seq[String]] =
    (absolute(env, "DESK_PYTHON"), absolute(env, "DESK_VAULT")) match
      case (Some(python), Some(folder)) if ownerStep(words) =>
        Right(Seq(python, "-B", Path.of(folder).resolve("desk_vault.py").toString) ++ words)
      case (Some(_), Some(_)) => Left("Not an owner step")
      case _                  => Left(notInstalled)

  // Starts the child inside the guard, waits for it without a time limit, and runs enter after it
  // exits, still inside the guard, so Control C at the Enter line cannot stop Desk either. A child
  // that cannot start gives the reason and no Enter line. A guard that cannot be put in place
  // throws before anything starts. A wait that ends early stops the child, so it never keeps the
  // terminal after Desk.
  def runOnTerminal(
      command: Seq[String],
      start: Seq[String] => Process,
      guard: (() => Unit) => Unit,
      enter: () => Unit
  ): Either[String, Int] =
    var outcome: Either[String, Int] = Left(notStarted)
    guard { () =>
      outcome = Try(start(command)) match
        case Failure(error) =>
          Left(notStarted + ": " + Option(error.getMessage).getOrElse(error.getClass.getSimpleName))
        case Success(process) =>
          val exit =
            try process.waitFor()
            finally if process.isAlive then process.destroy()
          enter()
          Right(exit)
    }
    outcome

  // Swaps in a handler that does nothing and puts the replaced one back, even when the body throws.
  private[desk] def ignoring(handle: SignalHandler => SignalHandler)(body: () => Unit): Unit =
    val previous = handle(_ => ())
    try body()
    finally
      val _ = handle(previous)

  // In cooked mode Control C sends SIGINT to Desk and the child together. The child keeps the
  // default and stops. Desk does nothing and keeps waiting. SIGQUIT is not swapped: the JVM keeps
  // it for itself and answers Control backslash with a thread dump, so it cannot stop Desk.
  val ignoreSignals: (() => Unit) => Unit =
    ignoring(handler => Signal.handle(new Signal("INT"), handler))

  def awaitEnter(input: InputStream, output: PrintStream): Unit =
    output.print("Press Enter to return to Desk.")
    output.flush()
    var byte = input.read()
    while byte != -1 && byte != '\n' do byte = input.read()
