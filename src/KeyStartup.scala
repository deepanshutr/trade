package desk

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

// The key step of start-up, before the workspace and before diagnostics. It opens the key screen
// when asked or when live trading has no usable key, finds the active key and reads the Keychain
// once for the whole session.
object KeyStartup:
  final case class Hooks(
      home: Path = Path.of(System.getProperty("user.home")),
      screen: KeyScreen.Context => Option[String] = context => KeyScreen.runOnTerminal(context),
      security: Seq[String] => Int = KeychainItems.security,
      readKey: (TradingConfig, Int) => Array[Byte] = ProtectedKey.read,
      worker: (TradingConfig, Boolean, String, Path, () => Array[Byte]) => TradingWorker =
        TradingWorker.create,
      say: String => Unit = text => println(text),
      stop: (Int, Option[String]) => Nothing = (code, message) =>
        message.foreach(text => System.err.println(text))
        sys.exit(code)
  )

  final case class Prepared(config: TradingConfig, session: Option[SessionKey])

  private val waiting = "Waiting for the Keychain prompt. Choose Allow, not Always Allow."

  // A usable key has an active choice for the network, a name on that network and a Keychain item.
  private def check(config: TradingConfig, hooks: Hooks): Either[String, String] =
    for
      names <- KeyNames.load(KeyNames.path(hooks.home))
      active <- ActiveKey.load(ActiveKey.path(hooks.home))
      address <- KeySetup.usable(
        config.network,
        names,
        active,
        found => KeychainItems.present(config.keyService, found, hooks.security)
      )
    yield address

  private def screen(config: TradingConfig, stateDirectory: Path, hooks: Hooks): Option[String] =
    val context = KeyScreen
      .system(config.network, config.keyService, hooks.home, stateDirectory)
      .copy(security = hooks.security)
    Try(hooks.screen(context)) match
      case Success(address) => address
      case Failure(error)   =>
        hooks.stop(1, Some("Key screen stopped: " + error.getClass.getSimpleName))

  def prepare(
      args: Main.Args,
      config: TradingConfig,
      stateDirectory: Path,
      hooks: Hooks
  ): Prepared =
    if args.keys then
      val _ = screen(config, stateDirectory, hooks)
      hooks.stop(0, None)
    else if args.record || !(args.trade || config.owner.nonEmpty) then Prepared(config, None)
    else
      val found = check(config, hooks).toOption.orElse(
        if args.trade then screen(config, stateDirectory, hooks) else None
      )
      found match
        case None =>
          if args.trade then hooks.stop(1, Some("No trading key")) else Prepared(config, None)
        case Some(address) =>
          val keyed = config.copy(agent = Some(address))
          if config.owner.isEmpty then Prepared(keyed, None)
          else
            hooks.say(waiting)
            val session = new SessionKey(() => hooks.readKey(keyed, 120))
            val _ = session.load()
            Prepared(keyed, Some(session))
