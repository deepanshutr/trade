package desk

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

// The key step of start-up, before the workspace and before diagnostics. It opens the key screen
// only for the settings child, finds the active key and reads the Keychain once. Without a usable
// key the workspace starts anyway and live shows how to add one in settings.
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

  private[desk] val waiting = "Waiting for the Keychain prompt. Choose Allow, not Always Allow."

  // A usable key has an active choice for the network, a name on that network and a Keychain item.
  // Desk takes a key when it may trade: with --trade or with an owner, and never when recording.
  private[desk] def wantsKey(args: Main.Args, config: TradingConfig): Boolean =
    !args.record && (args.trade || config.owner.nonEmpty)

  private[desk] def check(config: TradingConfig, hooks: Hooks): Either[String, String] =
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
    else if !wantsKey(args, config) then Prepared(config, None)
    else
      check(config, hooks).toOption match
        case None          => Prepared(config, None)
        case Some(address) =>
          val keyed = config.copy(agent = Some(address))
          if config.owner.isEmpty then Prepared(keyed, None)
          else
            hooks.say(waiting)
            val session = new SessionKey(() => hooks.readKey(keyed, 120))
            val _ = session.load()
            Prepared(keyed, Some(session))

object KeyReload:
  // The key row when a usable key was not read: the read was denied or timed out.
  val notRead = "The key was not read. Press r to ask again"

// The trading key Desk holds, and the reload after every settings child and on r. The settings
// file is the one source: its sizes and slippage apply at once, and its owner with the active key
// decides the live worker. The reload uses the start-up hooks, so the offline runner stays
// offline.
final class KeyReload(
    args: Main.Args,
    started: TradingConfig,
    initial: Option[SessionKey],
    directory: Path,
    hooks: KeyStartup.Hooks,
    createWorker: (TradingConfig, Boolean, String, Path) => TradingWorker,
    replace: (TradingConfig, String => TradingWorker) => Unit,
    setTerms: TradeTerms => Unit
):
  import KeyReload.notRead
  private var config = started
  @volatile private var held = initial
  private var row = Option.empty[Either[String, String]]

  // The key the workers copy from. Closing Desk closes it after the workers.
  def session: Option[SessionKey] = held

  private def holds(key: SessionKey): Boolean = key.load().isRight

  private def describe(found: Either[String, String]): Either[String, String] =
    found.flatMap { address =>
      if config.owner.nonEmpty && !held.exists(holds) then Left(notRead)
      else
        val name = KeyNames
          .load(KeyNames.path(hooks.home))
          .toOption
          .flatMap(_.find(_.address == address))
          .fold("")(_.name)
        Right(s"$name   ${address.take(6)}...${address.takeRight(4)}   active, in Keychain")
    }

  // The usable-key check as the settings row shows it. The check runs once, then after a reload.
  def keyRow(): Either[String, String] = synchronized {
    row.getOrElse {
      val shown = describe(KeyStartup.check(config, hooks))
      row = Some(shown)
      shown
    }
  }

  def reload(): Either[String, Unit] = synchronized {
    TradingConfig.load(Main.configPath(args, hooks.home)).flatMap { loaded =>
      // The feed and the archives keep the network Desk started with.
      if loaded.network != config.network then Left("Restart Desk to change the network")
      else
        setTerms(TradeTerms(loaded.sizes, loaded.slippage))
        val found = KeyStartup.check(loaded, hooks)
        // The agent follows the start-up rule. A key is read only with an owner.
        val address = found.toOption.filter(_ => KeyStartup.wantsKey(args, loaded))
        val next = loaded.copy(agent = address)
        val session = address.filter(_ => loaded.owner.nonEmpty) match
          case None                                                          => None
          case Some(key) if config.agent.contains(key) && held.exists(holds) => held
          case Some(_)                                                       =>
            // The owner is present and every r is a deliberate ask, so each one reads again.
            hooks.say(KeyStartup.waiting)
            val fresh = new SessionKey(() => hooks.readKey(next, 120))
            val _ = fresh.load()
            Some(fresh)
        // Sizes and slippage travel with each order review, so they alone need no new worker.
        val same = next.copy(sizes = config.sizes, slippage = config.slippage) == config
        try
          if !same || session != held then
            val factory: String => TradingWorker = session match
              case Some(key) => coin => hooks.worker(next, true, coin, directory, () => key.copy())
              case None      => coin => createWorker(next, true, coin, directory)
            replace(next, factory)
            val old = held
            held = session
            // The old worker held its own copy and wiped it when it retired.
            old.filter(key => !session.exists(_ eq key)).foreach(_.close())
        // A session read here that the replace never took is closed, so its key goes with it.
        finally session.filter(key => !held.exists(_ eq key)).foreach(_.close())
        config = next
        row = Some(describe(found))
        Right(())
    }
  }
