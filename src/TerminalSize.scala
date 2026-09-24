package desk

/** Serialize a size observation with its publication, independently of rendering. */
private[desk] final class TerminalSize(
    measure: () => (Int, Int),
    publish: ((Int, Int)) => Unit
):
  def read(): (Int, Int) = synchronized {
    val size = measure()
    publish(size)
    size
  }
