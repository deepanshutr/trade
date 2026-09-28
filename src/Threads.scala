package desk

object Threads:
  def daemon(name: String)(body: => Unit): Thread =
    val thread = new Thread(
      () => {
        try body
        catch case _: InterruptedException => ()
      },
      name
    )
    thread.setDaemon(true)
    thread
