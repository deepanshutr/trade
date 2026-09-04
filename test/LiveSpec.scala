package desk

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

class LiveSpec extends munit.FunSuite:
  val live = new munit.Tag("live")
  test("mainnet delivers a book within five seconds".tag(live)) {
    val queue = new ConcurrentLinkedQueue[Event]()
    val feed = new Feed(JdkConnector, JdkRest, queue, () => System.currentTimeMillis(), "XMR")
    val t = new Thread(() => { val _ = feed.connectOnce() })
    t.setDaemon(true)
    t.start()
    val deadline = System.currentTimeMillis() + 5000
    def hasBook = queue.asScala.exists(_.isInstanceOf[Event.BookUpdate])
    while System.currentTimeMillis() < deadline && !hasBook do Thread.sleep(50)
    assert(hasBook, queue.asScala.toVector.take(5))
  }
