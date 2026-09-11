package desk

import java.time.Instant
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

final class AnalysisWorker(
    snapshot: () => Snapshot,
    news: () => String,
    request: (String, () => Boolean, Boolean) => Either[String, ModelReply],
    emit: (String, String) => Unit,
    wall: () => Long,
    automaticEnabled: Boolean = true
) extends AutoCloseable:
  private final case class Question(text: String, market: Snapshot, at: Long)
  private val questions = new ArrayBlockingQueue[Question](4)
  private val schedule = new AnalysisSchedule
  private val closed = new AtomicBoolean(false)
  private val started = new AtomicBoolean(false)
  @volatile private var text = "Analysis waiting for closed candles"
  private var thread = Option.empty[Thread]
  def status: String =
    text + (if questions.isEmpty then "" else s". ${questions.size()} question queued")
  def ask(question: String): Boolean =
    !closed.get() && question.trim.nonEmpty && question.length <= 4000 &&
      questions.offer(Question(question, snapshot(), wall()))

  private def analyze(job: Question, isAutomatic: Boolean): Unit =
    val canceled = () =>
      closed.get() || (isAutomatic && (!questions.isEmpty || snapshot().coin != job.market.coin))
    if !canceled() then
      text = if isAutomatic then "Writing market summary" else "Analyzing your question"
      val newsText = news()
      val context = QuantContext.build(job.market, job.at, newsText, job.text)
      if !isAutomatic then
        emit(
          "data",
          job.market.coin + " computed at " + Instant.ofEpochMilli(job.at) + "\n" + QuantContext
            .readings(job.market, job.at)
        )
      val reply = request(context, canceled, !isAutomatic)
      val completed = reply.flatMap { first =>
        first.study match
          case Some(study) if !canceled() =>
            val result = study.compute(job.market, job.at)
            emit("data", result)
            request(
              QuantContext.build(job.market, job.at, newsText, job.text, Some(result)),
              canceled,
              false
            )
          case _ => Right(first)
      }
      if !canceled() then
        completed match
          case Right(answer) =>
            emit(
              "analysis",
              job.market.coin + " data at " + Instant.ofEpochMilli(job.at) + "\n" + answer.answer
            )
            text = "Analysis updated " + Instant.ofEpochMilli(wall()).toString
          case Left(reason) =>
            emit("data", reason + "\n" + QuantContext.readings(job.market, job.at))
            text = "Model unavailable. Local calculations available"

  def start(): Unit =
    if started.compareAndSet(false, true) then
      val worker = new Thread(
        () => {
          try
            while !closed.get() do
              val question = questions.poll(1000L, TimeUnit.MILLISECONDS)
              if question != null then analyze(question, false)
              else
                val market = snapshot()
                val now = wall()
                val ready = Quant
                  .measure(market.candles.getOrElse("1m", Vector.empty), "1m", now)
                  .status == "complete"
                if automaticEnabled && ready && schedule
                    .automatic(now, QuantContext.fingerprint(market, now))
                then
                  analyze(
                    Question(
                      "Explain the current directional evidence across short, medium and long views and upcoming events.",
                      market,
                      now
                    ),
                    true
                  )
          catch
            case _: InterruptedException => ()
            case _: Exception            => text = "Analysis stopped. Restart to reconnect"
        },
        "market-analysis"
      )
      worker.setDaemon(true)
      thread = Some(worker)
      worker.start()
  def close(): Unit =
    closed.set(true)
    thread.foreach(_.interrupt())
    thread.foreach(_.join(4000L))
