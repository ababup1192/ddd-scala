import java.time.LocalDateTime
import java.util.UUID

@main def hello(): Unit =
  val screening = Screening(PositionId())
  println(screening.addInterview(LocalDateTime.now()))
  // Runtime error
  println(screening.adopt.addInterview(LocalDateTime.now()))

case class Screening private(screeningId: ScreeningId, positionId: PositionId,
                             interviewSeq: InterviewSeq, screeningStatus: ScreeningStatus, applyDateTime: LocalDateTime):
  def addInterview(interviewDateTime: LocalDateTime): Screening =
    if (!screeningStatus.canAddInterview)
      throw RuntimeException("面接が選考中ではありません")
    else
      this.copy(interviewSeq = interviewSeq.addInterview(interviewDateTime))

  def adopt =
    this.copy(screeningStatus = ScreeningStatus.ADOPTED)

  def reject =
    this.copy(screeningStatus = ScreeningStatus.REJECTED)

object Screening:
  def apply(positionId: PositionId): Screening =
    new Screening(screeningId = ScreeningId(),
      positionId = positionId,
      interviewSeq = InterviewSeq(),
      screeningStatus = ScreeningStatus.IN_PROGRESS,
      applyDateTime = LocalDateTime.now()
    )

// DBからインスタンス生成。fromDb
// def reconstruct


/**
 * 採用選考ID
 */
case class ScreeningId private(value: String) extends AnyVal

object ScreeningId:
  def apply(): ScreeningId =
    new ScreeningId(UUID.randomUUID().toString)

enum ScreeningStatus(val canAddInterview: Boolean):
  /** 選考中 */
  case IN_PROGRESS extends ScreeningStatus(true)

  /** 採用 */
  case ADOPTED extends ScreeningStatus(false)

  /** 不採用 */
  case REJECTED extends ScreeningStatus(false)

/**
 * 採用ポジションID
 */
case class PositionId private(value: String) extends AnyVal

object PositionId:
  def apply(): PositionId =
    new PositionId(UUID.randomUUID().toString)

/**
 * 面接
 */
case class Interview(
                      /** 面接次数 */
                      phase: Int,

                      /** 面接日時 */
                      dateTime: LocalDateTime)

/**
 * 面接のファーストクラスコレクション
 */
case class InterviewSeq private(value: Seq[Interview]):

  /** 面接を追加します */
  def addInterview(interviewDateTime: LocalDateTime): InterviewSeq =
    new InterviewSeq(
      this.value :+ Interview(
        this.value.length + 1 // 既存の面接の１つ後の面接次数を設定
        ,
        interviewDateTime
      )
    )

object InterviewSeq:
  def apply(): InterviewSeq = new InterviewSeq(Seq())

/** 応募者 */
case class Applicant(name: String, mailAddress: String)
