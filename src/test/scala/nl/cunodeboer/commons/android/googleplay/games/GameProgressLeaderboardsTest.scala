package nl.cunodeboer.commons.android.googleplay.games

import java.util.concurrent.TimeUnit

import com.google.android.gms.common.api.PendingResult.a
import com.google.android.gms.common.api._
import com.google.android.gms.games.leaderboard.LeaderboardVariant.{TIME_SPAN_ALL_TIME, TIME_SPAN_DAILY, TIME_SPAN_WEEKLY}
import com.google.android.gms.games.leaderboard.Leaderboards
import com.google.android.gms.games.leaderboard.Leaderboards.{LoadPlayerScoreResult, SubmitScoreResult}
import grizzled.slf4j.Logging
import nl.cunodeboer.commons.android.googleplay.games.GooglePlayGamesProperty._
import nl.cunodeboer.commons.android.googleplay.games.Helpers.mkGameProgress_LB
import org.joda.time.DateTime
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.concurrent.AsyncAssertions
import org.scalatest.concurrent.AsyncAssertions.{Waiter => W}
import org.scalatest.mock.MockitoSugar
import org.scalatest.time.SpanSugar._
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}

class GameProgressLeaderboardsTest extends FunSuite with Matchers with BeforeAndAfterEach with MockitoSugar with AsyncAssertions with Logging {

  final val AllThreeTimeSpans = Set(TIME_SPAN_ALL_TIME, TIME_SPAN_WEEKLY, TIME_SPAN_DAILY)

  implicit def intTimes(i: Int) = new {
    def times(fn: => Unit) = (1 to i) foreach (x => fn)
  }

  def fakeSubmitted(timeSpan: GooglePlayGamesProperty.Value)(at: Long)(implicit gp: GameProgress) {
    gp.updateTimestampModified(timeSpan, at - 100)
    gp.updatetimestampSubmitted(timeSpan, at)
  }

  val MaxCallbackResponseTimeMillis = 500

  import nl.cunodeboer.commons.android.googleplay.games.AchievementResultType._

  /** Fake PendingResult[Leaderboards.SubmitScoreResult] submit */
  class FakePendingResultSubmitScoreResult(mockedUpdateSubmitScoreResult: Leaderboards.SubmitScoreResult, waiter: W) extends PendingResult[Leaderboards.SubmitScoreResult] {
    override def await(): SubmitScoreResult = ???

    override def await(p1: Long, p2: TimeUnit): SubmitScoreResult = ???

    override def cancel(): Unit = ???

    override def isCanceled: Boolean = ???

    override def a(p1: a): Unit = ???

    override def setResultCallback(resultCallback: ResultCallback[SubmitScoreResult]) = {
      resultCallback.onResult(mockedUpdateSubmitScoreResult)
      if (waiter != null) waiter.dismiss()
    }

    override def setResultCallback(resultCallback: ResultCallback[SubmitScoreResult], p2: Long, p3: TimeUnit): Unit = ???
  }

  object FakePendingResultSubmitScoreResult {
    def apply(mockedSubmitScoreResultResult: Leaderboards.SubmitScoreResult, waiter: W) = new FakePendingResultSubmitScoreResult(mockedSubmitScoreResultResult, waiter)
  }

  /** Fake PendingResult[Leaderboards.LoadPlayerScoreResult] load */
  class FakePendingResultLoadPlayerScoreResult(mockedLoadPlayerScoreResult: Leaderboards.LoadPlayerScoreResult, waiter: W) extends PendingResult[Leaderboards.LoadPlayerScoreResult] {

    override def await(): LoadPlayerScoreResult = ???

    override def await(p1: Long, p2: TimeUnit): LoadPlayerScoreResult = ???

    override def cancel(): Unit = ???

    override def isCanceled: Boolean = ???

    override def a(p1: a): Unit = ???

    override def setResultCallback(p1: ResultCallback[LoadPlayerScoreResult], p2: Long, p3: TimeUnit): Unit = ???

    override def setResultCallback(resultCallback: ResultCallback[LoadPlayerScoreResult]) = {
      new Thread(new Runnable {
        def run() {
          Thread.sleep(50)
          resultCallback.onResult(mockedLoadPlayerScoreResult)
          waiter.dismiss()
        }
      }).start()
    }
  }

  object FakePendingResultLoadPlayerScoreResult {
    def apply(mockedLoadPlayerScoreResult: Leaderboards.LoadPlayerScoreResult, waiter: W) = new FakePendingResultLoadPlayerScoreResult(mockedLoadPlayerScoreResult, waiter)
  }

  test("isBetterThen method of implicit class ScoreUtils works when lower is better") {
    val waiter = new W
    val gp_lowerBetter = mkGameProgress_LB(waiter)
    import gp_lowerBetter.ScoreUtils

    1 isBetterThen Some(2) shouldBe true
    2 isBetterThen Some(1) shouldBe false
    1 isBetterThen Some(-2) shouldBe false
    2 isBetterThen Some(0) shouldBe false
    2 isBetterThen None shouldBe true
    -2 isBetterThen None shouldBe true
  }

  test("isBetterThen method of implicit class ScoreUtils works when higher is better") {
    val gp_largerBetter = mkGameProgress_LB(null, Ok, false)
    import gp_largerBetter.ScoreUtils

    1 isBetterThen Some(2) shouldBe false
    2 isBetterThen Some(1) shouldBe true
    1 isBetterThen Some(-2) shouldBe true
    2 isBetterThen Some(0) shouldBe true
    2 isBetterThen None shouldBe true
    -2 isBetterThen None shouldBe true
  }

  test("isScoreSet works") {
    val gp = mkGameProgress_LB()
    gp.isScoreAllTimeSet shouldBe false
    gp.updateScore(1212)
    gp.isScoreAllTimeSet shouldBe true
  }

  test("Score and previousScore for all three time spans have the expected initial values") {
    val gp1 = mkGameProgress_LB()
    gp1.isScoreAllTimeSet shouldBe false
    gp1.isScoreWeeklySet shouldBe false
    gp1.isScoreDailySet shouldBe false
    gp1.isPreviousScoreAllTimeSet shouldBe false
    gp1.isPreviousScoreWeeklySet shouldBe false
    gp1.isPreviousScoreDailySet shouldBe false
    gp1.scoreAllTime should be(None)
    gp1.scoreWeekly should be(None)
    gp1.scoreDaily should be(None)
    gp1.updateScore(100) shouldBe AllThreeTimeSpans
    gp1.isScoreAllTimeSet shouldBe true
    gp1.scoreAllTime should be(Some(100))
  }

  test("updateScore only sets lower values when configured as lower is better") {
    val gp = mkGameProgress_LB(null, Ok, true)
    gp.updateScore(5000) shouldBe AllThreeTimeSpans
    gp.scoreAllTime.get shouldBe 5000
    gp.updateScore(5000) shouldBe empty
    gp.scoreAllTime.get shouldBe 5000
    gp.updateScore(6000) shouldBe empty
    gp.scoreAllTime.get shouldBe 5000
    gp.updateScore(4999) shouldBe AllThreeTimeSpans
    gp.scoreAllTime.get shouldBe 4999
    gp.updateScore(3500) shouldBe AllThreeTimeSpans
    gp.scoreAllTime.get shouldBe 3500
    gp.updateScore(4000) shouldBe empty
    gp.scoreAllTime.get shouldBe 3500
    gp.updateScore(100) shouldBe AllThreeTimeSpans
    gp.scoreAllTime.get shouldBe 100
    gp.updateScore(200) shouldBe empty
    gp.scoreAllTime.get shouldBe 100
    gp.updateScore(400) shouldBe empty
    gp.scoreAllTime.get shouldBe 100
  }

  test("updateScore only sets higher values when configured as higher is better") {
    val gp = mkGameProgress_LB(null, Ok, false)
    gp.updateScore(5000) shouldBe AllThreeTimeSpans
    gp.scoreAllTime shouldBe Some(5000)
    gp.updateScore(5000) shouldBe empty
    gp.scoreAllTime shouldBe Some(5000)
    gp.updateScore(6000) shouldBe AllThreeTimeSpans
    gp.scoreAllTime shouldBe Some(6000)
    gp.updateScore(5999) shouldBe empty
    gp.scoreAllTime shouldBe Some(6000)
    gp.updateScore(3500) shouldBe empty
    gp.scoreAllTime shouldBe Some(6000)
    gp.updateScore(6001) shouldBe AllThreeTimeSpans
    gp.scoreAllTime shouldBe Some(6001)
    gp.updateScore(10000) shouldBe AllThreeTimeSpans
    gp.scoreAllTime shouldBe Some(10000)
    gp.updateScore(200) shouldBe empty
    gp.scoreAllTime shouldBe Some(10000)
    gp.updateScore(10001) shouldBe AllThreeTimeSpans
    gp.scoreAllTime shouldBe Some(10001)
  }

  test("The previousScore is updated only when score is updated") {
    val gpL = mkGameProgress_LB(null, Ok, false)
    val gpS = mkGameProgress_LB(null, Ok, true)
    // Update if higher
    gpL.scoreAllTime shouldBe empty
    gpL.previousScoreAllTime shouldBe empty
    gpL.updateScore(10) shouldBe AllThreeTimeSpans // 10 | None
    gpL.previousScoreAllTime shouldBe empty
    gpL.updateScore(10) shouldBe empty // No update of score
    gpL.previousScoreAllTime shouldBe empty
    gpL.updateScore(9) shouldBe empty // No update of score
    gpL.previousScoreAllTime shouldBe empty
    gpL.updateScore(11) shouldBe AllThreeTimeSpans // 11 | 10
    gpL.previousScoreAllTime should be(Some(10))

    // Update if lower
    gpS.updateScore(15) shouldBe AllThreeTimeSpans // 15 | None
    gpS.previousScoreAllTime shouldBe empty
    gpS.updateScore(11) shouldBe AllThreeTimeSpans // 11 | 15
    gpS.previousScoreAllTime should be(Some(15))
    gpS.updateScore(12) shouldBe empty // No update of score
    gpS.previousScoreAllTime should be(Some(15))
    gpS.updateScore(5) shouldBe AllThreeTimeSpans // 5 | 11
    gpS.previousScoreAllTime should be(Some(11))
    gpS.updateScore(5) shouldBe empty // No update of score
    gpS.previousScoreAllTime should be(Some(11))
    gpS.updateScore(4) shouldBe AllThreeTimeSpans // 4 | 5
    gpS.previousScoreAllTime should be(Some(5))
  }

  test("Updating to a better score of a wider time span also updates the encompassing scores") {
    val gp = mkGameProgress_LB(null, Ok, false)
    val initialScore = Some(1000)
    val betterScore = Some(2000)

    gp.updateScore(initialScore.get)
    gp.scoreAllTime shouldBe initialScore
    gp.scoreDaily shouldBe initialScore
    gp.scoreWeekly shouldBe initialScore
    gp.previousScoreAllTime shouldBe empty
    gp.previousScoreWeekly shouldBe empty
    gp.previousScoreDaily shouldBe empty

    gp.updateScore(betterScore.get)
    gp.scoreAllTime shouldBe betterScore
    gp.scoreDaily shouldBe betterScore
    gp.scoreWeekly shouldBe betterScore
    gp.previousScoreAllTime shouldBe initialScore
    gp.previousScoreWeekly shouldBe initialScore
    gp.previousScoreDaily shouldBe initialScore
  }

  test("Updating a score of a time span only to a value that is only better after a reset, updates the same and more narrow time span scores") {
    val gp = mkGameProgress_LB(null, Ok, false)
    val allTimeBestScore = Some(2000)
    val dailyBestScore = Some(1500)
    val weeklyBestScore = Some(1750)

    val dt = new DateTime(gp.lbResetTimezone).withMillis(gp.currentTimeMillis())

    gp.updateScore(allTimeBestScore.get)

    // Same day, so no update of best weekly and daily score.
    gp.updateScore(dailyBestScore.get)
    gp.scoreAllTime shouldBe allTimeBestScore
    gp.scoreWeekly shouldBe allTimeBestScore
    gp.scoreDaily shouldBe allTimeBestScore

    // The daily score, initially set to the same value as the all-time score, is now more than a day old. The all-time
    // best score does no longer count as best of the day, it still stands as best of the week though.
    gp.updateTimestampModified(ScoreDaily, dt.minusHours(25).getMillis)
    gp.updateScore(dailyBestScore.get)
    gp.scoreAllTime shouldBe allTimeBestScore
    gp.scoreWeekly shouldBe allTimeBestScore
    gp.scoreDaily shouldBe dailyBestScore
    gp.previousScoreAllTime shouldBe empty
    gp.previousScoreWeekly shouldBe empty
    gp.previousScoreDaily shouldBe allTimeBestScore

    gp.updateTimestampModified(ScoreDaily, dt.getMillis)

    // Same day, so no change in best weekly score because it is still set to a better value. But the daily score should
    // be updated to the better score.
    gp.updateScore(weeklyBestScore.get)
    gp.scoreAllTime shouldBe allTimeBestScore
    gp.scoreWeekly shouldBe allTimeBestScore
    gp.scoreDaily shouldBe weeklyBestScore

    // The weekly score was set in the previous week, so the all-time best value does not count as best of the week any longer and
    // should therefore be updated to the worse new score.
    gp.updateTimestampModified(ScoreWeekly, dt.minusDays(8).getMillis)
    gp.updateScore(weeklyBestScore.get)
    gp.scoreAllTime shouldBe allTimeBestScore
    gp.scoreWeekly shouldBe weeklyBestScore
    gp.scoreDaily shouldBe weeklyBestScore
    gp.previousScoreAllTime shouldBe empty
    gp.previousScoreWeekly shouldBe allTimeBestScore
    gp.previousScoreDaily shouldBe weeklyBestScore

    gp.updateTimestampModified(ScoreWeekly, dt.getMillis)

    // Update the daily score to a score that is lower than the weekly score it is in.
    gp.updateTimestampModified(ScoreDaily, dt.minusHours(25).getMillis)
    gp.updateScore(weeklyBestScore.get - 10)
    gp.scoreDaily shouldBe Some(weeklyBestScore.get - 10)
  }

  test("Leaderboard ID is optional when no score is set") {
    val gp = mkGameProgress_LB(null, MissingID)
    gp.syncUp()

    verify(gp.leaderboardsApi, never).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
  }

  test("Leaderboard ID is not optional when the score is set, an IllegalStateException is thrown") {
    val gp = mkGameProgress_LB(null, MissingID)
    gp.updateScore(1234)
    intercept[IllegalStateException] {
      gp.syncUp()
    }

    verify(gp.leaderboardsApi, never).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
  }

  test("Synchronizing the score up only submits when it's updated later than it was submitted") {
    val waiter = new W
    val gp = mkGameProgress_LB(waiter, Ok, false)
    val leaderboardsApiMock = gp.leaderboardsApi

    gp.updateScore(1234)
    Thread.sleep(10)

    gp.isScoreAllTimeSet shouldBe true

    gp.syncUp()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(1))

    verify(leaderboardsApiMock, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())

    gp.syncUp() // Should not submit already submitted score.

    verify(leaderboardsApiMock, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())

    gp.updateScore(1235)
    Thread.sleep(10)

    gp.syncUp() // Should submit updated score (can be the same value but that should not happen in practise).
    verify(leaderboardsApiMock, times(2)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
  }

  test("Failure synchronize the score up can be retried and still succeed") {
    val waiter = new W
    val gp = mkGameProgress_LB(waiter, Errors)
    val leaderboardsApiMock = gp.leaderboardsApi

    gp.updateScore(1234)

    gp.isScoreAllTimeSet shouldBe true

    // First time should fail.
    gp.syncUp()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(1))
    verify(leaderboardsApiMock, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())

    // Retry should succeed.
    gp.syncUp()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(1))
    verify(leaderboardsApiMock, times(2)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
  }

  test("Modified and remote sync timestamp maps work") {
    val gp = mkGameProgress_LB()

    for (property <- List(ScoreAllTime, Achs, IncAchs)) {
      gp.timestampModified get property shouldBe empty
      gp.timestampSubmitted get property shouldBe empty
      gp.updateTimestampModified(property, 1234567)
      gp.updatetimestampSubmitted(property, 7654321)
      gp.timestampModified get property should be(Some(1234567))
      gp.timestampSubmitted get property should be(Some(7654321))
    }
  }

  test("Sync up does not submit an already submitted score") {
    implicit val gp = mkGameProgress_LB()
    val leaderboardsApiMock = gp.leaderboardsApi
    val now = gp.currentTimeMillis()

    gp.updateScore(1234)
    fakeSubmitted(ScoreAllTime)(now)
    fakeSubmitted(ScoreWeekly)(now)
    fakeSubmitted(ScoreDaily)(now)

    gp.syncUp()
    verify(leaderboardsApiMock, never).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
  }

  test("Sync up with an already submitted all-time or weekly score do not submit those again") {
    implicit val gp = mkGameProgress_LB()
    val leaderboardsApiMock = gp.leaderboardsApi

    gp.updateScore(1000)

    gp.timestampSubmitted get ScoreAllTime shouldBe empty
    gp.timestampSubmitted get ScoreWeekly shouldBe empty
    gp.timestampSubmitted get ScoreDaily shouldBe empty

    gp.syncUp()

    verify(leaderboardsApiMock, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    gp.timestampSubmitted get ScoreAllTime should not be empty
    gp.timestampSubmitted get ScoreWeekly should not be empty
    gp.timestampSubmitted get ScoreDaily should not be empty

    val unchangedAllTimeTimestamp = gp.currentTimeMillis()

    gp.updateScore(999)
    fakeSubmitted(ScoreAllTime)(unchangedAllTimeTimestamp)
    gp.syncUp()

    verify(leaderboardsApiMock, times(2)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    gp.timestampSubmitted get ScoreAllTime shouldBe Some(unchangedAllTimeTimestamp)

    val unchangedWeeklyTimestamp = gp.currentTimeMillis()

    gp.updateScore(998)
    fakeSubmitted(ScoreAllTime)(unchangedAllTimeTimestamp)
    fakeSubmitted(ScoreWeekly)(unchangedWeeklyTimestamp)
    gp.syncUp()

    verify(leaderboardsApiMock, times(3)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    gp.timestampSubmitted get ScoreAllTime shouldBe Some(unchangedAllTimeTimestamp)
    gp.timestampSubmitted get ScoreWeekly shouldBe Some(unchangedWeeklyTimestamp)
  }

  test("Sync up submits a score when is has not been submitted yet") {
    val waiter = new W
    implicit val gp = mkGameProgress_LB(waiter)
    val leaderboardsApiMock = gp.leaderboardsApi
    val now = gp.currentTimeMillis()

    gp.updateScore(1234)

    fakeSubmitted(ScoreAllTime)(now)

    gp.syncUp()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(1))
    verify(leaderboardsApiMock, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
  }

  test("Sync up does not submit a None score") {
    implicit val gp = mkGameProgress_LB()
    val leaderboardsApiMock = gp.leaderboardsApi
    val now = gp.currentTimeMillis()

    gp.isScoreAllTimeSet shouldBe false

    fakeSubmitted(ScoreAllTime)(now)

    gp.syncUp()
    verify(leaderboardsApiMock, never).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
  }

  test("Bi-directional sync score with worse local score does not submit to remote") {
    import com.google.android.gms.games.leaderboard.LeaderboardVariant.{COLLECTION_PUBLIC, COLLECTION_SOCIAL, TIME_SPAN_ALL_TIME}
    import org.mockito.Matchers.{eq => is}

    val waiter = new W
    val gp = mkGameProgress_LB(waiter, Ok)

    // Set worse local score than remote score of 10.
    gp.updateScore(123)

    // Score found in public leaderboard, no need so look in the social leaderboard.
    gp.syncScore()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(1))
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, never).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, never).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    gp.scoreAllTime shouldBe Some(10)

    // Score not found in public leaderboard but found in social leaderboard.
    gp.syncScore()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(2))
    verify(gp.leaderboardsApi, times(2)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, never).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    gp.scoreAllTime shouldBe Some(10)
  }

  test("Bi-directional sync score with better local score submits it to remote") {
    import com.google.android.gms.games.leaderboard.LeaderboardVariant.{COLLECTION_PUBLIC, COLLECTION_SOCIAL, TIME_SPAN_ALL_TIME}
    import org.mockito.Matchers.{eq => is}

    val waiter = new W
    val gp = mkGameProgress_LB(waiter, Ok)

    // Set better local score than remote score of 10.
    gp.updateScore(8)

    // Score found in public leaderboard, no need so look in the social leaderboard.
    gp.syncScore()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(2))
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, never).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    gp.scoreAllTime shouldBe Some(8)

    // Set better local score than remote score of 8.
    gp.updateScore(7)

    // Score not found in public leaderboard but found in social leaderboard.
    gp.syncScore()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(2))
    verify(gp.leaderboardsApi, times(2)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, times(2)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    gp.scoreAllTime shouldBe Some(7)
  }

  test("Bi-directional sync score with failing sync down does not sync up afterwards.") {
    import com.google.android.gms.games.leaderboard.LeaderboardVariant.{COLLECTION_PUBLIC, COLLECTION_SOCIAL, TIME_SPAN_ALL_TIME}
    import org.mockito.Matchers.{eq => is}

    val waiter = new W
    val gp = mkGameProgress_LB(waiter, Errors)

    // Set better local score than remote score of 10.
    gp.updateScore(8)

    // Score found in public leaderboard, no need so look in the social leaderboard.
    gp.syncScore()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(1))
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, never).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, never).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    gp.scoreAllTime shouldBe Some(8)
  }

}