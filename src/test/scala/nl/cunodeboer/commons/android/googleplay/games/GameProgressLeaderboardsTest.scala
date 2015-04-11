package nl.cunodeboer.commons.android.googleplay.games

import java.util.concurrent.TimeUnit

import com.google.android.gms.common.api.PendingResult.BatchCallback
import com.google.android.gms.common.api._
import com.google.android.gms.games.leaderboard.LeaderboardVariant.{TIME_SPAN_DAILY, TIME_SPAN_WEEKLY}
import com.google.android.gms.games.leaderboard.Leaderboards
import com.google.android.gms.games.leaderboard.Leaderboards.{LoadPlayerScoreResult, SubmitScoreResult}
import grizzled.slf4j.Logging
import nl.cunodeboer.commons.android.googleplay.games.GooglePlayGamesProperty._
import nl.cunodeboer.commons.android.googleplay.games.Helpers.mkGameProgress_LB
import org.mockito.Matchers.{eq => is, _}
import org.mockito.Mockito._
import org.scalatest.concurrent.AsyncAssertions.{Waiter => W}
import org.scalatest.mock.MockitoSugar
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}
import nl.cunodeboer.commons.android.googleplay.games.Utils.fakeSubmitted

class GameProgressLeaderboardsTest extends FunSuite with Matchers with BeforeAndAfterEach with MockitoSugar with myAsyncAssertions with Logging {

  import nl.cunodeboer.commons.android.googleplay.games.AchievementResultType._

  import MeasureType._

  /** Fake PendingResult[Leaderboards.SubmitScoreResult] submit */
  class FakePendingResultSubmitScoreResult(mockedUpdateSubmitScoreResult: Leaderboards.SubmitScoreResult, waiter: W) extends PendingResult[Leaderboards.SubmitScoreResult] {
    override def await(): SubmitScoreResult = ???

    override def await(p1: Long, p2: TimeUnit): SubmitScoreResult = ???

    override def cancel(): Unit = ???

    override def isCanceled: Boolean = ???

    override def addBatchCallback(batchCallback: BatchCallback): Unit = ???

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

    override def addBatchCallback(batchCallback: BatchCallback): Unit = ???

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
    val waiter = mkWaiter()
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
    val gp_largerBetter = mkGameProgress_LB(null, StatusOk, HigherIsBetter)
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

  test("Updating to a better score of a wider time span also updates the encompassing scores") {
    val gp = mkGameProgress_LB(null, StatusOk, HigherIsBetter)
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
    val waiter = mkWaiter()
    val gp = mkGameProgress_LB(waiter, StatusOk, HigherIsBetter)
    val cbd = mock[CallbackDummy]
    val leaderboardsApiMock = gp.leaderboardsApi

    gp.updateScore(1234)
    Thread.sleep(10)

    gp.isScoreAllTimeSet shouldBe true

    gp.syncUp(cbd.check())

    await(waiter, 1)

    verify(leaderboardsApiMock, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    verify(cbd, times(1)).check()

    gp.syncUp(cbd.check()) // Should not submit already submitted score.

    verify(leaderboardsApiMock, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    verify(cbd, times(2)).check()

    gp.updateScore(1235)
    Thread.sleep(10)

    gp.syncUp(cbd.check()) // Should submit updated score (can be the same value but that should not happen in practice).
    verify(leaderboardsApiMock, times(2)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    verify(cbd, times(3)).check()
  }

  test("Failure synchronize the score up can be retried and still succeed") {
    val waiter = mkWaiter()
    val gp = mkGameProgress_LB(waiter, StatusOkThenError_50times)
    val cbd = mock[CallbackDummy]
    val leaderboardsApiMock = gp.leaderboardsApi

    gp.updateScore(1234)

    gp.isScoreAllTimeSet shouldBe true

    // First time should fail.
    gp.syncUp(cbd.check())
    await(waiter, 1)
    verify(leaderboardsApiMock, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    verify(cbd, times(1)).check()

    // Retry should succeed.
    gp.syncUp(cbd.check())
    await(waiter, 1)
    verify(leaderboardsApiMock, times(2)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    verify(cbd, times(2)).check()
  }

  test("Modified and remote sync timestamp maps work") {
    val gp = mkGameProgress_LB()

    for (property <- List(ScoreAllTime, Achs, IncAchs)) {
      gp.timestampModified get property shouldBe empty
      gp.timestampSubmitted get property shouldBe empty
      gp.updateTimestampModified(property, 1234567)
      gp.updateTimestampSubmitted(property, 7654321)
      gp.timestampModified get property should be(Some(1234567))
      gp.timestampSubmitted get property should be(Some(7654321))
    }
  }

  test("Sync up does not submit an already submitted score") {
    implicit val gp = mkGameProgress_LB()
    val leaderboardsApiMock = gp.leaderboardsApi
    val cbd = mock[CallbackDummy]
    val now = gp.currentTimeMillis()

    gp.updateScore(1234)
    fakeSubmitted(ScoreAllTime)(now)
    fakeSubmitted(ScoreWeekly)(now)
    fakeSubmitted(ScoreDaily)(now)

    gp.syncUp(cbd.check())
    verify(leaderboardsApiMock, never).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    verify(cbd, times(1)).check()
  }

  test("Sync up with an already submitted wider time span score will only submit the next-widest time span's better score") {
    val waiter = mkWaiter()
    val cbWaiter = mkWaiter()
    implicit val gp = mkGameProgress_LB(waiter)
    val leaderboardsApiMock = gp.leaderboardsApi

    val scoreAllTime = 1000L
    val scoreWeekly = 999L
    val scoreDaily = 998L

    gp.updateScore(scoreAllTime)

    gp.timestampSubmitted get ScoreAllTime shouldBe empty
    gp.timestampSubmitted get ScoreWeekly shouldBe empty
    gp.timestampSubmitted get ScoreDaily shouldBe empty

    gp.syncUp {
      cbWaiter.dismiss()
    }

    await(cbWaiter, 1)
    await(waiter, 1)

    // All-time score should be submitted.
    verify(leaderboardsApiMock, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), is(scoreAllTime))
    verify(leaderboardsApiMock, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    gp.timestampSubmitted get ScoreAllTime should not be empty
    gp.timestampSubmitted get ScoreWeekly should not be empty
    gp.timestampSubmitted get ScoreDaily should not be empty

    val unchangedAllTimeTimestamp = gp.currentTimeMillis()

    // Set better weekly score.
    gp.updateScore(999)
    fakeSubmitted(ScoreAllTime)(unchangedAllTimeTimestamp)
    gp.syncUp {
      cbWaiter.dismiss()
    }
    await(cbWaiter, 1)
    await(waiter, 1)

    // Weekly score should be submitted.
    verify(leaderboardsApiMock, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), is(scoreWeekly))
    verify(leaderboardsApiMock, times(2)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    gp.timestampSubmitted get ScoreAllTime shouldBe Some(unchangedAllTimeTimestamp)

    val unchangedWeeklyTimestamp = gp.currentTimeMillis()

    // Set better daily score.
    gp.updateScore(scoreDaily)
    fakeSubmitted(ScoreAllTime)(unchangedAllTimeTimestamp)
    fakeSubmitted(ScoreWeekly)(unchangedWeeklyTimestamp)
    gp.syncUp {
      cbWaiter.dismiss()
    }
    await(cbWaiter, 1)
    await(waiter, 1)

    // Daily score should be submitted.
    verify(leaderboardsApiMock, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), is(scoreDaily))
    verify(leaderboardsApiMock, times(3)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    gp.timestampSubmitted get ScoreAllTime shouldBe Some(unchangedAllTimeTimestamp)
    gp.timestampSubmitted get ScoreWeekly shouldBe Some(unchangedWeeklyTimestamp)
  }

  test("Sync up submits a score when is has not been submitted yet") {
    val waiter = mkWaiter()
    implicit val gp = mkGameProgress_LB(waiter)
    val leaderboardsApiMock = gp.leaderboardsApi
    val cbd = mock[CallbackDummy]
    val now = gp.currentTimeMillis()

    gp.updateScore(1234)

    fakeSubmitted(ScoreAllTime)(now)

    gp.syncUp(cbd.check())
    await(waiter, 1)
    verify(leaderboardsApiMock, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    verify(cbd, times(1)).check()
  }

  test("Sync up does not submit a None score") {
    implicit val gp = mkGameProgress_LB()
    val leaderboardsApiMock = gp.leaderboardsApi
    val cbd = mock[CallbackDummy]
    val now = gp.currentTimeMillis()

    gp.isScoreAllTimeSet shouldBe false

    fakeSubmitted(ScoreAllTime)(now)

    gp.syncUp(cbd.check())
    verify(leaderboardsApiMock, never).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    verify(cbd, times(1)).check()
  }

  test("Bi-directional sync score with worse local score does not submit to remote") {
    import com.google.android.gms.games.leaderboard.LeaderboardVariant.{COLLECTION_PUBLIC, COLLECTION_SOCIAL, TIME_SPAN_ALL_TIME}
    import org.mockito.Matchers.{eq => is}

    val waiter = mkWaiter()
    val cbWaiter = mkWaiter()
    val gp = mkGameProgress_LB(waiter, StatusOk)

    // Set worse local score than remote score of 10 (all three time spans).
    gp.updateScore(123)

    // Score found in public leaderboard, no need so look in the social leaderboard.
    gp.syncScore {
      cbWaiter.dismiss()
    }
    await(cbWaiter, 1)
    await(waiter, 3)
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, never).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), anyInt(), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, never).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    gp.scoreAllTime shouldBe Some(10)
  }

  test("Bi-directional sync score with worse local score does not submit to remote [score in the social collection]") {
    import com.google.android.gms.games.leaderboard.LeaderboardVariant.{COLLECTION_PUBLIC, COLLECTION_SOCIAL, TIME_SPAN_ALL_TIME}
    import org.mockito.Matchers.{eq => is}

    val waiter = mkWaiter()
    val cbWaiter = mkWaiter()
    val gp = mkGameProgress_LB(waiter, StatusOkAndFirstGetScoreReturnsNullForTimeStampAlltime)

    // Set worse local score than remote score of 10 (all three time spans).
    gp.updateScore(123)

    //    withClue("timeout occurred:") {
    //      gp.syncScore() shouldBe true
    //    }

    gp.syncScore {
      cbWaiter.dismiss()
    }
    await(cbWaiter, 1)

    await(waiter, 4)

    // All three time spans should be tried.
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_WEEKLY), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_DAILY), is(COLLECTION_PUBLIC))

    // Only in time span all-time should the score be found in the social collection because, on the LoadPlayerScoreResult mock, the first call to getScore only then returns no value.
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, never).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_WEEKLY), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, never).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_DAILY), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, never).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())

    gp.scoreAllTime shouldBe Some(10)
    gp.scoreWeekly shouldBe Some(10)
    gp.scoreDaily shouldBe Some(10)
  }

  test("Bi-directional sync score with better local score submits it to remote") {
    import com.google.android.gms.games.leaderboard.LeaderboardVariant.{COLLECTION_PUBLIC, COLLECTION_SOCIAL, TIME_SPAN_ALL_TIME}
    import org.mockito.Matchers.{eq => is}

    val waiter = mkWaiter()
    val gp = mkGameProgress_LB(waiter, StatusOk)
    val cbd = mock[CallbackDummy]

    // Set better local score than remote score of 8 (all three time spans).
    gp.updateScore(8)

    // Score found in public leaderboard, no need to look in the social leaderboard.
    gp.syncScore {
      cbd.check()
    }
    await(waiter, 4)
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_WEEKLY), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_DAILY), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, never).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, never).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_WEEKLY), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, never).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_DAILY), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    verify(cbd, times(1)).check()
    gp.scoreAllTime shouldBe Some(8)
  }

  test("Bi-directional sync score with better local score submits it to remote [score in the social collection]") {
    import com.google.android.gms.games.leaderboard.LeaderboardVariant.{COLLECTION_PUBLIC, COLLECTION_SOCIAL, TIME_SPAN_ALL_TIME}
    import org.mockito.Matchers.{eq => is}

    val waiter = mkWaiter()
    val gp = mkGameProgress_LB(waiter, StatusOkAndFirstGetScoreReturnsNullForTimeStampAlltime)
    val cbd = mock[CallbackDummy]

    // Set better local score than remote score of 7 (all three time spans).
    gp.updateScore(7)


    gp.syncScore(cbd.check())
    await(waiter, 5)

    // All three time spans should be tried.
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_WEEKLY), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_DAILY), is(COLLECTION_PUBLIC))

    // Only in time span all-time should the score be found in the social collection because, on the LoadPlayerScoreResult mock, the first call to getScore only then returns no value.
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, never).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_WEEKLY), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, never).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_DAILY), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, times(1)).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    verify(cbd, times(1)).check()
    gp.scoreAllTime shouldBe Some(7)
  }

  test("Bi-directional sync score with failing sync down does not submit to remote afterwards.") {
    import com.google.android.gms.games.leaderboard.LeaderboardVariant.{COLLECTION_PUBLIC, COLLECTION_SOCIAL, TIME_SPAN_ALL_TIME}
    import org.mockito.Matchers.{eq => is}

    val waiter = mkWaiter()
    val gp = mkGameProgress_LB(waiter, StatusOkThenError_50times)
    val cbd = mock[CallbackDummy]

    // Set better local score than remote score of 10 (all three time spans).
    gp.updateScore(8)

    // Score found in public leaderboard, no need so look in the social leaderboard.
    gp.syncScore(cbd.check())
    await(waiter, 1)
    verify(gp.leaderboardsApi, times(1)).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_PUBLIC))
    verify(gp.leaderboardsApi, never).loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), is(COLLECTION_SOCIAL))
    verify(gp.leaderboardsApi, never).submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())
    verify(cbd, never).check() // Should not continue after a network error
    gp.scoreAllTime shouldBe Some(8)
  }

}