package nl.cunodeboer.commons.android.googleplay.games

import java.util.concurrent.TimeUnit

import android.database.CharArrayBuffer
import android.net.Uri
import android.os.Parcel
import com.google.android.gms.common.api.PendingResult.BatchCallback
import com.google.android.gms.common.api._
import com.google.android.gms.games.GamesStatusCodes._
import com.google.android.gms.games.Player
import com.google.android.gms.games.achievement.Achievement._
import com.google.android.gms.games.achievement.Achievements.{LoadAchievementsResult, UpdateAchievementResult}
import com.google.android.gms.games.achievement.{Achievement, AchievementBuffer, Achievements}
import com.google.android.gms.games.leaderboard.LeaderboardVariant._
import com.google.android.gms.games.leaderboard.Leaderboards.{LoadPlayerScoreResult, SubmitScoreResult}
import com.google.android.gms.games.leaderboard.{LeaderboardScore, Leaderboards}
import grizzled.slf4j.Logging
import org.joda.time.DateTime
import org.mockito.AdditionalMatchers.not
import org.mockito.Matchers.{eq => is, _}
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.scalatest.concurrent.AsyncAssertions.Waiter
import org.scalatest.mock

object Globals {
  val rnd = new scala.util.Random
  val MaxCallbackResponseTimeMillis = 500
}

object Utils {
  def fakeSubmitted(timeSpan: GooglePlayGamesProperty.Value)(at: Long)(implicit gp: GameProgress) {
    gp.updateTimestampModified(timeSpan, at - 100)
    gp.updateTimestampSubmitted(timeSpan, at)
  }
}

object AchievementResultType extends Enumeration {
  type AchievementResultType = Value
  val StatusOk, StatusOkAndFirstGetScoreReturnsNullForTimeStampAlltime, StatusOkThenError_50times, MissingID, IncrementUnlock = Value
}

object MeasureType extends Enumeration {
  type MeasureType = Value
  val LowerIsBetter, HigherIsBetter = Value
}

import MeasureType._

/** GameProgress with fixed fake local starting time so test outcomes don't depend on when they are run. */
class TestGameProgressFixedStartDateTime(apiClient: GoogleApiClient, measureType: MeasureType, initJSON: Option[String] = None, leaderBoardId: Option[String] = None) extends GameProgress(apiClient, measureType == LowerIsBetter, initJSON, leaderBoardId) {
  override def init() {
    val fakeNowTimeStamp = new DateTime(lbResetTimezone).withYear(2015).withWeekOfWeekyear(10).withDayOfWeek(3).withHourOfDay(12).getMillis
    val realStartTimestamp = System.currentTimeMillis()
    currentTimeMillis = () => fakeNowTimeStamp + (System.currentTimeMillis - realStartTimestamp)
  }
}

/** Fake PendingResult[Leaderboards.SubmitScoreResult] */
class FPR_LB_Submit(mockedUpdateSubmitScoreResult: Leaderboards.SubmitScoreResult, waiter: Waiter) extends PendingResult[Leaderboards.SubmitScoreResult] {
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

object FPR_LB_Submit {
  def apply(mockedSubmitScoreResultResult: Leaderboards.SubmitScoreResult, waiter: Waiter) = new FPR_LB_Submit(mockedSubmitScoreResultResult, waiter)
}

/** Fake PendingResult[Leaderboards.LoadPlayerScoreResult] */
class FPR_LB_Load(mockedLoadPlayerScoreResult: Leaderboards.LoadPlayerScoreResult, waiter: Waiter) extends PendingResult[Leaderboards.LoadPlayerScoreResult] {

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

object FPR_LB_Load {
  def apply(mockedLoadPlayerScoreResult: Leaderboards.LoadPlayerScoreResult, waiter: Waiter) = new FPR_LB_Load(mockedLoadPlayerScoreResult, waiter)
}

/** Fake PendingResult[Achievements.UpdateAchievementResult] */
class FPR_Ach[R <: Result](mockedUpdateAchievementResult: R, waiter: Waiter) extends PendingResult[R] {

  import nl.cunodeboer.commons.android.googleplay.games.Globals._

  override def await(): R = ???

  override def await(p1: Long, p2: TimeUnit): R = ???

  override def cancel(): Unit = ???

  override def isCanceled: Boolean = ???

  override def addBatchCallback(batchCallback: BatchCallback): Unit = ???

  override def setResultCallback(resultCallback: ResultCallback[R]) = {
    new Thread(new Runnable {
      def run() {
        val millis = rnd.nextInt(MaxCallbackResponseTimeMillis + 1) + 1
        Thread.sleep(millis)
        resultCallback.onResult(mockedUpdateAchievementResult)
        waiter.dismiss()
      }
    }).start()
  }

  override def setResultCallback(resultCallback: ResultCallback[R], p2: Long, p3: TimeUnit): Unit = ???

}

object FPR_Ach {
  val MaxCallbackResponseTimeMillis = 500

  def apply[R <: Result](mockedUpdateAchievementResult: R, waiter: Waiter) = new FPR_Ach(mockedUpdateAchievementResult, waiter)
}

object Helpers extends mock.MockitoSugar with Logging {

  import nl.cunodeboer.commons.android.googleplay.games.AchievementResultType._

  /**
   * Fixtures for Achievement object.
   **/
  def mkAchievement(id: String, atype: Int, astate: Int) = {
    new Achievement {
      override def getAchievementId = id

      override def getType: Int = atype

      override def getTotalSteps: Int = ???

      override def getUnlockedImageUri: Uri = ???

      override def getUnlockedImageUrl: String = ???

      override def getPlayer: Player = ???

      override def getRevealedImageUrl: String = ???

      override def getName: String = ???

      override def getName(p1: CharArrayBuffer): Unit = ???

      override def getRevealedImageUri: Uri = ???

      override def getCurrentSteps: Int = ???

      override def getDescription: String = ???

      override def getDescription(p1: CharArrayBuffer): Unit = ???

      override def getFormattedCurrentSteps: String = ???

      override def getFormattedCurrentSteps(p1: CharArrayBuffer): Unit = ???

      override def getFormattedTotalSteps: String = ???

      override def getFormattedTotalSteps(p1: CharArrayBuffer): Unit = ???

      override def getLastUpdatedTimestamp: Long = ???

      override def getXpValue: Long = ???

      override def getState: Int = astate

      override def writeToParcel(dest: Parcel, flags: Int): Unit = ???

      override def describeContents(): Int = ???

      override def isDataValid: Boolean = ???

      override def freeze(): Achievement = ???
    }

  }

  /**
   * Fixtures for Leaderboards API.
   *
   * @return a new TestGameProgress object with mocked members.
   */
  def mkGameProgress_LB(waiter: Waiter = null, resultType: AchievementResultType = StatusOk, measureType: MeasureType = LowerIsBetter) = {
    val apiStatusOk = new com.google.android.gms.common.api.Status(STATUS_OK)
    val statusError = new com.google.android.gms.common.api.Status(STATUS_NETWORK_ERROR_OPERATION_FAILED)

    val mockedGoogleApiClient = mock[GoogleApiClient]
    val mockedLeaderboardApi = mock[Leaderboards]
    val mockedSubmitScoreResult = mock[SubmitScoreResult]

    val mockedLeaderboardScore10 = mock[LeaderboardScore]
    when(mockedLeaderboardScore10.getRawScore).thenReturn(10)

    val mockedLoadPlayerScoreResultOkFirstScoreNullRest10 = mock[LoadPlayerScoreResult]
    val mockedLoadPlayerScoreResultOkScore10 = mock[LoadPlayerScoreResult]

    when(mockedLoadPlayerScoreResultOkScore10.getStatus).thenReturn(apiStatusOk)
    when(mockedLoadPlayerScoreResultOkScore10.getScore).thenReturn(mockedLeaderboardScore10)

    when(mockedLoadPlayerScoreResultOkFirstScoreNullRest10.getStatus).thenReturn(apiStatusOk)
    when(mockedLoadPlayerScoreResultOkFirstScoreNullRest10.getScore).thenReturn(null, mockedLeaderboardScore10)

    val mockedLoadPlayerScoreResultError = mock[LoadPlayerScoreResult]
    when(mockedLoadPlayerScoreResultError.getStatus).thenReturn(statusError)

    when(mockedLeaderboardApi.submitScoreImmediate(any[GoogleApiClient], anyString(), anyLong())).thenReturn(FPR_LB_Submit(mockedSubmitScoreResult, waiter))

    var gp: GameProgress = null

    resultType match {
      case StatusOk =>
        when(mockedSubmitScoreResult.getStatus).thenReturn(apiStatusOk)
        when(mockedLeaderboardApi.loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), anyInt(), anyInt())).thenReturn(FPR_LB_Load(mockedLoadPlayerScoreResultOkScore10, waiter))
        gp = new TestGameProgressFixedStartDateTime(mockedGoogleApiClient, measureType, None, Some("dummyLeaderboardId"))
        gp.leaderboardsApi = mockedLeaderboardApi
      case StatusOkAndFirstGetScoreReturnsNullForTimeStampAlltime =>
        when(mockedSubmitScoreResult.getStatus).thenReturn(apiStatusOk)
        when(mockedLeaderboardApi.loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), is(TIME_SPAN_ALL_TIME), anyInt())).thenReturn(FPR_LB_Load(mockedLoadPlayerScoreResultOkFirstScoreNullRest10, waiter))
        when(mockedLeaderboardApi.loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), not(is(TIME_SPAN_ALL_TIME)), anyInt())).thenReturn(FPR_LB_Load(mockedLoadPlayerScoreResultOkScore10, waiter))
        gp = new TestGameProgressFixedStartDateTime(mockedGoogleApiClient, measureType, None, Some("dummyLeaderboardId"))
        gp.leaderboardsApi = mockedLeaderboardApi
      case MissingID =>
        when(mockedSubmitScoreResult.getStatus).thenReturn(apiStatusOk)
        gp = new TestGameProgressFixedStartDateTime(mockedGoogleApiClient, measureType) // Leaderboard ID is missing
        gp.leaderboardsApi = mockedLeaderboardApi
      case StatusOkThenError_50times =>
        when(mockedSubmitScoreResult.getStatus).thenReturn(statusError, apiStatusOk)
        when(mockedLeaderboardApi.loadCurrentPlayerLeaderboardScore(any[GoogleApiClient], anyString(), anyInt(), anyInt())).thenReturn(FPR_LB_Load(mockedLoadPlayerScoreResultError, waiter))
        gp = new TestGameProgressFixedStartDateTime(mockedGoogleApiClient, measureType, None, Some("dummyLeaderboardId"))
        gp.leaderboardsApi = mockedLeaderboardApi
    }

    gp
  }

  /**
   * Fixtures for Achievements API.
   *
   * @return a new TestGameProgress object with mocked members.
   */
  def mkGameProgress_Ach(waiter: Waiter = null, resultType: AchievementResultType = StatusOk, measureType: MeasureType = LowerIsBetter) = {
    val gpWithLeaderBoardApi = mkGameProgress_LB(waiter) // Leaderboard API should behave normal, not testing it here.

    val mockedUpdateAchievementResultERROR = mock[UpdateAchievementResult]
    when(mockedUpdateAchievementResultERROR.getStatus).thenReturn(new Status(STATUS_NETWORK_ERROR_OPERATION_FAILED))

    val mockedUpdateAchievementResultOK = mock[UpdateAchievementResult]
    when(mockedUpdateAchievementResultOK.getStatus).thenReturn(new Status(STATUS_OK))

    val mockedReplacedClassForAchievementBuffer = mock[AchievementBuffer]
    val achievements = new java.util.ArrayList[Achievement]
    achievements.add(mkAchievement("CgkI_oPj6N8IEAIQDw", TYPE_STANDARD, STATE_UNLOCKED))
    achievements.add(mkAchievement("CgkI_oPj6N8IEAIQCA", TYPE_INCREMENTAL, STATE_UNLOCKED))
    achievements.add(mkAchievement("CgkI_oPj6N8IEAIQCQ", TYPE_INCREMENTAL, STATE_UNLOCKED))
    achievements.add(mkAchievement("CgkI_oPj6N8IEAIQCg", TYPE_INCREMENTAL, STATE_UNLOCKED))
    achievements.add(mkAchievement("CgkI_oPj6N8IEAIQCw", TYPE_INCREMENTAL, STATE_UNLOCKED))
    achievements.add(mkAchievement("CgkI_oPj6N8IEAIQDA", TYPE_INCREMENTAL, STATE_UNLOCKED))
    achievements.add(mkAchievement("CgkI_oPj6N8IEAIQDg", TYPE_STANDARD, STATE_UNLOCKED))
    achievements.add(mkAchievement("CgkI_oPj6N8IEAIQAw", TYPE_STANDARD, STATE_UNLOCKED))
    achievements.add(mkAchievement("CgkI_oPj6N8IEAIQBA", TYPE_STANDARD, STATE_UNLOCKED))
    achievements.add(mkAchievement("CgkI_oPj6N8IEAIQBQ", TYPE_STANDARD, STATE_UNLOCKED))
    achievements.add(mkAchievement("CgkI_oPj6N8IEAIQBg", TYPE_STANDARD, STATE_UNLOCKED))
    achievements.add(mkAchievement("CgkI_oPj6N8IEAIQBw", TYPE_STANDARD, STATE_UNLOCKED))
    when(mockedReplacedClassForAchievementBuffer.iterator).thenReturn(achievements.iterator)

    val mockedLoadAchievementResultOK = mock[LoadAchievementsResult]
    when(mockedLoadAchievementResultOK.getStatus).thenReturn(new Status(STATUS_OK))
    when(mockedLoadAchievementResultOK.getAchievements).thenReturn(
      mockedReplacedClassForAchievementBuffer
    )

    val mockedUpdateAchievementResultIncrementedToUnlocked = mock[UpdateAchievementResult]
    when(mockedUpdateAchievementResultIncrementedToUnlocked.getStatus).thenReturn(new com.google.android.gms.common.api.Status(STATUS_ACHIEVEMENT_UNLOCKED))

    val gp = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], measureType, None, Some("dummyLeaderboardId"))
    resultType match {
      case StatusOk =>
        val mockedAchievementsApiOK = mock[Achievements]

        when(mockedAchievementsApiOK.unlockImmediate(any[GoogleApiClient], anyString())).thenReturn(FPR_Ach(mockedUpdateAchievementResultOK, waiter))
        when(mockedAchievementsApiOK.load(any[GoogleApiClient], anyBoolean())).thenReturn(FPR_Ach(mockedLoadAchievementResultOK, waiter))

        gp.leaderboardsApi = gpWithLeaderBoardApi.leaderboardsApi
        gp.achievementsApi = mockedAchievementsApiOK
        gp
      case IncrementUnlock =>
        val mockedAchievementsApiIncToUnlock = mock[Achievements](s"Mock for incremental achievements that unlock at count 10, returning error status when passing addTotal == 5 and addTotal == 12")

        // Support non-incremental unlocking
        when(mockedAchievementsApiIncToUnlock.unlockImmediate(any[GoogleApiClient], anyString())).thenReturn(FPR_Ach(mockedUpdateAchievementResultOK, waiter))
        when(mockedAchievementsApiIncToUnlock.load(any[GoogleApiClient], anyBoolean())).thenReturn(FPR_Ach(mockedLoadAchievementResultOK, waiter))

        // Support incremental unlocking and fail at predetermined counts.
        when(mockedAchievementsApiIncToUnlock.incrementImmediate(any[GoogleApiClient], anyString(), anyInt())).thenAnswer(new Answer[PendingResult[Achievements.UpdateAchievementResult]] {
          override def answer(invocation: InvocationOnMock): PendingResult[Achievements.UpdateAchievementResult] = {
            val addTotal = invocation.getArgumentAt(2, classOf[Int]) // Note: addTotal is assumed to be the only addition, initial count 0
            debug(s">>> addCount is $addTotal")
            if (addTotal == 5 || addTotal == 12) {
              debug(s">>> will return error status")
              FPR_Ach(mockedUpdateAchievementResultERROR, waiter)
            } else
            if (addTotal < 10) FPR_Ach(mockedUpdateAchievementResultOK, waiter) else FPR_Ach(mockedUpdateAchievementResultIncrementedToUnlocked, waiter)
          }
        })

        gp.leaderboardsApi = gpWithLeaderBoardApi.leaderboardsApi
        gp.achievementsApi = mockedAchievementsApiIncToUnlock
        gp
      case StatusOkThenError_50times =>
        val count = 50
        val pair = List(mockedUpdateAchievementResultOK, mockedUpdateAchievementResultERROR)
        val l = new scala.collection.mutable.MutableList[Achievements.UpdateAchievementResult]
        for (n <- 1 to count) l ++= pair
        val mockedAchievementsApiFAIL = mock[Achievements]("Mock for Achievements returning only error statuses")
        when(mockedAchievementsApiFAIL.unlockImmediate(any[GoogleApiClient], anyString())).
          thenReturn(
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter),
            FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter), FPR_Ach(mockedUpdateAchievementResultOK, waiter), FPR_Ach(mockedUpdateAchievementResultERROR, waiter)
          )
        gpWithLeaderBoardApi.leaderboardsApi
        gp.achievementsApi = mockedAchievementsApiFAIL
        gp
    }
  }
}