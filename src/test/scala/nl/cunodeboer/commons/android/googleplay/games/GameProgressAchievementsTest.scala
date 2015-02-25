package nl.cunodeboer.commons.android.googleplay.games

import com.google.android.gms.common.api._
import com.google.android.gms.games.leaderboard.LeaderboardVariant.{TIME_SPAN_ALL_TIME, TIME_SPAN_DAILY, TIME_SPAN_WEEKLY}
import grizzled.slf4j.Logging
import nl.cunodeboer.commons.android.googleplay.games.AchievementResultType._
import nl.cunodeboer.commons.android.googleplay.games.FPR_Ach.MaxCallbackResponseTimeMillis
import nl.cunodeboer.commons.android.googleplay.games.Helpers.mkGameProgress_Ach
import org.joda.time.DateTime
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.concurrent.AsyncAssertions
import org.scalatest.concurrent.AsyncAssertions.{Waiter => Wtr}
import org.scalatest.mock.MockitoSugar
import org.scalatest.time.SpanSugar._
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}
import play.api.libs.json.Json

/* Test GameProgress with fake local time so test outcomes don't depend on when the tests are run. */
class TestGameProgress(apiClient: GoogleApiClient, lowerIsBetter: Boolean, initJSON: Option[String] = None, leaderBoardId: Option[String] = None) extends GameProgress(apiClient, lowerIsBetter, initJSON, leaderBoardId) {
  override def init() {
    val fakeNowTimeStamp = new DateTime(lbResetTimezone).withYear(2015).withWeekOfWeekyear(10).withDayOfWeek(3).withHourOfDay(12).getMillis
    val realStartTimestamp = System.currentTimeMillis()
    currentTimeMillis = () => fakeNowTimeStamp + (System.currentTimeMillis - realStartTimestamp)
  }
}

class GameProgressAchievementsTest extends FunSuite with Matchers with BeforeAndAfterEach with MockitoSugar with AsyncAssertions with Logging {

  final val AllThreeTimeSpans = Set(TIME_SPAN_ALL_TIME, TIME_SPAN_WEEKLY, TIME_SPAN_DAILY)

  implicit def intTimes(i: Int) = new {
    def times(fn: => Unit) = (1 to i) foreach (x => fn)
  }

  def fakeSubmitted(timeSpan: GooglePlayGamesProperty.Value)(at: Long)(implicit gp: GameProgress) {
    gp.updateTimestampModified(timeSpan, at - 100)
    gp.updatetimestampSubmitted(timeSpan, at)
  }

  test("Updated unlocked achievements are submitted and unchanged ones are skipped") {
    val waiter = new Wtr
    val gp = mkGameProgress_Ach(waiter)
    val achievementsApiMock = gp.achievementsApi

    // No changes yet.
    gp.syncUp()
    verify(achievementsApiMock, never).unlockImmediate(any[GoogleApiClient], anyString())

    gp.unlockAchievement("achievement X")
    gp.unlockAchievement("achievement Y")
    gp.unlockAchievement("achievement Z")
    Thread.sleep(10)

    gp.syncUp()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(3))
    verify(achievementsApiMock, times(3)).unlockImmediate(any[GoogleApiClient], anyString())

    // No changes.
    gp.syncUp()
    verify(achievementsApiMock, times(3)).unlockImmediate(any[GoogleApiClient], anyString())

    gp.unlockAchievement("achievement Q")
    Thread.sleep(10)

    // Another achievement was unlocked.
    gp.syncUp()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(1))
    verify(achievementsApiMock, times(4)).unlockImmediate(any[GoogleApiClient], anyString())

    gp.achievementsUnlockedCount shouldBe 4
    gp.achievementsUnlockedRemoteCount shouldBe 4
  }

  test("Updated incremental achievements are submitted and unchanged ones are skipped") {
    val waiter = new Wtr
    val gp = mkGameProgress_Ach(waiter, IncrementUnlock)
    val achievementsApiMock1 = gp.achievementsApi
    val achievement1 = "Achievement 1"

    // No changes yet.
    gp.syncIncrementalAchievementsUp()
    verify(achievementsApiMock1, never).incrementImmediate(any[GoogleApiClient], anyString(), anyInt())

    gp.incAchievement(achievement1, 1)
    Thread.sleep(10)

    gp.syncIncrementalAchievementsUp()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(1))
    verify(achievementsApiMock1, times(1)).incrementImmediate(any[GoogleApiClient], anyString(), anyInt())

    // Unchanged incremental achievement.
    gp.syncIncrementalAchievementsUp()
    verify(achievementsApiMock1, times(1)).incrementImmediate(any[GoogleApiClient], anyString(), anyInt())

    gp.incAchievement(achievement1, 7)
    Thread.sleep(10)

    gp.syncIncrementalAchievementsUp()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(1))
    verify(achievementsApiMock1, times(2)).incrementImmediate(any[GoogleApiClient], anyString(), anyInt())
  }

  test("Bi-directional sync of achievements works as expected") {
    val waiter = new Wtr
    val gp = mkGameProgress_Ach(waiter) // Contains 12 remote achievements
    val achievementsApiMock = gp.achievementsApi

    gp.unlockAchievement("achievement not remote 1")
    gp.unlockAchievement("achievement not remote 2")
    gp.unlockAchievement("achievement not remote 3")

    gp.syncAchievements()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(4))
    verify(achievementsApiMock, times(1)).load(any[GoogleApiClient], anyBoolean())
    verify(achievementsApiMock, times(3)).unlockImmediate(any[GoogleApiClient], anyString())
    gp.achievementsUnlockedCount shouldBe 15
    gp.achievementsUnlockedRemoteCount shouldBe 15
  }

  test("Syncing achievements down and then up works") {
    val waiter = new Wtr
    val gp = mkGameProgress_Ach(waiter) // Contains 12 remote achievements
    val achievementsApiMock = gp.achievementsApi

    gp.unlockAchievement("achievement not remote 1")
    gp.unlockAchievement("achievement not remote 2")
    gp.unlockAchievement("achievement not remote 3")

    gp.syncAchievementsDown()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(1))
    gp.achievementsUnlockedCount shouldBe 15
    gp.achievementsUnlockedRemoteCount shouldBe 12
    gp.syncAchievementsUp()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(3))
    verify(achievementsApiMock, times(1)).load(any[GoogleApiClient], anyBoolean())
    verify(achievementsApiMock, times(3)).unlockImmediate(any[GoogleApiClient], anyString())
    gp.achievementsUnlockedCount shouldBe 15
    gp.achievementsUnlockedRemoteCount shouldBe 15
  }

  test("Incrementing an unlocked achievements is ignored") {
    val gp = mkGameProgress_Ach()

    gp.unlockAchievement("Achievement X")
    gp.incAchievement("Achievement X", 1)
    gp.incrementalAchievementsAddToRemoteCount shouldBe 0
  }

  test("Unlocking the same achievement multiple times has the same effect as unlocking it one time when syncing up") {
    val waiter = new Wtr
    val gp = mkGameProgress_Ach(waiter)
    val achievementsApiMock = gp.achievementsApi

    gp.unlockAchievement("achievement X")
    gp.unlockAchievement("achievement X")
    gp.unlockAchievement("achievement X")
    gp.unlockAchievement("achievement X")

    gp.syncUp()
    waiter.await(timeout(MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(1))
    verify(achievementsApiMock, times(1)).unlockImmediate(any[GoogleApiClient], anyString())

    gp.achievementsUnlockedCount shouldBe 1
    gp.achievementsUnlockedRemoteCount shouldBe 1
  }

  test("Submitting 100 achievements to Google Play Games are all confirmed") {
    val count = 100
    val waiter = new Wtr
    val gp = mkGameProgress_Ach(waiter)
    val achievementsApiMock = gp.achievementsApi

    for (n <- 1 to count) {
      val a = s"achievement $n"
      gp.unlockAchievement(a)
      gp.isUnlocked(a) shouldBe true
      gp.isUnlockedRemote(a) shouldBe false
    }

    withClue("locally stored achievements count of") {
      gp.achievementsUnlockedCount shouldBe count
    }

    withClue("achievements initially at Google Play Games Services count of") {
      gp.achievementsUnlockedRemoteCount shouldBe 0
    }

    gp.syncUp()
    waiter.await(timeout(count * MaxCallbackResponseTimeMillis millis), org.scalatest.concurrent.AsyncAssertions.dismissals(count))
    verify(achievementsApiMock, times(count)).unlockImmediate(any[GoogleApiClient], anyString())

    for (n <- 1 to count) {
      val a = s"achievement $n"
      gp.isUnlocked(a) shouldBe true
      gp.isUnlockedRemote(a) shouldBe true
    }

    withClue("achievements unlockedRemoteCount at Google Play Games count of") {
      gp.achievementsUnlockedRemoteCount shouldBe count
    }
  }

  test("When synchronizing 100 achievements of which 50 fail, then the GameProgress object reflects that") {
    val waiter = new Wtr
    val count = 100
    val gp = mkGameProgress_Ach(waiter, Errors)
    val achievementsApiMock = gp.achievementsApi

    for (n <- 1 to count) gp.unlockAchievement(s"achievement $n")

    withClue("locally stored achievements count of") {
      gp.achievementsUnlockedCount shouldBe count
    }

    withClue("achievements initially at Google Play Games Services count of") {
      gp.achievementsUnlockedRemoteCount shouldBe 0
    }

    gp.syncUp()
    waiter.await(timeout(count * MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(count))
    verify(achievementsApiMock, times(count)).unlockImmediate(any[GoogleApiClient], anyString())

    var isUnlockedLocallyOnlyCount = 0
    for (n <- 1 to count) {
      val a = s"achievement $n"
      if (gp.isUnlocked(a) && !gp.isUnlockedRemote(a)) isUnlockedLocallyOnlyCount += 1
    }

    withClue("Achievements unlocked locally only count of") {
      isUnlockedLocallyOnlyCount shouldBe 50
    }

    withClue("Out of 100 achievements at Google Play Games Services the unlocked count of") {
      gp.achievementsUnlockedRemoteCount shouldBe 50
    }
  }

  test("Incrementing 5 achievements that unlock at count 10 with some failures works as expected") {
    val waiter = new Wtr
    val gp = mkGameProgress_Ach(waiter, IncrementUnlock)
    val achievementsApiMock1 = gp.achievementsApi

    gp.unlockAchievement("Achievement DU1")

    // 10 => unlock @ GP with success response
    val achievementIU1 = "Achievement IU1"
    gp.incAchievement(achievementIU1, 1)
    gp.incAchievement(achievementIU1, 2)
    gp.incAchievement(achievementIU1, 3)
    gp.incAchievement(achievementIU1, 4)

    gp.incrementalAchievementsAddToRemoteCount shouldBe 1
    gp.incrementalAchievementsAddToRemote(achievementIU1) shouldBe 10

    // 5 => increment @ GP with error response
    val achievementE1 = "Achievement E1"
    gp.incAchievement(achievementE1, 5)

    gp.unlockAchievement("Achievement DU2")

    gp.incrementalAchievementsAddToRemoteCount shouldBe 2
    gp.incrementalAchievementsAddToRemote(achievementE1) shouldBe 5

    // 9 => increment @ GP with success response
    val achievementI2 = "Achievement I2"
    gp.incAchievement(achievementI2, 2)
    gp.incAchievement(achievementI2, 3)
    gp.incAchievement(achievementI2, 4)

    gp.incrementalAchievementsAddToRemoteCount shouldBe 3
    gp.incrementalAchievementsAddToRemote(achievementI2) shouldBe 9

    // 12 => unlock @ GP with error response
    val achievementE2 = "Achievement E2"
    gp.incAchievement(achievementE2, 5)
    gp.incAchievement(achievementE2, 7)

    gp.incrementalAchievementsAddToRemoteCount shouldBe 4
    gp.incrementalAchievementsAddToRemote(achievementE2) shouldBe 12

    // 15 => unlock @ GP with success response
    val achievementIU2 = "Achievement IU2"
    gp.incAchievement(achievementIU2, 1)
    gp.incAchievement(achievementIU2, 2)
    gp.incAchievement(achievementIU2, 3)
    gp.incAchievement(achievementIU2, 4)
    gp.incAchievement(achievementIU2, 5)

    gp.unlockAchievement("Achievement DU3")

    gp.incrementalAchievementsAddToRemote(achievementIU2) shouldBe 15
    gp.incrementalAchievementsAddToRemoteCount shouldBe 5

    withClue("Achievements at Google Play Games Services before syncing count of") {
      gp.achievementsUnlockedRemoteCount shouldBe 0
    }

    gp.syncUp()
    waiter.await(timeout(15 * MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(8))
    verify(achievementsApiMock1, times(3)).unlockImmediate(any[GoogleApiClient], anyString())
    verify(achievementsApiMock1, times(5)).incrementImmediate(any[GoogleApiClient], anyString(), anyInt())

    debug(Json.prettyPrint(gp.toJSON))

    withClue("[additions count must be equal to the amount of failures while synchronizing to Google Play Games Services]") {
      gp.incrementalAchievementsAddToRemoteCount shouldBe 2
    }

    withClue("unlocked achievements at Google Play Games Services count of") {
      gp.achievementsUnlockedRemoteCount shouldBe 5
    }

    gp.achievementsUnlockedRemote should contain(achievementIU1)
    gp.achievementsUnlockedRemote should contain(achievementIU2)
    gp.achievementsUnlockedRemote should not contain achievementI2

    withClue("unlocked achievements count of") {
      gp.achievementsUnlockedCount shouldBe 5
    }

    gp.achievementsUnlocked should contain(achievementIU1)
    gp.achievementsUnlocked should contain(achievementIU2)
    gp.achievementsUnlocked should not contain achievementI2

    gp.incrementalAchievementsAddToRemote should contain key achievementE1
    gp.incrementalAchievementsAddToRemote should contain key achievementE2
    gp.achievementsUnlockedRemote should not contain achievementE1
    gp.achievementsUnlockedRemote should not contain achievementE2
  }

}