package nl.cunodeboer.commons.android.googleplay.games

import com.google.android.gms.common.api._
import grizzled.slf4j.Logging
import org.joda.time.DateTime
import org.scalatest.concurrent.AsyncAssertions
import org.scalatest.mock.MockitoSugar
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}

/* Test GameProgress with fake local time set to half a second before the end of a week: 24:00 PST. */
class TestGameProgressEndOfWeek(apiClient: GoogleApiClient, lowerIsBetter: Boolean, initJSON: Option[String] = None, leaderBoardId: Option[String] = None) extends GameProgress(apiClient, lowerIsBetter, initJSON, leaderBoardId) {
  val lastSecondOfSundayTimestamp = new DateTime(lbResetTimezone).withWeekOfWeekyear(6).withDayOfWeek(7).withHourOfDay(23).withMinuteOfHour(59).withSecondOfMinute(59).withMillisOfSecond(500).getMillis

  override def init() {
    val realStartTimestamp = System.currentTimeMillis()
    currentTimeMillis = () => lastSecondOfSundayTimestamp + (System.currentTimeMillis - realStartTimestamp)
  }
}

/* Test GameProgress with fake local time set to half a second before the end of a wednesday: 24:00 PST. */
class TestGameProgressEndOfWednesday(apiClient: GoogleApiClient, lowerIsBetter: Boolean, initJSON: Option[String] = None, leaderBoardId: Option[String] = None) extends GameProgress(apiClient, lowerIsBetter, initJSON, leaderBoardId) {
  val lastSecondOfSundayTimestamp = new DateTime(lbResetTimezone).withWeekOfWeekyear(6).withDayOfWeek(3).withHourOfDay(23).withMinuteOfHour(59).withSecondOfMinute(59).withMillisOfSecond(500).getMillis

  override def init() {
    val realStartTimestamp = System.currentTimeMillis()
    currentTimeMillis = () => lastSecondOfSundayTimestamp + (System.currentTimeMillis - realStartTimestamp)
  }
}

class GameProgressTimeSpansTest extends FunSuite with Matchers with BeforeAndAfterEach with MockitoSugar with AsyncAssertions with Logging {

  /**
   * @return a new GameProgress object with faked local time set to half a second before the end of a week: 24:00 PST
   */
  def mkGameProgressWithEndOfWeek() = new TestGameProgressEndOfWeek(null, true, None, Some("dummyLeaderboardId"))

  /**
   * @return a new GameProgress object with faked local time set to half a second before the end of a week: 24:00 PST
   */
  def mkGameProgressWithEndOfWednesday() = new TestGameProgressEndOfWednesday(null, true, None, Some("dummyLeaderboardId"))

  def blockFor510Millis() = Thread.sleep(510)

  test("isNotOnThisDay method works") {
    val gp = mkGameProgressWithEndOfWednesday()
    import gp.TimeStampUtils
    blockFor510Millis()
    (Some(gp.lastSecondOfSundayTimestamp) isNotOnThisDay) shouldBe true
    (Some(gp.lastSecondOfSundayTimestamp) isNotInThisWeek) shouldBe false
  }

  test("isNotInThisWeek method works") {
    val gp = mkGameProgressWithEndOfWeek()
    import gp.TimeStampUtils
    blockFor510Millis()
    (Some(gp.lastSecondOfSundayTimestamp) isNotInThisWeek) shouldBe true
    (Some(gp.lastSecondOfSundayTimestamp) isNotOnThisDay) shouldBe true
  }

  test("scoreDaily resets the daily leaderboard at 24:00 PST") {
    val gp = mkGameProgressWithEndOfWednesday()
    gp.updateScore(1000)
    gp.scoreDaily should not be empty
    blockFor510Millis()
    gp.scoreDaily shouldBe empty
  }

  test("previousScoreDaily resets the daily leaderboard at 24:00 PST") {
    val gp = mkGameProgressWithEndOfWednesday()
    gp.updateScore(1000)
    gp.previousScoreDaily shouldBe empty
    blockFor510Millis()
    gp.previousScoreDaily shouldBe Some(1000)
  }

  test("scoreWeekly resets the weekly leaderboard at 24:00 PST on Sunday") {
    val gp = mkGameProgressWithEndOfWeek()
    gp.updateScore(1000)
    gp.scoreWeekly should not be empty
    blockFor510Millis()
    gp.scoreWeekly shouldBe empty
    gp.scoreWeekly shouldBe empty
  }

  test("previousScoreWeekly resets the weekly leaderboard at 24:00 PST on Sunday") {
    val gp = mkGameProgressWithEndOfWeek()
    gp.updateScore(1000)
    gp.previousScoreWeekly shouldBe empty
    blockFor510Millis()
    gp.previousScoreWeekly shouldBe Some(1000)
  }

}