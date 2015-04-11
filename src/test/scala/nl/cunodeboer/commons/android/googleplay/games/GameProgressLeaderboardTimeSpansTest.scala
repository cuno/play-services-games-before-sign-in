package nl.cunodeboer.commons.android.googleplay.games

import com.google.android.gms.common.api._
import com.google.android.gms.games.leaderboard.LeaderboardVariant._
import grizzled.slf4j.Logging
import nl.cunodeboer.commons.android.googleplay.games.AchievementResultType._
import nl.cunodeboer.commons.android.googleplay.games.GooglePlayGamesProperty._
import nl.cunodeboer.commons.android.googleplay.games.Helpers._
import nl.cunodeboer.commons.android.googleplay.games.MeasureType._
import org.joda.time.DateTime
import org.scalatest.concurrent.AsyncAssertions
import org.scalatest.mock.MockitoSugar
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}

/* Test GameProgress with fake local time set to half a second before the end of a Saturday: 24:00 PST. */
class TestGameProgressEndOfSaturday(apiClient: GoogleApiClient, lowerIsBetter: Boolean, initJSON: Option[String] = None, leaderBoardId: Option[String] = None) extends GameProgress(apiClient, lowerIsBetter, initJSON, leaderBoardId) {
  val lastSecondOfSaturdayTimestamp = new DateTime(lbResetTimezone).withWeekOfWeekyear(6).withDayOfWeek(6).withHourOfDay(23).withMinuteOfHour(59).withSecondOfMinute(59).withMillisOfSecond(500).getMillis

  override def init() {
    val realStartTimestamp = System.currentTimeMillis()
    currentTimeMillis = () => lastSecondOfSaturdayTimestamp + (System.currentTimeMillis - realStartTimestamp)
  }
}

/* Test GameProgress with fake local time set to half a second before the end of a wednesday: 24:00 PST. */
class TestGameProgressEndOfWednesday(apiClient: GoogleApiClient, lowerIsBetter: Boolean, initJSON: Option[String] = None, leaderBoardId: Option[String] = None) extends GameProgress(apiClient, lowerIsBetter, initJSON, leaderBoardId) {
  val lastSecondOfWednesdayTimestamp = new DateTime(lbResetTimezone).withWeekOfWeekyear(6).withDayOfWeek(3).withHourOfDay(23).withMinuteOfHour(59).withSecondOfMinute(59).withMillisOfSecond(500).getMillis

  override def init() {
    val realStartTimestamp = System.currentTimeMillis()
    currentTimeMillis = () => lastSecondOfWednesdayTimestamp + (System.currentTimeMillis - realStartTimestamp)
  }
}

class GameProgressTimeSpansTest extends FunSuite with Matchers with BeforeAndAfterEach with MockitoSugar with AsyncAssertions with Logging {

  final val AllThreeTimeSpans = Set(TIME_SPAN_ALL_TIME, TIME_SPAN_WEEKLY, TIME_SPAN_DAILY)
  final val DailyAndWeeklyTimeSpans = Set(TIME_SPAN_WEEKLY, TIME_SPAN_DAILY)
  final val DailyTimeSpan = Set(TIME_SPAN_DAILY)

  /**
   * @return a new GameProgress object with faked local time set to half a second before the end of a week: 24:00 PST
   */
  def mkGameProgressWithEndOfWeek() = new TestGameProgressEndOfSaturday(null, true, None, Some("dummyLeaderboardId"))

  /**
   * @return a new GameProgress object with faked local time set to half a second before the end of a week: 24:00 PST
   */
  def mkGameProgressWithEndOfWednesday() = new TestGameProgressEndOfWednesday(null, true, None, Some("dummyLeaderboardId"))

  def blockFor510Millis() = Thread.sleep(510)

  test("isNotInDailyTimespan method of implicit class TimeStampUtils works ") {
    val gp = mkGameProgressWithEndOfWednesday()
    import gp.TimeStampUtils
    blockFor510Millis()
    (Some(gp.lastSecondOfWednesdayTimestamp) isNotInDailyTimespan) shouldBe true
    (Some(gp.lastSecondOfWednesdayTimestamp) isNotInWeeklyTimespan) shouldBe false
  }

  test("isNotInWeeklyTimespan method of implicit class TimeStampUtils works") {
    val gp = mkGameProgressWithEndOfWeek()
    import gp.TimeStampUtils
    blockFor510Millis()
    (Some(gp.lastSecondOfSaturdayTimestamp) isNotInWeeklyTimespan) shouldBe true
    (Some(gp.lastSecondOfSaturdayTimestamp) isNotInDailyTimespan) shouldBe true
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

  test("scoreWeekly resets the weekly leaderboard at 24:00 PST on Saturday") {
    val gp = mkGameProgressWithEndOfWeek()
    gp.updateScore(1000)
    gp.scoreWeekly should not be empty
    blockFor510Millis()
    gp.scoreWeekly shouldBe empty
    gp.scoreWeekly shouldBe empty
  }

  test("previousScoreWeekly resets the weekly leaderboard at 24:00 PST on Saturday") {
    val gp = mkGameProgressWithEndOfWeek()
    gp.updateScore(1000)
    gp.previousScoreWeekly shouldBe empty
    blockFor510Millis()
    gp.previousScoreWeekly shouldBe Some(1000)
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
    val gp = mkGameProgress_LB(null, StatusOk)
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
    val gp = mkGameProgress_LB(null, StatusOk, HigherIsBetter)
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
    val gpL = mkGameProgress_LB(null, StatusOk, HigherIsBetter)
    val gpS = mkGameProgress_LB(null, StatusOk, LowerIsBetter)
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

  test("Updating a score to a value that is only better in case of a time span reset, updates the score of that time span and also of more narrow time spans") {
    val gp = mkGameProgress_LB(null, StatusOk, HigherIsBetter)
    val allTimeBestScore = Some(2000)
    val dailyBestScore = Some(1500)
    val weeklyBestScore = Some(1750)

    val dt = new DateTime(gp.lbResetTimezone).withMillis(gp.currentTimeMillis())

    gp.updateScore(allTimeBestScore.get)

    // Same day, so no reset and update of best weekly and daily score.
    gp.updateScore(dailyBestScore.get) shouldBe empty
    gp.scoreAllTime shouldBe allTimeBestScore
    gp.scoreWeekly shouldBe allTimeBestScore
    gp.scoreDaily shouldBe allTimeBestScore
    gp.updateScore(weeklyBestScore.get) shouldBe empty
    gp.scoreAllTime shouldBe allTimeBestScore
    gp.scoreWeekly shouldBe allTimeBestScore
    gp.scoreDaily shouldBe allTimeBestScore

    // The daily score, initially set to the same value as the all-time score, is now more than a day old. The all-time
    // best score does no longer count as best of the day, it still stands as best of the week though.
    gp.updateTimestampModified(ScoreDaily, dt.minusHours(25).getMillis)
    gp.updateScore(dailyBestScore.get) shouldBe DailyTimeSpan

    gp.scoreAllTime shouldBe allTimeBestScore
    gp.scoreWeekly shouldBe allTimeBestScore
    gp.scoreDaily shouldBe dailyBestScore
    gp.previousScoreAllTime shouldBe empty // Not set yet.
    gp.previousScoreWeekly shouldBe empty // Not set yet.
    gp.previousScoreDaily shouldBe empty // Reset

    gp.updateTimestampModified(ScoreDaily, dt.getMillis)

    // Same day, so no change in best weekly score because it is still set to a better value. But the daily score should
    // be updated to the better score.
    gp.updateScore(weeklyBestScore.get) shouldBe DailyTimeSpan
    gp.scoreAllTime shouldBe allTimeBestScore
    gp.scoreWeekly shouldBe allTimeBestScore
    gp.scoreDaily shouldBe weeklyBestScore

    // The weekly score was set in the previous week, so the all-time best value does not count as best of the week any
    // longer and should therefore be updated to the worse new score.
    gp.updateTimestampModified(ScoreDaily, dt.minusDays(8).getMillis)
    gp.updateTimestampModified(ScoreWeekly, dt.minusDays(8).getMillis)
    gp.updateScore(weeklyBestScore.get) shouldBe DailyAndWeeklyTimeSpans
    gp.scoreAllTime shouldBe allTimeBestScore
    gp.scoreWeekly shouldBe weeklyBestScore
    gp.scoreDaily shouldBe weeklyBestScore
    gp.previousScoreAllTime shouldBe empty // Not set yet.
    gp.previousScoreWeekly shouldBe empty // Reset.
    gp.previousScoreDaily shouldBe empty // Reset.

    gp.updateTimestampModified(ScoreWeekly, dt.getMillis)

    // Update the daily score to a score that is worse than the best score of that week.
    gp.updateTimestampModified(ScoreDaily, dt.minusHours(25).getMillis)
    gp.updateScore(weeklyBestScore.get - 10) shouldBe DailyTimeSpan
    gp.scoreDaily shouldBe Some(weeklyBestScore.get - 10)
    gp.previousScoreDaily shouldBe empty // Reset.
  }

  test("Updating to a score to a value that is only better as a daily score, updates the previous daily score") {
    val gp = mkGameProgress_LB(null, StatusOk, HigherIsBetter)
    val allTimeBestScore = Some(2000)
    val dailyBestScore = Some(1500)
    val betterDailyBestScore = Some(1550)

    val dt = new DateTime(gp.lbResetTimezone).withMillis(gp.currentTimeMillis())

    gp.updateScore(allTimeBestScore.get)

    // The daily score, initially set to the same value as the all-time score, is now more than a day old. The all-time
    // best score does no longer count as best of the day, it still stands as best of the week though.
    gp.updateTimestampModified(ScoreDaily, dt.minusHours(25).getMillis)
    gp.updateScore(dailyBestScore.get) shouldBe DailyTimeSpan
    gp.scoreAllTime shouldBe allTimeBestScore
    gp.scoreWeekly shouldBe allTimeBestScore
    gp.scoreDaily shouldBe dailyBestScore
    gp.previousScoreAllTime shouldBe empty // Not set.
    gp.previousScoreWeekly shouldBe empty // Not set.
    gp.previousScoreDaily shouldBe empty // Reset.

    // Update to better daily score only.
    gp.updateScore(betterDailyBestScore.get) shouldBe DailyTimeSpan
    gp.scoreDaily shouldBe betterDailyBestScore
    gp.previousScoreDaily shouldBe dailyBestScore

    // Wider time span scores should be unchanged.
    gp.scoreAllTime shouldBe allTimeBestScore
    gp.scoreWeekly shouldBe allTimeBestScore
  }

  test("Updating to a score to a value that is only better as a weekly and daily score, updates the previous weekly and daily scores") {
    val gp = mkGameProgress_LB(null, StatusOk, HigherIsBetter)
    val allTimeBestScore = Some(2000)
    val weeklyBestScore = Some(1750)
    val betterWeeklyBestScore = Some(1800)

    val dt = new DateTime(gp.lbResetTimezone).withMillis(gp.currentTimeMillis())

    gp.updateScore(allTimeBestScore.get)

    // The daily score, initially set to the same value as the all-time score, is now more than a week old. The all-time
    // best score does no longer count as best of the week and day.
    gp.updateTimestampModified(ScoreDaily, dt.minusDays(8).getMillis)
    gp.updateTimestampModified(ScoreWeekly, dt.minusDays(8).getMillis)
    gp.updateScore(weeklyBestScore.get) shouldBe DailyAndWeeklyTimeSpans
    gp.scoreAllTime shouldBe allTimeBestScore
    gp.scoreWeekly shouldBe weeklyBestScore
    gp.scoreDaily shouldBe weeklyBestScore
    gp.previousScoreAllTime shouldBe empty //Not set.
    gp.previousScoreWeekly shouldBe empty //Reset.
    gp.previousScoreDaily shouldBe empty //Reset.

    // Update to better weekly and daily score only.
    gp.updateScore(betterWeeklyBestScore.get) shouldBe DailyAndWeeklyTimeSpans
    gp.scoreWeekly shouldBe betterWeeklyBestScore
    gp.scoreDaily shouldBe betterWeeklyBestScore
    gp.previousScoreWeekly shouldBe weeklyBestScore
    gp.previousScoreDaily shouldBe weeklyBestScore

    // Wider time span scores should be unchanged.
    gp.scoreAllTime shouldBe allTimeBestScore
  }

}