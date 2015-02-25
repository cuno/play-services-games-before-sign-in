package nl.cunodeboer.commons.android.googleplay.games

import com.google.android.gms.common.api._
import grizzled.slf4j.Logging
import nl.cunodeboer.commons.android.googleplay.games.AchievementResultType._
import nl.cunodeboer.commons.android.googleplay.games.Globals._
import nl.cunodeboer.commons.android.googleplay.games.GooglePlayGamesProperty._
import nl.cunodeboer.commons.android.googleplay.games.Helpers.mkGameProgress_Ach
import org.scalatest.concurrent.AsyncAssertions
import org.scalatest.mock.MockitoSugar
import org.scalatest.time.SpanSugar._
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}

class GameProgressJsonTest extends FunSuite with Matchers with BeforeAndAfterEach with MockitoSugar with AsyncAssertions with Logging {

  // Run a code block n times.
  implicit def intTimes(n: Int) = new {
    def times(fn: => Unit) = (1 to n) foreach (x => fn)
  }

  val gpSerializedValid =
    """{
      |    "timestampModified_Achs": 1425501279343,
      |    "timestampModified_IncAchs": 1425501279343,
      |    "scoreWeekly": 12345,
      |    "previousScoreAllTime": 1234,
      |    "gamesWonCount": 4,
      |    "incrementalAchievementsAddToRemote": {
      |        "Achievement E2": 12,
      |        "Achievement E1": 5
      |    },
      |    "previousScoreWeekly": 1234,
      |    "scoreDaily": 12345,
      |    "timestampSubmitted_Achs": 1425501279750,
      |    "achievementsUnlockedRemote": [
      |        "Achievement IU1",
      |        "Achievement U1"
      |    ],
      |    "timestampSubmitted_ScoreAllTime": 1425501279345,
      |    "timestampModified_ScoreAllTime": 1425501279343,
      |    "checksum": "7c42c281db05e21389e41563f3904fad8e66c9f4",
      |    "previousScoreDaily": 1234,
      |    "timestampModified_ScoreWeekly": 1425501279343,
      |    "timestampSubmitted_IncAchs": 1425501279499,
      |    "gamesLostCount": 3,
      |    "gamesDrawnCount": 2,
      |    "timestampSubmitted_ScoreWeekly": 1425501279345,
      |    "scoreAllTime": 12345,
      |    "timestampModified_ScoreDaily": 1425501279343,
      |    "achievementsUnlocked": [
      |        "Achievement IU1",
      |        "Achievement U1"
      |    ],
      |    "timestampSubmitted_ScoreDaily": 1425501279345
      |}""".stripMargin

  test("The clear() method works") {
    val gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid))
    gpDeserialized.scoreAllTime should not be empty
    gpDeserialized.scoreDaily should not be empty
    gpDeserialized.scoreWeekly should not be empty
    gpDeserialized.achievementsUnlocked should not be empty
    gpDeserialized.achievementsUnlockedRemote should not be empty
    gpDeserialized.incrementalAchievementsAddToRemote should not be empty
    gpDeserialized.previousScoreAllTime should not be empty
    gpDeserialized.previousScoreDaily should not be empty
    gpDeserialized.previousScoreWeekly should not be empty
    gpDeserialized.gamesDrawnCount should be > 0
    gpDeserialized.gamesLostCount should be > 0
    gpDeserialized.gamesWonCount should be > 0
    gpDeserialized.timestampModified should not be empty
    gpDeserialized.timestampSubmitted should not be empty

    gpDeserialized.clear()

    gpDeserialized.scoreAllTime shouldBe empty
    gpDeserialized.scoreDaily shouldBe empty
    gpDeserialized.scoreWeekly shouldBe empty
    gpDeserialized.achievementsUnlocked shouldBe empty
    gpDeserialized.achievementsUnlockedRemote shouldBe empty
    gpDeserialized.incrementalAchievementsAddToRemote shouldBe empty
    gpDeserialized.previousScoreAllTime shouldBe empty
    gpDeserialized.previousScoreDaily shouldBe empty
    gpDeserialized.previousScoreWeekly shouldBe empty
    gpDeserialized.gamesDrawnCount shouldBe 0
    gpDeserialized.gamesLostCount shouldBe 0
    gpDeserialized.gamesWonCount shouldBe 0
    gpDeserialized.timestampModified shouldBe empty
    gpDeserialized.timestampSubmitted shouldBe empty
  }

  test("JSON serializing and de-serializing works") {
    val waiter = new org.scalatest.concurrent.AsyncAssertions.Waiter
    val gpSerialize = mkGameProgress_Ach(waiter, IncrementUnlock, false)

    gpSerialize.incAchievement("Achievement E1", 5)
    gpSerialize.incAchievement("Achievement E2", 12)
    gpSerialize.incAchievement("Achievement I1", 9)
    gpSerialize.incAchievement("Achievement IU1", 15)
    gpSerialize.unlockAchievement("Achievement U1")

    val score = 12345
    val previousScore = 1234
    gpSerialize.updateScore(previousScore)
    gpSerialize.updateScore(score)

    // Non-Google-Play properties: add random amount of wins, losses and draws
    val gamesWonCount = rnd.nextInt(10)
    gamesWonCount times gpSerialize.registerWin()

    val gamesDrawnCount = rnd.nextInt(10)
    gamesDrawnCount times gpSerialize.registerDraw()

    val gamesLostCount = rnd.nextInt(10)
    gamesLostCount times gpSerialize.registerLoss()

    gpSerialize.syncUp()
    waiter.await(timeout(3 * MaxCallbackResponseTimeMillis + 1000 millis), org.scalatest.concurrent.AsyncAssertions.dismissals(6))

    val json = gpSerialize.toString

    val gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(json))

    gpSerialize.scoreAllTime.get should equal(score)
    gpSerialize.previousScoreAllTime.get should equal(previousScore)
    gpSerialize.previousScoreAllTime should equal(gpDeserialized.previousScoreAllTime)
    gpSerialize.scoreAllTime should equal(gpDeserialized.scoreAllTime)
    gpSerialize.achievementsUnlocked should equal(gpDeserialized.achievementsUnlocked)
    gpSerialize.achievementsUnlockedRemote should equal(gpDeserialized.achievementsUnlockedRemote)
    gpSerialize.incrementalAchievementsAddToRemote should equal(gpDeserialized.incrementalAchievementsAddToRemote)

    // Time stamps
    gpSerialize.timestampModified should (contain key ScoreAllTime and not contain value(null))
    gpSerialize.timestampModified should contain key Achs
    gpSerialize.timestampModified should contain key IncAchs
    gpSerialize.timestampSubmitted should (contain key ScoreAllTime and not contain value(null))
    gpSerialize.timestampSubmitted should contain key Achs
    gpSerialize.timestampSubmitted should contain key IncAchs
    gpSerialize.timestampModified should contain theSameElementsAs gpDeserialized.timestampModified
    gpSerialize.timestampSubmitted should contain theSameElementsAs gpDeserialized.timestampSubmitted

    // Non-Google-Play properties
    gpDeserialized.gamesDrawnCount should equal(gamesDrawnCount)
    gpDeserialized.gamesLostCount should equal(gamesLostCount)
    gpDeserialized.gamesWonCount should equal(gamesWonCount)
    gpDeserialized.gamesPlayedCount should equal(gamesDrawnCount + gamesLostCount + gamesWonCount)
    gpDeserialized.gamesPlayedCount should equal(gpSerialize.gamesPlayedCount)

    // JSON de-serializing "{}" results in properties that are an empty collection or 0
    val gpDeserializedEmptyJson = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some("{}"))
    gpDeserializedEmptyJson.gamesDrawnCount should equal(0)
    gpDeserializedEmptyJson.gamesLostCount should equal(0)
    gpDeserializedEmptyJson.gamesWonCount should equal(0)
    gpDeserializedEmptyJson.scoreAllTime should equal(None)
    gpDeserializedEmptyJson.previousScoreAllTime should equal(None)
    gpDeserializedEmptyJson.achievementsUnlocked should equal(Set.empty[String])
    gpDeserializedEmptyJson.achievementsUnlockedRemote should equal(Set.empty[String])
    gpDeserializedEmptyJson.incrementalAchievementsAddToRemote should equal(Map.empty[String, Int])
  }

  test("A valid game progress JSON file will be validated as such") {
    val gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid))

    // No need no test all properties.
    gpDeserialized.scoreAllTime should not be empty
    gpDeserialized.achievementsUnlocked should not be empty
    gpDeserialized.achievementsUnlockedRemote should not be empty
    gpDeserialized.incrementalAchievementsAddToRemote should not be empty
    gpDeserialized.previousScoreAllTime should not be empty
    gpDeserialized.gamesDrawnCount should be > 0
    gpDeserialized.gamesLostCount should be > 0
    gpDeserialized.gamesWonCount should be > 0
    gpDeserialized.timestampModified should not be empty
    gpDeserialized.timestampSubmitted should not be empty
  }

  test("A invalid game progress JSON file will be validated as such") {
    var gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("1425501279343", "1425501279344")))

    def assertAllEmpty() {
      gpDeserialized.scoreAllTime shouldBe empty
      gpDeserialized.achievementsUnlocked shouldBe empty
      gpDeserialized.achievementsUnlockedRemote shouldBe empty
      gpDeserialized.incrementalAchievementsAddToRemote shouldBe empty
      gpDeserialized.previousScoreAllTime shouldBe empty
      gpDeserialized.gamesDrawnCount shouldBe 0
      gpDeserialized.gamesLostCount shouldBe 0
      gpDeserialized.gamesWonCount shouldBe 0
      gpDeserialized.timestampModified shouldBe empty
      gpDeserialized.timestampSubmitted shouldBe empty
    }

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("1425501279343", "1425501279345")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("12345", "12346")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("1234,", "1235,")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("4,", "45,")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"gamesLostCount\": 3", "\"gamesLostCount\": 2")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"gamesDrawnCount\": 2", "\"gamesDrawnCount\": 1")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"previousScoreWeekly\": 1234", "\"previousScoreWeekly\": 1234321")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"scoreDaily\": 12345", "\"scoreDai\": 12345")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("timestampSubmitted_Achs\": 1425501279750", "timestampSubmitted_Achs\": 1425501279751")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"timestampSubmitted_ScoreAllTime\": 1425501279345", "\"timestampSubmitted_ScoreAllTime\": 1425510279345")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"timestampModified_ScoreAllTime\": 1425501279343", "\"timestampModified_ScoreAllTime\": 142553127934321")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"previousScoreDaily\": 1234", "\"previousScoreDaily\": 154763")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"timestampModified_ScoreWeekly\": 1425501279343", "\"timestampModified_ScoreWeekly\": 142552345676421")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"timestampSubmitted_IncAchs\": 1425501279499", "\"timestampSubmitted_IncAchs\": 142550127948865")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"timestampSubmitted_ScoreWeekly\": 1425501279345", "\"timestampSubmitted_ScoreWeekly\": 1425501179354")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"scoreAllTime\": 12345", "\"scoreAllTime\": 1232123")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"timestampModified_ScoreDaily\": 1425501279343", "\"timestampModified_ScoreDaily\": 4255012793651")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"timestampSubmitted_ScoreDaily\": 1425501279345", "\"timestampSubmitted_ScoreDaily\": 14124124124124")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"Achievement E2\": 12", "\"Achievement E2\": 11")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("\"Achievement U1\"", "\"Achievement u1\"")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("Achievement IU1", "Achievement IUx")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("Achievement U1", "Achievement Ux")))

    assertAllEmpty()

    gpDeserialized = new TestGameProgressFixedStartDateTime(mock[GoogleApiClient], true, Some(gpSerializedValid.replace("achievementsUnlocked", "achievementsUnlockde")))

    assertAllEmpty()
  }

}