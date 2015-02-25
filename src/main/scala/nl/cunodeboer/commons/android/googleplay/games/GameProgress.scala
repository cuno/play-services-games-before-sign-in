package nl.cunodeboer.commons.android.googleplay.games

import com.google.android.gms.common.api.{GoogleApiClient, ResultCallback}
import com.google.android.gms.games.Games
import com.google.android.gms.games.GamesStatusCodes.{STATUS_ACHIEVEMENT_UNLOCKED, STATUS_OK}
import com.google.android.gms.games.achievement.Achievement._
import com.google.android.gms.games.achievement.Achievements.{LoadAchievementsResult, UpdateAchievementResult}
import com.google.android.gms.games.achievement.{Achievement, Achievements}
import com.google.android.gms.games.leaderboard.LeaderboardVariant.{COLLECTION_PUBLIC, COLLECTION_SOCIAL, TIME_SPAN_ALL_TIME, TIME_SPAN_DAILY, TIME_SPAN_WEEKLY}
import com.google.android.gms.games.leaderboard.Leaderboards.{LoadPlayerScoreResult, SubmitScoreResult}
import com.google.android.gms.games.leaderboard.{LeaderboardScore, Leaderboards}
import grizzled.slf4j.Logging
import nl.cunodeboer.commons.android.googleplay.games.GooglePlayGamesProperty._
import org.joda.time.{DateTime, DateTimeZone}
import play.api.libs.json.Json
import play.api.libs.json.Json._

import scala.collection.concurrent.TrieMap

/**
 * Stores a user's game progress locally and synchronizes with Google Play Games Services.
 *
 * @see https://developers.google.com/games/services/training/signin#saving_player_progress_before_sign-in
 *
 * @author Cuno de Boer
 */
class GameProgress(googleApiClient: GoogleApiClient, val smallerIsBetter: Boolean, initJSON: Option[String] = None, leaderBoardId: Option[String] = None) extends Logging {

  final val lbResetTimezone = DateTimeZone.forID("America/Los_Angeles")

  /*
  Constants for use with JSON
   */
  private final val FieldGamesWonCount = "gamesWonCount"
  private final val FieldGamesDrawnCount = "gamesDrawnCount"
  private final val FieldGamesLostCount = "gamesLostCount"
  private final val FieldScoreAllTime = "scoreAllTime"
  private final val FieldScoreDaily = "scoreDaily"
  private final val FieldScoreWeekly = "scoreWeekly"
  private final val FieldPreviousScoreAllTime = "previousScoreAllTime"
  private final val FieldPreviousScoreDaily = "previousScoreDaily"
  private final val FieldPreviousScoreWeekly = "previousScoreWeekly"
  private final val FieldIncrementalAchievementsAddToRemote = "incrementalAchievementsAddToRemote"
  private final val FieldAchievementsUnlockedRemote = "achievementsUnlockedRemote"
  private final val FieldAchievementsUnlocked = "achievementsUnlocked"

  /*
  The stats that are kept.
   */
  private var _timestampModified: Map[GooglePlayGamesProperty.Value, Long] = Map.empty
  private var _timestampSubmitted: Map[GooglePlayGamesProperty.Value, Long] = Map.empty
  private var _gamesWonCount = 0
  private var _gamesDrawnCount = 0
  private var _gamesLostCount = 0
  private var _scoreAllTime: Option[Long] = None
  private var _scoreDaily: Option[Long] = None
  private var _scoreWeekly: Option[Long] = None
  private var _previousScoreAllTime: Option[Long] = None
  private var _previousScoreDaily: Option[Long] = None
  private var _previousScoreWeekly: Option[Long] = None
  private val _achievementsUnlocked: TrieMap[String, String] = new TrieMap[String, String]
  private val _achievementsUnlockedRemote: TrieMap[String, String] = new TrieMap[String, String]
  private val _incrementalAchievementsAddToRemote: TrieMap[String, Int] = new TrieMap[String, Int]

  /*
  The supported Google Play Games API's.
   */
  private var _leaderboardsApi: Leaderboards = _
  private var _achievementsApi: Achievements = _

  var currentTimeMillis: () => Long = () => 0

  /**
   * Can be overridden for testing purposes.
   */
  protected[commons] def init() {
    debug("Initializing GameProgress")
    _leaderboardsApi = Games.Leaderboards
    _achievementsApi = Games.Achievements

    currentTimeMillis = () => System.currentTimeMillis
  }

  init()

  private val md = java.security.MessageDigest.getInstance("SHA-1")
  private final val ReplacementSha1 = "2f67cb79c409007642432be6ea110b26c2ba4d35"

  /**
   * Virtual methods for type Long to determine if a timestamps is in the current week or day.
   * @param timestamp the left hand side optional long
   */
  implicit class TimeStampUtils(val timestamp: Option[Long]) {
    def isNotOnThisDay = timestamp.isEmpty || new DateTime(currentTimeMillis(), lbResetTimezone).getDayOfYear != new DateTime(timestamp.get, lbResetTimezone).getDayOfYear

    def isNotInThisWeek = timestamp.isEmpty || new DateTime(currentTimeMillis(), lbResetTimezone).getWeekOfWeekyear != new DateTime(timestamp.get, lbResetTimezone).getWeekOfWeekyear
  }

  /**
   * Virtual methods for type Long to compare scores.
   * @param lhs the left hand side long value
   */
  implicit class ScoreUtils(val lhs: Long) {

    /**
     * @param rhs the right hand side optional long, None counts as the worst possible value
     * @return true if better, false otherwide
     */
    def isBetterThen(rhs: Option[Long]) = if (smallerIsBetter) lhs < rhs.getOrElse(Long.MaxValue) else lhs > rhs.getOrElse(Long.MinValue)
  }

  def leaderboardsApi = _leaderboardsApi

  def achievementsApi = _achievementsApi

  initJSON match {
    case Some(jsonString) =>
      val json = Json.parse(jsonString)
      _scoreAllTime = (json \ FieldScoreAllTime).asOpt[Long]
      _scoreDaily = (json \ FieldScoreDaily).asOpt[Long]
      _scoreWeekly = (json \ FieldScoreWeekly).asOpt[Long]
      _previousScoreAllTime = (json \ FieldPreviousScoreAllTime).asOpt[Long]
      _previousScoreDaily = (json \ FieldPreviousScoreDaily).asOpt[Long]
      _previousScoreWeekly = (json \ FieldPreviousScoreWeekly).asOpt[Long]
      (json \ FieldAchievementsUnlocked).asOpt[Set[String]] match {
        case Some(set) => for (valueAndKey <- set) _achievementsUnlocked.put(valueAndKey, valueAndKey)
        case None =>
      }
      (json \ FieldAchievementsUnlockedRemote).asOpt[Set[String]] match {
        case Some(set) => for (valueAndKey <- set) _achievementsUnlockedRemote.put(valueAndKey, valueAndKey)
        case None =>
      }
      (json \ FieldIncrementalAchievementsAddToRemote).asOpt[Map[String, Int]] match {
        case Some(map) => for ((key, value) <- map) _incrementalAchievementsAddToRemote.put(key, value)
        case None =>
      }
      _gamesDrawnCount = (json \ FieldGamesDrawnCount).asOpt[Int].getOrElse(0)
      _gamesLostCount = (json \ FieldGamesLostCount).asOpt[Int].getOrElse(0)
      _gamesWonCount = (json \ FieldGamesWonCount).asOpt[Int].getOrElse(0)
      // Time stamps
      (json \ s"timestampModified_$ScoreAllTime").asOpt[Long] match {
        case Some(timestamp) => _timestampModified += (ScoreAllTime -> timestamp)
        case None =>
      }
      (json \ s"timestampModified_$ScoreWeekly").asOpt[Long] match {
        case Some(timestamp) => _timestampModified += (ScoreWeekly -> timestamp)
        case None =>
      }
      (json \ s"timestampModified_$ScoreDaily").asOpt[Long] match {
        case Some(timestamp) => _timestampModified += (ScoreDaily -> timestamp)
        case None =>
      }
      (json \ s"timestampModified_$Achs").asOpt[Long] match {
        case Some(timestamp) => _timestampModified += (Achs -> timestamp)
        case None =>
      }
      (json \ s"timestampModified_$IncAchs").asOpt[Long] match {
        case Some(timestamp) => _timestampModified += (IncAchs -> timestamp)
        case None =>
      }
      (json \ s"timestampSubmitted_$ScoreAllTime").asOpt[Long] match {
        case Some(timestamp) => _timestampSubmitted += (ScoreAllTime -> timestamp)
        case None =>
      }
      (json \ s"timestampSubmitted_$ScoreWeekly").asOpt[Long] match {
        case Some(timestamp) => _timestampSubmitted += (ScoreWeekly -> timestamp)
        case None =>
      }
      (json \ s"timestampSubmitted_$ScoreDaily").asOpt[Long] match {
        case Some(timestamp) => _timestampSubmitted += (ScoreDaily -> timestamp)
        case None =>
      }
      (json \ s"timestampSubmitted_$Achs").asOpt[Long] match {
        case Some(timestamp) => _timestampSubmitted += (Achs -> timestamp)
        case None =>
      }
      (json \ s"timestampSubmitted_$IncAchs").asOpt[Long] match {
        case Some(timestamp) => _timestampSubmitted += (IncAchs -> timestamp)
        case None =>
      }

      // Validate checksum.
      (json \ "checksum").asOpt[String] match {
        case Some(sha1Deserialized) if sha1Deserialized == sha1sum =>
          debug("Game progress file is valid")
        case _ =>
          warn(s"Game progress file is corrupt")
          clear()
      }
    case None =>
  }

  /**
   * Clear all properties.
   */
  def clear() {
    _achievementsUnlocked.clear()
    _achievementsUnlockedRemote.clear()
    _incrementalAchievementsAddToRemote.clear()
    _scoreAllTime = None
    _scoreDaily = None
    _scoreWeekly = None
    _previousScoreAllTime = None
    _previousScoreDaily = None
    _previousScoreWeekly = None
    _gamesDrawnCount = 0
    _gamesLostCount = 0
    _gamesWonCount = 0
    _timestampModified = Map.empty
    _timestampSubmitted = Map.empty
  }

  /**
   * Should be called after every local change of a Google Play Property.
   */
  private def updateTimestampModified(property: GooglePlayGamesProperty.Value) {
    updateTimestampModified(property, currentTimeMillis())
  }

  /**
   * Updates the appropriate submitted-timestamps. Updating the all-time score timestamp automatically also updates weekly
   * and daily timestamps and updating the weekly score timestamp automatically also updates the daily timestamp.
   *
   * Should be called after every remote change of a Google Play Property.
   *
   * @param property property to set modified timestamp for
   */
  private def updatetimestampsSubmitted(property: GooglePlayGamesProperty.Value) {
    val ts = currentTimeMillis()
    updatetimestampSubmitted(property, ts)
    // Also update encompassing leaderboard score time spans.
    property match {
      case ScoreAllTime =>
        updatetimestampSubmitted(ScoreWeekly, ts)
        updatetimestampSubmitted(ScoreDaily, ts)
      case ScoreWeekly =>
        updatetimestampSubmitted(ScoreDaily, ts)
      case _ =>
    }
  }

  /**
   * Updates a single timestamp, for testing purposes.
   * @param property property to set modified timestamp for
   * @param timestamp the timestamp in milliseconds
   */
  protected[commons] def updateTimestampModified(property: GooglePlayGamesProperty.Value, timestamp: Long) {
    _timestampModified += (property -> timestamp)
    debug(s"Updated timestamp for modified $property")
  }

  /**
   * Updates a single timestamp, for testing purposes.
   * @param property property to set submitted timestamp for
   * @param timestamp the timestamp in milliseconds
   */
  protected[commons] def updatetimestampSubmitted(property: GooglePlayGamesProperty.Value, timestamp: Long) {
    _timestampSubmitted += (property -> timestamp)
    debug(s"Updated timestamp for synced up of $property")
  }

  /**
   * For testing purposes.
   * @return the modified timestamp
   */
  protected[commons] def timestampModified = _timestampModified

  /**
   * For testing purposes.
   * @return the submitted timestamp
   */
  protected[commons] def timestampSubmitted = _timestampSubmitted

  /**
   * For testing purposes.
   * @param value the alternative Leaderboards API
   */
  protected[commons] def leaderboardsApi_=(value: Leaderboards) {
    _leaderboardsApi = value
  }

  /**
   * For testing purposes.
   * @param value the alternative Achievements API
   */
  protected[commons] def achievementsApi_=(value: Achievements) {
    _achievementsApi = value
  }

  /**
   * @return a set containing unlocked achievements that are confirmed to be unlocked at Google Play Games Services. It's purpose is to not unlock it again.
   */
  protected[commons] def achievementsUnlockedRemote = _achievementsUnlockedRemote.keySet

  /**
   * @return a map containing achievements mapped to progress increment counts (not the total progress) that will be submitted to Google Play Games Services at some point.
   */
  protected[commons] def incrementalAchievementsAddToRemote = _incrementalAchievementsAddToRemote.toMap

  protected[commons] def achievementsUnlockedRemoteCount = _achievementsUnlockedRemote.size

  protected[commons] def incrementalAchievementsAddToRemoteCount = _incrementalAchievementsAddToRemote.size

  /**
   * @return a set containing unlocked achievements returned from local storage which will be submitted to Google Play Games Services at some point.
   */
  def achievementsUnlocked = _achievementsUnlocked.keySet

  /**
   * @return the number of finished games that ended in a win.
   */
  def gamesWonCount = _gamesWonCount

  /**
   * @return the number of games that ended in a loss.
   */
  def gamesLostCount = _gamesLostCount

  /**
   * @return the number of games that ended in a draw.
   */
  def gamesDrawnCount = _gamesDrawnCount

  /**
   * @return the number of games played
   */
  def gamesPlayedCount = _gamesWonCount + _gamesDrawnCount + _gamesLostCount

  /**
   * @return the score with time span all time
   */
  def scoreAllTime = _scoreAllTime

  /**
   * @return the previous score with time span all time
   */
  def previousScoreAllTime = _previousScoreAllTime

  /**
   * @return the score with time span daily
   */
  def scoreDaily = {
    resetOldDailyScore()
    _scoreDaily
  }

  /**
   * @return the previous score with time span daily
   */
  def previousScoreDaily = {
    resetOldDailyScore()
    _previousScoreDaily
  }

  /**
   * @return the score with time span weekly
   */
  def scoreWeekly = {
    resetOldWeeklyScore()
    _scoreWeekly
  }

  /**
   * @return the previous score with time span weekly
   */
  def previousScoreWeekly = {
    resetOldWeeklyScore()
    _previousScoreWeekly
  }

  def isScoreAllTimeSet = _scoreAllTime.nonEmpty

  def isScoreWeeklySet = _scoreWeekly.nonEmpty

  def isScoreDailySet = _scoreDaily.nonEmpty

  def isPreviousScoreAllTimeSet = _previousScoreAllTime.nonEmpty

  def isPreviousScoreWeeklySet = _previousScoreWeekly.nonEmpty

  def isPreviousScoreDailySet = _previousScoreDaily.nonEmpty

  def achievementsUnlockedCount = _achievementsUnlocked.size

  /**
   * @param achievementId the achievement ID to check
   * @return true if the achievement is unlocked
   */
  def isUnlocked(achievementId: String) = {
    _achievementsUnlocked.contains(achievementId)
  }

  protected[commons] def isUnlockedRemote(achievementId: String) = {
    _achievementsUnlockedRemote.contains(achievementId)
  }

  /**
   * Register a game that ended in a win.
   */
  def registerWin() = {
    _gamesWonCount += 1
  }

  /**
   * Register a game that ended in a draw.
   */
  def registerDraw() = {
    _gamesDrawnCount += 1
  }

  /**
   * Register a game that ended in a loss.
   */
  def registerLoss() = {
    _gamesLostCount += 1
  }

  /**
   * Resets the daily score if it's the next day, reset occurs at 24:00 PST.
   */
  private def resetOldDailyScore() {
    if (_timestampModified get ScoreDaily isNotOnThisDay) {
      debug("Resetting daily score")
      _previousScoreDaily = _scoreDaily
      _scoreDaily = None
    } else {
      debug("NOT resetting daily score")
    }
  }

  /**
   * Resets the weekly score if it's the next week, reset occurs at 24:00 PST on sunday.
   */
  private def resetOldWeeklyScore() {
    if (_timestampModified get ScoreWeekly isNotInThisWeek) {
      debug("Resetting weekly score")
      _previousScoreWeekly = _scoreWeekly
      _scoreWeekly = None
    } else {
      debug("NOT resetting weekly score")
    }
  }

  /**
   * Updates the score for time spans all time, daily and weekly.
   * @param score the better score candidate
   * @return a set of time span constants indicating for which time spans the score was modified.
   *
   * @see com.google.android.gms.games.leaderboard.LeaderboardVariant#TIME_SPAN_ALL_TIME
   * @see com.google.android.gms.games.leaderboard.LeaderboardVariant#TIME_SPAN_DAILY
   * @see com.google.android.gms.games.leaderboard.LeaderboardVariant#TIME_SPAN_WEEKLY
   */
  def updateScore(score: Long) = {
    // Reset daily and weekly scores when appropriate, before comparing old and new scores.
    resetOldWeeklyScore()
    resetOldDailyScore()

    // Update leaderboard scores of the appropriate timestamps, trying wider time spans first.
    score match {
      case score if score isBetterThen _scoreAllTime =>
        _previousScoreAllTime = _scoreAllTime
        _scoreAllTime = Some(score)
        _previousScoreWeekly = _scoreWeekly
        _scoreWeekly = _scoreAllTime
        _previousScoreDaily = _scoreDaily
        _scoreDaily = _scoreAllTime
        updateTimestampModified(ScoreAllTime)
        updateTimestampModified(ScoreWeekly)
        updateTimestampModified(ScoreDaily)
        Set(TIME_SPAN_ALL_TIME, TIME_SPAN_WEEKLY, TIME_SPAN_DAILY)
      case score if score isBetterThen _scoreWeekly =>
        _scoreWeekly = Some(score)
        _previousScoreDaily = _scoreDaily
        _scoreDaily = _scoreWeekly
        updateTimestampModified(ScoreWeekly)
        updateTimestampModified(ScoreDaily)
        Set(TIME_SPAN_WEEKLY, TIME_SPAN_DAILY)
      case score if score isBetterThen _scoreDaily =>
        _scoreDaily = Some(score)
        updateTimestampModified(ScoreDaily)
        Set(TIME_SPAN_DAILY)
      case _ => Set[Int]()
    }
  }

  def unlockAchievement(achievementId: String) {
    _achievementsUnlocked.putIfAbsent(achievementId, achievementId)
    updateTimestampModified(Achs)
  }

  /**
   * Increments an achievement. The count at which it will be unlocked is not known locally so it is possible to go over
   * the remote maximum, this is OK because Play Games Services will ignore incrementing unlocked incremental achievements.
   * @param achievementId the ID of the achievement
   * @param addition the amount to add, one by default
   */
  def incAchievement(achievementId: String, addition: Int = 1) {
    // Ignore if unlocked (when unlocked only locally, it will be submitted at some point in time)
    if (_achievementsUnlocked.contains(achievementId)) {
      debug(s"Not incrementing unlocked achievement '$achievementId'")
    } else {
      _incrementalAchievementsAddToRemote.get(achievementId) match {
        case Some(count) => _incrementalAchievementsAddToRemote.replace(achievementId, count + addition)
        case None => _incrementalAchievementsAddToRemote.put(achievementId, addition)
      }
      updateTimestampModified(IncAchs)
    }
  }

  /**
   * Bi-directional sync of the score and unlocked achievements.
   */
  def sync() {
    syncScore()
    syncAchievements()
  }

  /**
   * Retrieves remote score and submits a score to Google Play Games Services if the local score is better.
   */
  def syncScore() {
    syncScoreDown {
      syncScoreUp()
    }
  }

  /**
   * Retrieves remote achievements and submits achievements to Google Play Games Services that are not yet submitted.
   */
  def syncAchievements() {
    syncAchievementsDown {
      syncAchievementsUp()
    }
  }

  def syncDown() {
    syncScoreDown()
    syncAchievementsDown()
  }

  /**
   * Asynchronously checks the currently signed in player's remote score, looking in both the public and social leaderboard.
   * Then, in the callback method, updates the local score if the remote score is better and finally runs the doAfter code block.
   */
  def syncScoreDown() {
    syncScoreDown({})
  }

  /**
   * Asynchronously checks the currently signed in player's remote score, looking in both the public and social leaderboard.
   * Then, in the callback method, updates the local score if the remote score is better and finally runs the doAfter code block.
   *
   * @param doAfter code to run after the download succeeded
   */
  private def syncScoreDown(doAfter: => Unit) {
    def handleRemoteScore(leaderboardScore: LeaderboardScore) {
      val remoteScore = leaderboardScore.getRawScore
      if (remoteScore isBetterThen _scoreAllTime) {
        updatetimestampsSubmitted(ScoreAllTime) // We know local isn't better, so don't sync up
        _scoreAllTime = Some(remoteScore)
        debug(s"Found better remote score: $remoteScore, local score is now ${_scoreAllTime}")
      }
      doAfter
    }
    // Load player's best score.
    leaderBoardId match {
      case Some(lbId) =>
        // Try Public collection.
        _leaderboardsApi.loadCurrentPlayerLeaderboardScore(googleApiClient, lbId, TIME_SPAN_ALL_TIME, COLLECTION_PUBLIC).setResultCallback(new ResultCallback[LoadPlayerScoreResult] {
          override def onResult(res: LoadPlayerScoreResult) {
            res.getStatus.getStatusCode match {
              case STATUS_OK =>
                val leaderboardScore = res.getScore
                if (leaderboardScore == null) {
                  debug("Found no public leaderboard score, now trying social leaderboard")
                  // Try social collection.
                  _leaderboardsApi.loadCurrentPlayerLeaderboardScore(googleApiClient, lbId, TIME_SPAN_ALL_TIME, COLLECTION_SOCIAL).setResultCallback(new ResultCallback[LoadPlayerScoreResult] {
                    override def onResult(res: LoadPlayerScoreResult) {
                      res.getStatus.getStatusCode match {
                        case STATUS_OK =>
                          val leaderboardScore = res.getScore
                          if (leaderboardScore == null) {
                            debug(s"Found no public or social leaderboard score")
                          } else {
                            handleRemoteScore(leaderboardScore)
                          }
                        case _ =>
                          debug(s"Received status code ${res.getStatus.getStatusCode} when trying to get score from social leaderboard")
                      }
                    }
                  })
                } else {
                  handleRemoteScore(leaderboardScore)
                }
              case _ =>
                debug(s"Received status code ${res.getStatus.getStatusCode} when trying to get score from public leaderboard")
            }
          }
        })
      case None => // No leaderboard so nothing to do
    }

  }

  /**
   * Asynchronously loads the currently signed in player's unlocked achievements.
   */
  def syncAchievementsDown() {
    syncAchievementsDown({})
  }

  /**
   * Asynchronously loads the currently signed in player's unlocked achievements.
   * @param doAfter code to run after the download attempt
   */
  private def syncAchievementsDown(doAfter: => Unit) {
    _achievementsApi.load(googleApiClient, true).setResultCallback(new ResultCallback[LoadAchievementsResult] {

      import scala.collection.JavaConversions.asScalaIterator

      override def onResult(res: LoadAchievementsResult) {
        res.getStatus.getStatusCode match {
          case STATUS_OK =>
            val buffer = res.getAchievements
            for (a: Achievement <- buffer.iterator) {
              a.getType match {
                case TYPE_STANDARD | TYPE_INCREMENTAL =>
                  a.getState match {
                    case STATE_HIDDEN | STATE_REVEALED => // Not unlocked
                    case STATE_UNLOCKED => // Unlocked
                      val valueAndKey = a.getAchievementId
                      _achievementsUnlocked.put(valueAndKey, valueAndKey)
                      _achievementsUnlockedRemote.put(valueAndKey, valueAndKey)
                      debug(s"sync achievements down unlocked achievement with id $valueAndKey")
                  }
              }
            }
            buffer.close()
            res.release()
            doAfter
          case _ => debug(s"Received status code ${res.getStatus.getStatusCode} when trying to load achievements")
        }
      }
    }

    )
  }

  /**
   * Asynchronously submits the local storage to Google Play Games if it has changes locally.
   */
  def syncUp() {
    syncScoreUp()
    syncAchievementsUp()
    syncIncrementalAchievementsUp()
  }

  def syncScoreUp() {
    if (leaderBoardId.isEmpty) {
      if (isScoreAllTimeSet) throw new IllegalStateException("Can't submit score when no leaderboard ID is set")
    } else {
      def submitScore(score: Option[Long], timeSpan: GooglePlayGamesProperty.Value) {
        score match {
          case Some(score) =>
            _leaderboardsApi.submitScoreImmediate(googleApiClient, leaderBoardId.get, score).setResultCallback(new ResultCallback[SubmitScoreResult] {
              override def onResult(res: SubmitScoreResult) {
                val statusCode = res.getStatus.getStatusCode
                debug(s"SubmitScoreResult status code: $statusCode")
                statusCode match {
                  // Remote score update confirmed.
                  case STATUS_OK =>
                    updatetimestampsSubmitted(timeSpan)
                  case _ => // No problem, better luck next time
                }
                res.release()
              }
            })
          case _ =>
            debug("No score to submit.")
            updatetimestampsSubmitted(timeSpan)
        }
      }
      def submitWidestTimeStampScore(score: Option[Long], timeSpan: GooglePlayGamesProperty.Value) {
        // It's only necessary to submit the score of the widest time span. E.G. A new best score of all-time is also the
        // new best score of the week which is also the new best of the day.
        (_timestampModified get timeSpan, _timestampSubmitted get timeSpan) match {
          case (Some(_), None) =>
            debug(s"[submitWidestTimeStampScore()] Submitting $timeSpan because it has never been submitted.")
            submitScore(score, timeSpan)
          case (Some(modified), Some(submitted)) if modified >= submitted =>
            debug(s"[submitWidestTimeStampScore()] Submitting $timeSpan because it is modified.")
            submitScore(score, timeSpan)
          case _ =>
            debug(s"[submitWidestTimeStampScore()] NOT submitting $timeSpan because it is NOT modified.")
            timeSpan match {
              case ScoreAllTime => submitWidestTimeStampScore(_scoreWeekly, ScoreWeekly)
              case ScoreWeekly => submitWidestTimeStampScore(_scoreDaily, ScoreDaily)
              case _ =>
            }
        }
      }
      submitWidestTimeStampScore(_scoreAllTime, ScoreAllTime)
    }
  }

  def syncAchievementsUp() {
    def submitAchievements() {
      // Tests if all local achievements are already submitted.
      var noSubmitAttempts = true

      if (_achievementsUnlocked.isEmpty) updatetimestampsSubmitted(Achs)
      else for (achievementId <- _achievementsUnlocked.keys) {
        // Skip if already done.
        if (!_achievementsUnlockedRemote.contains(achievementId)) {
          noSubmitAttempts = false
          _achievementsApi.unlockImmediate(googleApiClient, achievementId).setResultCallback(new ResultCallback[UpdateAchievementResult]() {
            override def onResult(res: UpdateAchievementResult) {
              val statusCode = res.getStatus.getStatusCode
              debug(s"sync non-incremental achievements result status code: $statusCode")
              statusCode match {
                // Remote unlock confirmed.
                case STATUS_OK =>
                  _achievementsUnlockedRemote.put(achievementId, achievementId)
                  updatetimestampsSubmitted(Achs)
                case _ => // No problem, better luck next time
              }
            }
          })
        }
      }
      if (noSubmitAttempts) {
        debug("No achievements to unlock.")
        updatetimestampsSubmitted(Achs)
      }
    }
    (_timestampModified get Achs, _timestampSubmitted get Achs) match {
      case (Some(modified), None) =>
        debug(s"Submitting achievements because changes are not submitted yet, ever")
        submitAchievements()
      case (Some(modified), Some(submitted)) if modified >= submitted =>
        debug(s"Submitting achievements because changes are not submitted yet")
        submitAchievements()
      case _ =>
        debug(s"Not submitting achievements because no modified achievements were found")
    }
  }

  def syncIncrementalAchievementsUp() {
    def submitIncrementalAchievements() {
      // Tests if all local incremental achievements are already submitted.
      var noIncrementAttempts = true

      if (_incrementalAchievementsAddToRemote.isEmpty) updatetimestampsSubmitted(IncAchs)
      else for ((achievementId, addition) <- _incrementalAchievementsAddToRemote) {
        // Unlocked achievemnts can't be incremented and will be ignored.
        if (_achievementsUnlockedRemote.contains(achievementId)) {
          debug(s"Ignoring incrementing request of unlocked achievement '$achievementId'")
        } else {
          noIncrementAttempts = false
          _achievementsApi.incrementImmediate(googleApiClient, achievementId, addition).setResultCallback(new ResultCallback[UpdateAchievementResult]() {
            override def onResult(res: UpdateAchievementResult) {
              val statusCode = res.getStatus.getStatusCode
              debug(s"sync incremental achievements result status code: $statusCode")
              statusCode match {
                case STATUS_OK =>
                  // Adding to remote confirmed.
                  _incrementalAchievementsAddToRemote.remove(achievementId)
                  updatetimestampsSubmitted(IncAchs)
                  debug(s"Added to achievement '$achievementId'")
                case STATUS_ACHIEVEMENT_UNLOCKED =>
                  // Unlocking remote confirmed.
                  _incrementalAchievementsAddToRemote.remove(achievementId, addition)
                  _achievementsUnlocked.put(achievementId, achievementId)
                  _achievementsUnlockedRemote.put(achievementId, achievementId)
                  updatetimestampsSubmitted(IncAchs)
                  debug(s"Added to, and unlocked achievement '$achievementId'")
                case _ => // No problem, better luck next time
                  debug(s"Error status $statusCode received for submitting achievement '$achievementId'")
              }
            }
          })
        }
      }
      if (noIncrementAttempts) {
        debug("No achievements to increment.")
        updatetimestampsSubmitted(IncAchs)
      }
    }
    (_timestampModified get IncAchs, _timestampSubmitted get IncAchs) match {
      case (Some(modified), None) =>
        debug(s"Submitting incremental achievements because changes are not submitted yet, ever")
        submitIncrementalAchievements()
      case (Some(modified), Some(submitted)) if modified >= submitted =>
        debug(s"Submitting incremental achievements because changes are not submitted yet")
        submitIncrementalAchievements()
      case _ =>
        debug(s"Not submitting incremental achievements because no modified incremental achievements were found")
    }
  }

  /**
   * Generates a sha1sum to provide some protection against editing of the values in the JSON game progress file.
   * @return the sha1sum
   */
  private def sha1sum = {
    val input1 = (
      _previousScoreAllTime.getOrElse[Long](0) + _scoreAllTime.getOrElse[Long](0)
        + _previousScoreWeekly.getOrElse[Long](0) + _scoreWeekly.getOrElse[Long](0)
        + _previousScoreDaily.getOrElse[Long](0) + _scoreDaily.getOrElse[Long](0)
        - ((gamesWonCount * 5 + gamesDrawnCount * 3 + gamesLostCount) * 25 + _achievementsUnlocked.size * 8 +
        _achievementsUnlockedRemote.size * 5 + _incrementalAchievementsAddToRemote.size * 3)).toString
    val input2 = achievementsUnlocked.toList.sorted.map(key => s"$key").mkString
    val input3 = achievementsUnlockedRemote.toList.sorted.map(key => s"$key").mkString
    val input4 = _incrementalAchievementsAddToRemote.toList.sorted.map(entry => s"${entry._1}${entry._2}").mkString
    val input5 = _timestampModified.toList.sorted.map(entry => s"${entry._1}${entry._2}").mkString
    val input6 = _timestampSubmitted.toList.sorted.map(entry => s"${entry._1}${entry._2}").mkString
    val input = s"$input1$input2$input3$input4$input5$input6"
    md.digest(input.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }

  /**
   * Creates a json representation of the game properties with a dummy checksum that is to be replaced a with a real one
   * later on
   * .
   * @return the Json object
   */
  protected[commons] def toJSON = {
    toJson(Map(
      FieldGamesWonCount -> toJson(_gamesWonCount),
      FieldGamesDrawnCount -> toJson(_gamesDrawnCount),
      FieldGamesLostCount -> toJson(_gamesLostCount),
      FieldScoreAllTime -> toJson(_scoreAllTime),
      FieldScoreDaily -> toJson(_scoreDaily),
      FieldScoreWeekly -> toJson(_scoreWeekly),
      FieldPreviousScoreAllTime -> toJson(_previousScoreAllTime),
      FieldPreviousScoreDaily -> toJson(_previousScoreDaily),
      FieldPreviousScoreWeekly -> toJson(_previousScoreWeekly),
      FieldIncrementalAchievementsAddToRemote -> toJson(incrementalAchievementsAddToRemote),
      FieldAchievementsUnlockedRemote -> toJson(achievementsUnlockedRemote),
      FieldAchievementsUnlocked -> toJson(achievementsUnlocked),
      s"timestampModified_$ScoreAllTime" -> toJson(_timestampModified.get(ScoreAllTime)),
      s"timestampModified_$ScoreWeekly" -> toJson(_timestampModified.get(ScoreWeekly)),
      s"timestampModified_$ScoreDaily" -> toJson(_timestampModified.get(ScoreDaily)),
      s"timestampModified_$Achs" -> toJson(_timestampModified.get(Achs)),
      s"timestampModified_$IncAchs" -> toJson(_timestampModified.get(IncAchs)),
      s"timestampSubmitted_$ScoreAllTime" -> toJson(_timestampSubmitted.get(ScoreAllTime)),
      s"timestampSubmitted_$ScoreWeekly" -> toJson(_timestampSubmitted.get(ScoreWeekly)),
      s"timestampSubmitted_$ScoreDaily" -> toJson(_timestampSubmitted.get(ScoreDaily)),
      s"timestampSubmitted_$Achs" -> toJson(_timestampSubmitted.get(Achs)),
      s"timestampSubmitted_$IncAchs" -> toJson(_timestampSubmitted.get(IncAchs)),
      "checksum" -> toJson(ReplacementSha1) // To be replaced with actual sha1sum
    ))
  }

  override def toString = {
    val jsonString = Json.stringify(toJSON)
    jsonString.replace(ReplacementSha1, sha1sum)
  }

}