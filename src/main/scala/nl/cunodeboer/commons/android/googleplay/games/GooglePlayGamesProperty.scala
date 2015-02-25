package nl.cunodeboer.commons.android.googleplay.games

/**
 * Enumeration that defines the types of values that Google Play Games Services provides.
 */
object GooglePlayGamesProperty extends Enumeration {
  type Property = Value
  val ScoreAllTime, ScoreDaily, ScoreWeekly, Achs, IncAchs = Value
}