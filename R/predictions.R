#' Calculates tthe probability, that team a beats team b.
#' For both teams, there have to be a measurement of numeric
#' measurement of their strength. For example an elo number.
#'
#' @param elo_team_a Elo team a
#' @param elo_team_b Elo team b
#'
#' @return Probability, that team a beats team b
#' @export
#'
#' @examples
prob_a_beats_b <- function(elo_team_a, elo_team_b) {
  if (is.na(elo_team_a) || is.na(elo_team_b))
    stop("There has to be a elo for each team")
  dr <- elo_team_a - elo_team_b
  return(1 / (10^(-dr/400) + 1))
}

#' Function to predict the sore of a football match.
#'
#' @param result A string. 'home', 'draw' or 'away'
#' @param ratio The probability that the home team wins
#' @param max_add_goals Maximum additional goals if the home team wins
#'
#' @return A string of the result
#' @export
#'
#' @examples
#' pred_score("home", 0.8)
pred_score <- function(result, ratio, max_add_goals = 2) {
  if (!result %in% c("home", "draw", "away"))
    stop("result has to be 'home', 'draw' or 'away'")
  win_goals <- as.integer(1 + round(ratio * sample(0:max_add_goals, size = 1)))
  lose_goals <- as.integer(sum(runif(win_goals - 1) < ratio))
  if (result == "draw") {
    win_goals <- sample(max_add_goals, size = 1)
    lose_goals <- win_goals
  }
  if (result == "away") {
    return(paste(lose_goals, win_goals, sep = ":"))
  }
  return(paste(win_goals, lose_goals, sep = ":"))
}

