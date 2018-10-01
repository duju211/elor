#' Return all fixtures of a certain league.
#'
#' @param league
#'
#' @return
#' @export
#'
#' @examples
read_fixtures <- function(token, com_id) {
  request <- paste0("/v1/competitions/", com_id, "/fixtures")
  df_fixtures <- send_request(request, token)
  df_fixtures <- dplyr::mutate(
    df_fixtures, status = forcats::as_factor(status))
}

#' Function to read informations about
#' all competitions available
#'
#' @param token
#'
#' @return
#' @export
#'
#' @examples
read_competitions <- function(token) {
  req_string <- "/v2/competitions"
  df_competition <- send_request(req_string, token)
  df_competition
}

read_competition_table <- function(token, com_id, type = "TOTAL") {
  req_string <- paste0("/v2/competitions/", com_id, "/standings")
  standings <- send_request(token = token, request = req_string)
  erg <- standings$table[[which(standings$type == type)]]
  clean_erg <- tibble::as_tibble(erg[, !purrr::map_lgl(erg, purrr::is_list)])
  clean_erg$id <- erg$team[["id"]]
  clean_erg$name <- erg$team[["name"]]
  clean_erg$crestUrl <- erg$team[["crestUrl"]]
  return(clean_erg)
}

#' Read information about every team in a certain competition
#'
#' @param token
#' @param com_id
#'
#' @return
#' @export
#'
#' @examples
read_team_info <- function(token, com_id) {
  req_string <- paste0("/v1/competitions/", com_id, "/teams")
  send_request(token = token, request = req_string)
}

send_request <- function(request, token ) {
  url <- "api.football-data.org"
  headers <- c("X-Auth-Token", token)
  a <- httr::GET(
    paste0(url,request),
    httr::add_headers(
      "X-Auth-Token" = token, "X-Response-Control" = "minified"))
  df <- jsonlite::fromJSON(httr::content(a, as = "text"))
  if (any(stringr::str_detect(names(df), "error")))
    stop(df$error)
  if (!is.data.frame(df))
    df <- df[[length(df)]]
  df <- jsonlite::flatten(df)
  tibble::as_tibble(df)
}

read_past_fixtures <- function(token) {
  req_string <- "/v1/fixtures?timeFrame=p99"
  df_fixtures <- send_request(req_string, token)
  df_fixtures
}
