elo_day <- function(date = Sys.Date()) {
  web <- "http://api.clubelo.com/"
  readr::read_csv(paste(web, date, sep = ""))
}

elo_club <- function(club) {
  if (length(club) != 1)
    stop("Function only constructed for a single string!")
  club_pro <- stringr::str_replace_all(club, "\\s", "")
  web <- "http://api.clubelo.com/"
  readr::read_csv(paste(web, club_pro, sep = ""))
}

elo_club_multiple <- function(club_vec) {
  erg_vec <- vector(mode = "list", length = length(club_vec))
  for (i in seq_along(club_vec)) {
    tryCatch(
      erg_vec[[i]] <- elo_club(club_vec[[i]]), finally = next)
  }
  dplyr::bind_rows(erg_vec)
}

#' Function to read todays SPI (Soccer Power Index) of 538.
#'
#' @return
#' @export
#'
#' @examples
spi_day <- function() {
  url <- "https://projects.fivethirtyeight.com/global-club-soccer-rankings/"

  html <- xml2::read_html(url)
  teams_nodes <- rvest::html_nodes(html, ".team-div .name")
  teams <- rvest::html_text(teams_nodes)
  ratings_nodes <- rvest::html_nodes(html, ".num:nth-child(8)")
  ratings <- rvest::html_text(ratings_nodes)
  # Delete first entry ("spi")
  ratings <- ratings[-1]

  erg <- tibble::tibble(team = teams, spi = ratings)
  return(erg)
}

