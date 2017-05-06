#' Capitalize the first letter of each word in a character string.
#'
#' \code{capwords} is a slightly simplified version of the \code{capwords}
#'   example function provided in the \code{\link[base]{toupper}} documentation.
#'
#' @param s A character vector, or an object that can be coerced to character by
#'   \code{\link[base]{as.character}}.
#' @return A string where the first letter of each word is capitalized, and the
#'   remaining letters are lower-case.
#' @examples
#' capwords("the quick brown fox did a thing.")
#' capwords("THE QUICK BROWN FOX DID A THING.")
#' @export
capwords <- function(s) {
  cap <- function(s) {
    paste(toupper(substring(s, 1, 1)),
          { s <- substring(s, 2); tolower(s) },
          sep = "", collapse = " " )
  }
  sapply(strsplit(s, split = " "), cap, USE.NAMES = ! is.null(names(s)))
}

#' Generate a list of the Rankings Tables.
#'
#' Given a dataframe of all of the "Ranks" worksheet, \code{rank_list} generates
#'   a list where each element of the list is a nicely-formatted table. Each
#'   table can then be called using the syntax \code{r_list[[table]]}.
#'
#' This function is not called directly, but is instead a helper function for
#'   \code{link{print_board}}
#'
#' @param r A data frame version of the "Ranks" worksheet in the Daily Mini
#'   spreadsheet.
#' @return A list of length 6, where each element of the list returns the
#'   corresponding rankings table.
rank_list = function(r) {
  # trim trailing rows from r
  r <- r[ ! r$X2 == ".", ]

  # trim separating columns from r
  r <- r[, -seq(1, 24, by = 4)]

  # initialize list and names
  r_list  <- vector("list", (ncol(r) / 3))
  r_names <- vector("character", length(r_list))
  for (i in 1 : length(r_list)) {

    cols <- (1:3) + 3 * (i - 1)

    r_names[i]  <- as.character(r[, cols][1,1])
    r_list[[i]] <- as.data.frame(r[-1, cols])

    colnames(r_list[[i]]) <- c("Rank", "Player", "Metric")

    r_list[[i]]$Metric <- gsub(":00$", "", r_list[[i]]$Metric)
  }
  names(r_list) <- r_names

  return(r_list)
}

#' Print nicely formatted Rankings Tables.
#'
#' \code{print_board} takes a table given by \code{tab} and prints a
#'   simply-formatted version of that table. Tables are narrow enough to be
#'   displayed in the Slack app on an iPhone 6.
#'
#' @param tab A character input for the table to be displayed. Can be any of the
#'  following:
#'   \itemize{
#'     \item Average Times: \code{"AVERAGES"}, \code{"AVG"}, or \code{"A"}
#'     \item Personal Bests: \code{"BESTS"}, \code{"BEST"}, or \code{"B"}
#'     \item Games Played: \code{"GAMES"}, \code{"GAME"}, or \code{"G"}
#'     \item Monthly Leaderboard: \code{"MONTH"}, \code{"MO"}, or \code{"M"}
#'     \item All-Time Points: \code{"POINTS"}, \code{"POINT"}, or \code{"P"}
#'     \item All-Time Wins: \code{"WINS"}, \code{"WIN"}, or \code{"W"}
#'   }
#' @param given_only A boolean that determines whether or not to only print the
#'    player's given name in the table. Defaults to \code{TRUE}.
#' @inheritParams rank_list
#' @return A formatted table.
#' @export
print_board <- function(tab, r, given_only = TRUE){

  r_list <- rank_list(r)

  if (toupper(tab) %in% c("WINS",    "WIN",   "W")) { board <- "WINS" }
  if (toupper(tab) %in% c("POINTS",  "POINT", "P")) { board <- "POINTS" }
  if (toupper(tab) %in% c("AVERAGES","AVG",   "A")) { board <- "AVERAGE TIME" }
  if (toupper(tab) %in% c("GAMES",   "GAME",  "G")) { board <- "GAMES PLAYED" }
  if (toupper(tab) %in% c("BESTS",   "BEST",  "B")) { board <- "PERSONAL BEST" }
  if (toupper(tab) %in% c("MONTH",   "MO",    "M")) {
    board <- names(r_list)[grep("POINTS IN", names(r_list), ignore.case = TRUE)]
  }

  tmp <- r_list[[board]]
  names(tmp) <- c("Rank", "Player", capwords(board))
  if (given_only) {
    tmp$Player <- gsub("[ ].*", "", tmp$Player)
  }

  knitr::kable(tmp, format = "pandoc", align = c("r", "l", "r"))
}

#' Print nicely formatted Scoreboard
#'
#' \code{print_scoreboard} prints a simply-formatted version of the 'Dashboard'
#'   worksheet from the Daily Mini spreadsheet. Tables are narrow enough to be
#'   displayed in the Slack app on an iPhone 6.
#'
#' Technically, this function prints a simply-formatted version of the
#'   'simple_dashboard' worksheet. This worksheet is nearly identical to the
#'   'Dashboard' worksheet apart from some labeling and formatting differences.
#'   One major difference, however, is that 'Dashboard' sometimes reports
#'   multiple statistics for a single category (e.g., 'Total Points' and
#'   'Average Time'), while 'simple_dashboard' only reports the most relevant.
#'
#' @param s A data frame version of the "simple_dashboard" worksheet in the
#'   Daily Mini spreadsheet.
#' @inheritParams print_board
#' @return A formatted table.
#' @export
print_scoreboard = function(s, given_only = TRUE){

  tmp <- s

  if (given_only) {
    tmp$Player <- gsub("[ ].*", "", tmp$Player)
  }
  tmp[which(is.na(tmp[, 1])), 1] <- " "
  tmp[which(is.na(tmp[, 2])), 2] <- " "
  tmp[which(is.na(tmp[, 3])), 3] <- " "

  knitr::kable(tmp,
               format    = "html",#"pandoc",
               align     = c("r", "l", "r"),
               col.names = c(NULL, NULL, NULL))
}
