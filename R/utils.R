#' Silently connect to and download a Google Sheet.
#'
#' \code{get_ws} takes a \code{key} and a worksheet (\code{ws}) and silently
#'   returns the googlesheet object
#'   (see \code{\link[googlesheets]{googlesheet}}) and a tibble of the
#'   worksheet.
#'
#' @param key Path to a file that contains a unique key for the Google sheet.
#'   Should be readable by \code{link[base]{scan}} with \code{what="char"}.
#' @param ws A character string specifying title of the worksheet.
#' @return The function returns a list. The first element of the list, \code{ss}
#'   is a googlesheet object, the second element, \code{ww}, is a tibble of the
#'   worksheet.
#' @export
get_ws <- function(key, ws) {

  k <- scan(key, what = "char", quiet = TRUE)

  suppressWarnings(
    suppressMessages( {
      ss <- googlesheets::gs_key(k,lookup=TRUE)
      ww <- googlesheets::gs_read(ss, ws=ws, verbose=FALSE)
    })
  )
  return(list(ss = ss, ws = ww))
}

get_link <- function(key){

  paste0("Daily Mini Spreadsheet: ",
         "https://docs.google.com/spreadsheets/d/",
         scan(key, what = "char", quiet = TRUE),
         "/edit#gid=0")
}

print_call <- function(user_name, fun, text) {
  cat(
    paste0(
      paste0(user_name, " called ", fun, " at ", Sys.time(), "\n"),
      paste0(user_name,"'s input was: ", text, "\n")
    )
  )
}
#' Check whether a user input is a time or a comment.
#'
#' \code{check_input} takes a user's input and determines whether or not it can
#'   be coerced to a time via \code{format_time}.
#'
#' @param input A numeric or charater string.
#' @return Returns \code{TRUE} if the input could be coerced to a time,
#'   \code{FALSE} otherwise.
#' @export
check_input <- function(input) {
  ! class(try(format_time(input), silent = T)) == "try-error"
}

#' Splits a string.
#'
#' \code{ustrsplit} is a wrapper for \code{[base]{strsplit}} that only accepts a
#'   single character string, but in exchange, retuns a vector instead of a
#'   list.
#'
#' @param x A character string.
#' @param split character containing a regular expression that can be used for
#'   splitting.
#' @return Returns a character vector.
#' @export
ssplit = function(x, split){
  unlist(strsplit(x, split))
}

#' Change System Date to Google Sheets date.
#'
#' \code{dm_date} converts the output from R's \code{Sys.date()} to the format
#'   currently used in our Daily Mini Google Spreadsheet. This spreadsheet
#'   format uses an abbreviated day of the week, an abbreviated month, and the
#'   numeric day of the month.
#' @inheritParams write_time
#' @return The function returns a character string of the re-formatted date. For
#'   example, the \code{Sys.Date()} output on April 2, 2017 is
#'   \code{"2017-04-02"}, and the newly returned character string would be
#'   \code{"Sun, Apr 02"}.
#' @examples
#' dm_date()    # today
#' dm_date(shift = 1)   # tomorrow
#' dm_date(shift = -7)  # one week ago
#' @export
dm_date <- function(date = Sys.Date(), shift = 0) {
  tmp_date <- as.Date(date) + shift
  dow   <- weekdays(tmp_date, abbreviate = T)
  month <- month.abb[as.numeric(strsplit(as.character(tmp_date), "-")[[1]][2])]
  day   <- as.numeric(strsplit(as.character(tmp_date), "-")[[1]][3])

  return(paste0(dow, ", ", month, " ", sprintf("%02d", day)))
}

#' Round down to the nearest integer.
#'
#' \code{round_int} takes a numeric input \code{x} and rounds each element
#'   to the nearest integer. Ties are broken using a 'half-up' rule. This is
#'   different from R's default rounding method, which breaks ties by rounding
#'   to the nearest even number (i.e. 'half-even').
#'
#' @param x A numeric vector.
#' @return The function returns the vector \code{x} rounded to the nearest
#'   integer.
#' @examples
#' round_int(2.5)  # rounding 'half-up' returns 3
#' round(2.5)      # rounding 'half-even', R's default, return 2
#' @export
round_int <- function(x) {
  floor( x + 0.5 )
}

#' Convert some number of seconds into an HH:MM:SS format.
#'
#' \code{s_to_hms} takes a numeric input \code{s} (or an input that can be
#'   coerced to numeric) that corresponds to some number of seconds, and returns
#'   a string that is formatted to the appropriate \code{HH:MM:SS}.
#'
#' @param x A single number, in seconds.
#' @return a character string of the form \code{HH:MM:SS}.
#' @examples
#' s_to_hms(2.5)
#' s_to_hms(120)
#' s_to_hms(4242)
#' @export
s_to_hms <- function(x) {
  if(is.na(suppressWarnings(as.numeric(x)))) {
    stop("s must be numeric or able to be coerced to numeric by as.numeric()")
  }
  if(x <= 0) {
    stop("s must be positive")
  }

  x <- as.numeric(x)

  hh <- floor(x / 3600)
  mm <- floor((x - hh * 3600) / 60)
  ss <- round_int(x - hh * 3600 - mm * 60)
  if (ss == 60){
    ss <- 0
    mm <- mm + 1
  }
  if (mm == 60){
    mm <- 0
    hh <- hh + 1
  }
  out <- paste0(sprintf("%02d", hh),":",
                sprintf("%02d", mm),":",
                sprintf("%02d", ss))
  return(out)
}

#' Convert an HH:MM:SS formatted string to seconds.
#'
#' \code{hms_to_s} takes a character string formatted in \code{HH:MM:SS} and
#'   returns the equivalent number of seconds.
#'
#' @param x A character string of the form \code{HH:MM:SS}.
#' @return The number of seconds (numeric) represented by \code{HH:MM:SS}.
#' @examples
#' hms_to_s(10:10:10)
#' hms_to_s(00:10:10)
#' hms_to_s(00:00:10)
#' @export
hms_to_s <- function(x){
  return(sum(as.numeric(ssplit(x, ":")) * c(3600, 60, 1)))
}


#' Standardize some input time to an HH:MM:SS format.
#'
#' \code{format_time} takes either a numeric or character input \code{s},
#'   corresponding to some time, and returns a string that is formatted to a
#'   consistent \code{HH:MM:SS} format.
#'
#'\code{format_time} is flexible with regard to the format of \code{time},
#'   but if hours and/or minutes are supplied in addition to seconds, they must
#'   be some form where mintues precede seconds, and hours precede minutes.
#'   Thus, to indicate 1 hour, 30 minutes, 5 seconds, the following values of
#'   \code{time} are all acceptable: \code{"1:30:05"}, \code{5405},
#'   \code{"5405"}, \code{"30:3605"}, \code{"1.5:0:5"}. However,
#'   \code{1.5:05}, is not.
#'
#' @param time Either a single numeric, corresponding to some number of seconds,
#'   or a character string where hours, minutes, and/or seconds are separated by
#'   a colon (\code{:}).
#'
#' @return a character string of the form \code{HH:MM:SS}.
#' @examples
#' format_time(2.5) # fractions are okay
#'
#' # Many different ways to express "01:30:05":
#' format_time("1:30:05")  # 1 hour, 30 minutes, 5 seconds
#' format_time(5405)       # 5405 seconds (numeric)
#' format_time("5405")     # 5405 seconds (string)
#' format_time("30:3605")  # 30 minutes, 3605 seconds
#' format_time("1.5:0:5")  # 1.5 hours, 0 minutes, 5 seconds
#'
#' # Order is important
#' format_time("1.5:5")    # 1.5 minutes, 5 seconds
#' @export
format_time <- function(time) {
  tmp <- unlist(strsplit(as.character(time), ":"))
  if (length(tmp) == 1) {
    s_to_hms(as.numeric(tmp))
  } else if (length(tmp) == 2) {
    s_to_hms(as.numeric(tmp[1]) * 60 + as.numeric(tmp[2]))
  } else if (length(tmp) == 3) {
    s_to_hms(as.numeric(tmp[1]) * 3600 +
               as.numeric(tmp[2]) * 60 + as.numeric(tmp[3]))
  }
}
