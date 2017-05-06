#' Write a Daily Mini time to the Google Sheet.
#'
#' \code{write_time} takes a time provided by a specified user and writes that
#'   time to the corresponding cell in a data spreadsheet given by \code{ss}.
#'
#' @param ss a googlesheet object (see \code{\link[googlesheets]{googlesheet}}).
#' @param d A data frame version of the "Data" worksheet in spreadsheet
#'   \code{ss} (see \code{\link[googlesheets]{googlesheet}}).
#' @param time Either an integer corresponding to a number of seconds elapsed
#'   (e.g., 42 or 142), or a character string in the form:
#'   \itemize{
#'     \item HH:MM:SS (e.g. \code{"01:23:45"})
#'     \item MM:SS    (e.g. \code{"23:45"})
#'     \item SS       (e.g. \code{"45"})
#'   }
#'   Any any number of hours, minutes, or seconds are valid (i.e.,
#'   \code{"0:03"}, \code{"000:3"}, \code{"3"}, and \code{"00:00:03"} are all
#'   valid ways to input three seconds).
#' @param user A character string providing a username, which should
#'   correspond to a column in the spreadsheet.
#' @param unames A user-defined \code{\link[base]{switch}} function that
#'   identifies user-specific column-names given a username (see
#'   \code{\link{write_time}} examples).
#' @param date an optional parameter to specify the date in a YYY-MM-DD format.
#'   Default is \code{Sys.Date()}.
#' @param shift An integer to define the number of days by which the data entry
#'   date should be shifted. Default is 0. The day after today (tomorrow) is +1,
#'   yesterday is -1, etc.
#' @param overwrite A boolean to determine whether existing values in the cell
#'   should be overwritten. Default is \code{FALSE}.
#' @return The function returns a message, stating which cell was written to,
#'   the value that was written, and if the cell was overwritten, a message
#'   to inform the user that the previous value was overwritten.
#'
#' @examples
#' \dontrun{
#'
#' unames <- function(user) {
#'   switch(user,
#'          user_1 = "A_User",
#'          user_2 = "B_User",
#'          user_3 = "C_User"
#'   )
#' }
#'
#' write_time(ss, d, "0:42", "user_1", unames = unames)
#' }
#' @export
write_time <- function(ss,
                       d,
                       time,
                       user,
                       unames,
                       date = Sys.Date(),
                       shift = 0,
                       overwrite = FALSE) {

  is_time <- try(format_time(time), silent = T)


  if ( ! is.na(read_cell(d, user, unames, date, shift))) {
    if( ! overwrite) {
      warning("This cell already has a value. To overwrite, include the flag -o")
    } else if (class(is_time) == "try-error") {
      edit_data(ss, d, time, user, unames, date, shift)
    } else {
      edit_data(ss, d, format_time(time), user, unames, date, shift)
    }
  } else if (class(is_time) == "try-error") {
    edit_data(ss, d, time, user, unames, date, shift)
  } else {
    edit_data(ss, d, format_time(time), user, unames, date, shift)
  }
}

#' Write a value to a Google Sheet.
#'
#' \code{edit_data} is a wrapper for \code{\link[googlesheets]{gs_edit_cells}}
#'   that writes \code{input} to a worksheet called 'Data', in a cell specified
#'   by \code{user} and \code{shift}.
#'
#'   This function is not called directly, but is a helper function for
#'   \code{\link{write_time}}.
#'
#' @inheritParams googlesheets::gs_edit_cells
#' @inheritParams write_time
#' @return NULL
edit_data <- function(ss, d, input, user, unames, date = Sys.Date(), shift=0) {
  googlesheets::gs_edit_cells(ss     = ss,
                              ws     = "Data",
                              input  = input,
                              anchor = get_cell(d, user, unames, date, shift))
}

#' Determine cell location in spreadsheet.
#'
#' \code{get_cell} finds a cell specified by a column that corresponds to
#'   \code{user} and a row specified by today's date, possibly
#'   shifted by \code{shift}.
#'
#'   This function is not called directly, but is a helper function for
#'   \code{\link{write_time}}.
#'
#' @inheritParams write_time
#' @return The function returns the alpha-numeric location of the cell (e.g,
#'   \code{"D42"}) as a character string.
get_cell <- function(d, user, unames, date = Sys.Date(), shift = 0) {
  paste0(LETTERS[which(colnames(d)[1:20] == unames(user))],
         which(d$dates == dm_date(date, shift)) + 1)
}

#' Read the current value in a specified cell.
#'
#' \code{read_cell} finds a cell specified by a column that corresponds to
#'   \code{user} and a row specified by today's date, possibly
#'   shifted by \code{shift}, and returns the corresponding value.
#'
#'   This function is not called directly, but is a helper function for
#'   \code{\link{write_time}}.
#'
#' @inheritParams write_time
#' @return The function returns the value in the cell.
read_cell <- function(d, user, unames, date = Sys.Date(), shift = 0){
  d[which(d$dates == dm_date(date, shift)),
    which(colnames(d)[1:20] == unames(user))]
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
  dow     <- weekdays(tmp_date, abbreviate = T)
  month   <- month.abb[as.numeric(strsplit(as.character(tmp_date), "-")[[1]][2])]
  day     <- as.numeric(strsplit(as.character(tmp_date), "-")[[1]][3])

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
    s_to_hms(as.numeric(tmp[1]) * 3600 + as.numeric(tmp[2]) * 60 + as.numeric(tmp[3]))
  }
}
