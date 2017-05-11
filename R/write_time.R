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
#' @param user_col A character string providing a username, which should
#'   correspond to a column in the spreadsheet.
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
#' @export
write_time <- function(ss,
                       d,
                       time,
                       user_col,
                       date = Sys.Date(),
                       shift = 0,
                       overwrite = FALSE) {

  entry <- read_cell(d, user_col, date, shift)

  if ( ! is.na(entry)) {
    if( ! overwrite) {
      out <- paste0("It looks like %s already has an entry for %s: %s.\n",
                    "To overwrite it, include the flag -ow, like this:\n",
                    "`/mini [time]. -ow`\n",
                    "To post a time for tomorrow, include a '%%2B1' in your call,",
                    "like this:\n `/mini [time] %%2B1`.")
      out <- sprintf(out,
                     ssplit(user_col, " ")[1],
                     dm_date(date, shift),
                     entry)
    } else {
      out <- "Logged %s for %s on %s and overwrote the previous entry."
      out <- sprintf(out,
                     edit_data(ss, d, time, user_col, date, shift),
                     ssplit(user_col, " ")[1],
                     dm_date(date, shift))
    }
  } else {
    out <- "Logged %s for %s on %s."
    out <- sprintf(out,
                   edit_data(ss, d, time, user_col, date, shift),
                   ssplit(user_col, " ")[1],
                   dm_date(date, shift))
  }
  return(out)
}

#' Write a value to a Google Sheet.
#'
#' \code{edit_data} is a wrapper for \code{\link[googlesheets]{gs_edit_cells}}
#'   that writes \code{input} to a worksheet called 'Data', in a cell specified
#'   by \code{user_col} and \code{shift}.
#'
#'   This function is not called directly, but is a helper function for
#'   \code{\link{write_time}}.
#'
#' @inheritParams googlesheets::gs_edit_cells
#' @inheritParams write_time
#' @return Returns the version of \code{input} that was uploaded, either an
#'   HH:MM:SS transformation of the input, or the input as a character string.
edit_data <- function(ss, d, input, user_col, date = Sys.Date(), shift=0) {
  if (check_input(input)) {
    input <- format_time(input)
  }

  googlesheets::gs_edit_cells(ss     = ss,
                              ws     = "Data",
                              input  = input,
                              anchor = get_cell(d, user_col, date, shift))
  return(input)
}

#' Determine cell location in spreadsheet.
#'
#' \code{get_cell} finds a cell specified by a column that corresponds to
#'   \code{user_col} and a row specified by today's date, possibly
#'   shifted by \code{shift}.
#'
#'   This function is not called directly, but is a helper function for
#'   \code{\link{write_time}}.
#'
#' @inheritParams write_time
#' @return The function returns the alpha-numeric location of the cell (e.g,
#'   \code{"D42"}) as a character string.
get_cell <- function(d, user_col, date = Sys.Date(), shift = 0) {
  paste0(LETTERS[which(colnames(d)[1:20] == user_col)],
         which(d$dates == dm_date(date, shift)) + 1)
}

#' Read the current value in a specified cell.
#'
#' \code{read_cell} finds a cell specified by a column that corresponds to
#'   \code{user_col} and a row specified by today's date, possibly
#'   shifted by \code{shift}, and returns the corresponding value.
#'
#'   This function is not called directly, but is a helper function for
#'   \code{\link{write_time}}.
#'
#' @inheritParams write_time
#' @return The function returns the value in the cell.
read_cell <- function(d, user_col, date = Sys.Date(), shift = 0){
  d[which(d$dates == dm_date(date, shift)),
    which(colnames(d)[1:20] == user_col)]
}
