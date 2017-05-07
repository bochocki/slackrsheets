#' Silently connect to and download a Google Sheet
#'
#' \code{get_ws} takes a \code{key} and a worksheet (\code{ws}) and silently
#'   returns the googlesheet object
#'   (see \code{\link[googlesheets]{googlesheet}}) and a tibble of the
#'   worksheet.
#'
#' @param key Path to a file that contains s unique key for the Google sheet.
#'   Should be readable by \code{link[base]{scan}} with \code{what="char}.
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
