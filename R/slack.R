# A function to return bespoke FAIL message
#'
#' When somebody fails, it hurts just a little bit. These messages are for them.
#'
#' @param u A user's name.
#' @return A bespoke message
#' @export
slack_text_fails <- function(u) {
  responses <- c(
    sprintf("It's a hard knock life, %s.", u),
    sprintf("_Ouch_. Better luck next time %s.", u),
    sprintf("Remember that poster of the kitten on the tree branch? Hang in there %s.", u),
    sprintf("It's okay, maybe Brian will fail today too."),
    sprintf("No biggie, %s, don't sweat it.", u),
    sprintf("Sometimes, failure is ~not~ an option, I guess?"),
    sprintf(">Dang, today's puzzle was _ruff_.\n>-a dog I heard on the street today"),
    sprintf("Welp, at least you had fun, right %s?", u),
    sprintf("I feel your pain, %s. I feel your pain.", u)
  )
  return(responses[sample(length(responses), 1)])
}

# A function build a message and POST it to Slack.
#'
#' Given inputs, uses httr::POST to post a message to Slack.
#'
#' @param url The \code{return_url} provided by Slack (character string).
#' @param channel The channel to post the message to (character string).
#' @param username The username to post in response to (character string).
#' @param text The text that should be posted (character string).
#' @param private Whether this message should be posted privately to the user
#'   (\code{TRUE}), or posted publically (\code{FALSE}).
#' @return NULL
#' @export
slack_message <- function(url,
                          channel = NULL,
                          username = NULL,
                          text = NULL,
                          private = TRUE) {

  msg <- c("payload={")

  if(private){
    msg <- c(msg, "\"response_type\": \"ephemeral\"")
  } else {
    msg <- c(msg, "\"response_type\": \"in_channel\"")
  }

  if (! is.null(channel)) {
    msg <- c(msg, sprintf("\"channel\": \"%s\"", channel))
  }

  if (! is.null(username)) {
    msg <- c(msg, sprintf("\"username\": \"%s\"", username))
  }

  if (! is.null(text)) {
    msg <- c(msg, sprintf("\"text\": \"%s\"", text))
  }

  msg <- c(msg, "}")

  ml <- length(msg)

  if(ml > 3) {
    msg <- paste0(msg[1],
                  paste0(msg[-c(1, ml - 1, ml)], collapse = ","),
                  ",",
                  paste0(msg[ml - c(1, 0)],      collapse = ""))
  }

  httr::POST(
    url = url,
    encode = "form",
    httr::add_headers(`Content-Type` = "application/x-www-form-urlencoded",
                      Accept         = "*/*"),
    body = URLencode(msg)
  )
}



# function to format tables
