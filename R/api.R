# @param unames A user-defined \code{\link[base]{switch}} function that
#   identifies user-specific column-names given a username (see
#   \code{\link{write_time}} examples).
# @examples
# \dontrun{
#
# unames <- function(user) {
#   switch(user,
#          user_1 = "A_User",
#          user_2 = "B_User",
#          user_3 = "C_User"
#   )
# }
log <- function(text, user_name, response_url, channel_name) {

  source("unames.R")

  print(text)
  IN <- slack_parse(text, user_name)

  user_name  <- IN$user_name
  user_col   <- unames(user_name)
  user_given <- ssplit(user_col, " ")[1]

  GS <- get_ws("key.txt", "Data")

  # calibrate date to EST
  date <- as.Date(format(as.POSIXct(Sys.time(), tz = Sys.timezone()),
                         tz = "America/New_York",
                         usetz = TRUE))

  confirmation <- write_time(GS$ss, GS$ws,
                             IN$time, user_col,
                             date, IN$shift, IN$ow)

  if (!grepl("include the flag -ow", confirmation)) {

    # Post private message about time
    if (check_input(IN$time)) {
      slack_message(response_url,
                    channel = channel_name,
                    user_name = user_name,
                    text = paste0(confirmation, "\n",
                                  slack_text_pass(user_given, IN$time)),
                    private = TRUE)
    } else {
      slack_message(response_url,
                    channel = channel_name,
                    user_name = user_name,
                    text = paste0(confirmation, "\n",
                                  slack_text_fail(user_given)),
                    private = TRUE)
    }

    # Post public message about time
    slack_message(response_url,
                  channel = channel_name,
                  user_name = user_name,
                  text = slack_post_time(user_given, IN$time, IN$shift),
                  private = FALSE)

  } else {
    slack_message(response_url,
                  channel = channel_name,
                  user_name = user_name,
                  text = paste0(confirmation),
                  private = TRUE)
  }
}


dash <- function(text, user_name, response_url, channel_name){

  s <- get_ws("key.txt", "simple_dashboard")$ws

  slack_message(response_url,
                channel = channel_name,
                user_name = user_name,
                text = slack_table(print_scoreboard(s)),
                private = TRUE)
}

ore <- function(text, user_name, response_url, channel_name){

  s <- get_ws("key.txt", "simple_dashboard")$ws

  slack_message(response_url,
                channel = channel_name,
                user_name = user_name,
                text = slack_table(print_scoreboard(s)[1:6]),
                private = TRUE)
}

rank <- function(text, user_name, response_url, channel_name){

  text <- trimws(gsub("rank|ranks|ranking|rankings", "", text))

  r <- get_ws("key.txt", "Ranks")$ws

  slack_message(response_url,
                channel = channel_name,
                user_name = user_name,
                text = slack_table(print_board(text, r)),
                private = TRUE)
}

mini <- function(text, user_name, response_url, channel_name) {

  if (grepl("dash|scoreboard", text)) {

    print_call(user_name, "DASH", text)
    dash(text, user_name, response_url, channel_name)

  } else if (grepl("rank|ranks", text)) {

    print_call(user_name, "RANK", text)
    rank(text, user_name, response_url, channel_name)

  } else if (grepl("ore", text)) {

    print_call(user_name, "ORE", text)
    ore(text, user_name, response_url, channel_name)

  } else if (grepl("link", text)) {

    print_call(user_name, "LINK", text)
    slack_message(response_url,
                  channel = channel_name,
                  user_name = user_name,
                  text = get_link("key.txt"),
                  private = TRUE)

  } else if (grepl("help", text)) {

    print_call(user_name, "HELP", text)
    slack_message(response_url,
                  channel = channel_name,
                  user_name = user_name,
                  text = "Check out the help doc: https://github.com/bochocki/slackrsheets",
                  private = TRUE)

  } else if (grepl("talk", text)) {

    print_call(user_name, "TALK", text)
    slack_message(response_url,
                  channel = channel_name,
                  user_name = user_name,
                  text = "Sorry not right now, I have a headache.",
                  private = TRUE)

  } else {

    print_call(user_name, "LOG", text)
    log(text, user_name, response_url, channel_name)

  }
  return("Session finished.")
}
