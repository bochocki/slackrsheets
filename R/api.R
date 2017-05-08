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

  cat(paste0(user_name, " called LOG at ", Sys.time(),"\n"))
  cat(paste0(user_name,"'s input was: ", text))

  source("unames.R")

  IN <- slack_parse(text, user_name)

  if(length(IN) == 1){
    slack_message(response_url,
                  channel = channel_name,
                  user_name = user_name,
                  text = IN,
                  private = TRUE)
  } else {

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

    # Post private confirmation that time was uploaded
    slack_message(response_url,
                  channel = channel_name,
                  user_name = user_name,
                  text = confirmation,
                  private = TRUE)

    if (!grepl("include the flag -ow", confirmation)) {

      # Post private message about time
      if (check_input(IN$time)) {
        slack_message(response_url,
                      channel = channel_name,
                      user_name = user_name,
                      text = slack_text_pass(user_given, IN$time),
                      private = TRUE)
      } else {
        slack_message(response_url,
                      channel = channel_name,
                      user_name = user_name,
                      text = slack_text_fail(user_given),
                      private = TRUE)
      }

      # Post public message about time
      slack_message(response_url,
                    channel = channel_name,
                    user_name = user_name,
                    text = slack_post_time(user_given, IN$time, IN$shift),
                    private = FALSE)

    }
  }
}

dash <- function(text, user_name, response_url, channel_name){

  cat(paste0(user_name, " called DASH at ", Sys.time(),"\n"))
  cat(paste0(user_name,"'s input was: ", text))

  s <- get_ws("key.txt", "simple_dashboard")$ws

  slack_message(response_url,
                channel = channel_name,
                user_name = user_name,
                text = slack_table(print_scoreboard(s)),
                private = TRUE)
}

rank <- function(text, user_name, response_url, channel_name){

  cat(paste0(user_name, " called RANK at ", Sys.time(),"\n"))
  cat(paste0(user_name,"'s input was: ", text))

  text <- trimws(gsub("rank|ranks|ranking|rankings", "", text))

  r <- get_ws("key.txt", "Ranks")$ws

  slack_message(response_url,
                channel = channel_name,
                user_name = user_name,
                text = slack_table(print_board(text, r)),
                private = TRUE)
}

mini <- function(text, user_name, response_url, channel_name) {

  slack_message(response_url,
                channel = channel_name,
                user_name = user_name,
                text = "",
                private = FALSE)

  slack_message(response_url,
                channel = channel_name,
                user_name = user_name,
                text = "",
                private = TRUE)

  if (grepl("dash|scoreboard", text)) {
    dash(text, user_name, response_url, channel_name)
  } else if (grepl("rank|ranks", text)) {
    rank(text, user_name, response_url, channel_name)
  } else if (grepl("help", text)){
    slack_message(response_url,
                  channel = channel_name,
                  user_name = user_name,
                  text = "Checkout the help doc: https://github.com/bochocki/slackrsheets",
                  private = TRUE)
  } else {
    log(text, user_name, response_url, channel_name)
  }
  return(invisible())
}
