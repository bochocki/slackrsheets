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

  print(paste0(user_name, " called UPDATE at ", Sys.time()))
  print(paste0(user_name,"'s input was: ", text))

  source("unames.R")

  IN <- slack_parse(text, user_name)


  user_name  <- IN$user_name
  user_col   <- unames(user_name)
  user_given <- ssplit(user_col, " ")[1]

  GS <- get_ws("key.txt", "Data")

  confirmation <- write_time(GS$ss, GS$ws,
                             user_col, user_given,
                             IN$time, IN$shift, IN$ow)

  # Post public message about time
  slack_message(response_url,
                channel = channel_name,
                user_name = user_name,
                text = slack_post_time(user_given, IN$time, IN$shift),
                private = FALSE)

  # Post private confirmation that time was uploaded
  slack_message(response_url,
                channel = channel_name,
                user_name = user_name,
                text = confirmation,
                private = TRUE)

  # Post private message about time
  if (check_input(time)) {
    slack_message(response_url,
                  channel = channel_name,
                  user_name = user_name,
                  text = slack_text_fail(user_given, IN$time),
                  private = TRUE)
  } else {
    slack_message(response_url,
                  channel = channel_name,
                  user_name = user_name,
                  text = slack_text_fail(user_given),
                  private = TRUE)
  }
}

dash <- function(text, user_name, response_url){

  print(paste0(user_name, " called DASH at ", Sys.time()))
  print(paste0(user_name,"'s input was: ", text))

  s <- slackrsheets::get_ws("key.txt", "simple_dashboard")$ws

  return(paste0(paste0(matrix(slackrsheets::print_scoreboard(s)),collapse="\n")," "))
}

rank <- function(text, user_name, response_url){

  print(paste0(user_name, " called RANK at ", Sys.time()))
  print(paste0(user_name, "'s input was: ", text))

  text <- gsub("rank|ranks|rankings", "", text)
  print(text)
  r <- slackrsheets::get_ws("key.txt", "Ranks")$ws

  return(paste0(paste0(matrix(slackrsheets::print_board(text, r)),collapse="\n")," "))
}

mini <- function(text, user_name, response_url, channel_name) {

  slack_message(response_url,
                channel = channel_name,
                user_name = user_name,
                text = "I'm on it.",
                private = TRUE)

  if (grepl("dash|scoreboard", text)) {
    dash(text, user_name, response_url)
  } else if (grepl("rank|ranks", text)) {
    rank(text, user_name, response_url)
  } else {
    log(text, user_name, response_url, channel_name)
  }
}
