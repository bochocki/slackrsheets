update <- function(text, user_name, response_url, channel_name) {

  source("unames.R")

  user_str <- unlist(strsplit(unames(user_name)," "))[1]

  if (! check_input(text)) {
    resp <- httr::POST(url = response_url, encode = "form",
                       httr::add_headers(`Content-Type` = "application/x-www-form-urlencoded", Accept = "*/*"),
                       body = URLencode(sprintf("payload={\"response_type\": \"in_channel\",
                                                 \"channel\": \"%s\",
                                                 \"username\": \"%s\",
                                                 \"text\": \"%s: fail.\"}",
                                                channel_name, user_name, user_str)))

    resp <- httr::POST(url = response_url, encode = "form",
                       httr::add_headers(`Content-Type` = "application/x-www-form-urlencoded", Accept = "*/*"),
                       body = URLencode(sprintf("payload={\"response_type\": \"ephemeral\",
                                                 \"channel\": \"%s\",
                                                 \"username\": \"%s\",
                                                 \"text\": \"_Ouch. Better luck next time %s._\"}",
                                                channel_name, user_name, user_str)))
  } else {
    date_str <- slackrsheets::dm_date()
    time_str <- slackrsheets::format_time(text)

    resp <- httr::POST(url = response_url, encode = "form",
                       httr::add_headers(`Content-Type` = "application/x-www-form-urlencoded", Accept = "*/*"),
                       body = URLencode(sprintf("payload={\"response_type\": \"in_channel\",
                                                  \"channel\": \"%s\",
                                                  \"username\": \"%s\",
                                                  \"text\": \"Good hustle %s. Got you down for %s on %s.\"}",
                                                channel_name, user_name, user_str, time_str, date_str)))

  }

  GS <- slackrsheets::get_ws("key.txt", "Data")

  return(
    invisible(
      slackrsheets::write_time(GS$ss, GS$ws, text, user_name, unames)
    )
  )
}

dash <- function(response_url){

  print("User called dash")

  s <- slackrsheets::get_ws("key.txt", "simple_dashboard")$ws

  return(paste0(paste0(matrix(slackrsheets::print_scoreboard(s)),collapse="\n")," "))
}

rank <- function(text, response_url){

  print("User called rank")

  r <- slackrsheets::get_ws("key.txt", "Ranks")$ws

  return(paste0(paste0(matrix(slackrsheets::print_board(text, r)),collapse="\n")," "))
}
