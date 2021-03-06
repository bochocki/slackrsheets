#' A function to return time messages.
#'
#' These messages are displayed publically so everyone knows what time the user
#'   input.
#'
#' @param user_given A user's first (given) name.
#' @inheritParams write_time
#' @return A bespoke message
#' @export
slack_post_time <- function(user_given, time, shift) {
  if (shift ==  0) { shift_day <- "" }
  if (shift == -1) { shift_day <- " _(for yesterday)_" }
  if (shift ==  1) { shift_day <- " _(for tomorrow)_"  }
  if (shift <= -2) { shift_day <- sprintf(" _(for %s days ago)_"      , shift) }
  if (shift >= 2) { shift_day <- sprintf(" _(for %s days from today)_", shift) }

  return(sprintf("%s: %s%s", user_given, time, shift_day))
}

#' A function to return bespoke FAIL messages.
#'
#' When somebody fails, it hurts just a little bit. These messages are for them.
#'
#' @inheritParams slack_post_time
#' @return A random bespoke message that (often) includes the user's name.
#' @export
slack_text_fail <- function(user_given) {
  responses <- c(
    sprintf("It's a hard knock life, %s. :sweat:", user_given),
    sprintf("_Ouch_. Better luck next time %s. :face_with_head_bandage:", user_given),
    sprintf("Remember that poster of the kitten on the tree branch? :cat:\nHang in there %s.", user_given),
    sprintf("It's okay, maybe Brian failed today too. :grimacing:"),
    sprintf("No biggie, %s, don't sweat it. :sweat_smile:", user_given),
    sprintf("Sometimes, failure is ~not~ an option, I guess? :thinking_face:"),
    sprintf(">Dang, today's puzzle was _ruff_.\n>-a dog I heard on the street today :dog:"),
    sprintf("Welp, at least you had fun, right %s? :sob:", user_given),
    sprintf("I feel your pain, %s. I feel your pain. :weary:", user_given)
  )
  return(responses[sample(length(responses), 1)])
}

#' A function to return bespoke SUCCESS message.
#'
#' These people completed the puzzle for the day. Let's make them feel good!
#'
#' @inheritParams slack_post_time
#' @return A random bespoke message that (often) includes the user's name.
#' @export
slack_text_pass <- function(user_given, time) {

  secs <- hms_to_s(format_time(time))

  if (secs < 20) {
    responses <- c(
      sprintf("SLOW DOWN, %s, YOU'RE GONNA HURT SOMEBODY! :explody_parrot:", user_given),
      sprintf("_Damn_ %s, where's the fire?! :fire::fire::fire:", user_given),
      sprintf("How do you even read the clues that fast? :fast_parrot:"),
      sprintf("That's a _mighty_ fast time %s. We're gonna need you to pee into a cup. :beer:", user_given),
      sprintf("%s: Please contact CREVICE to verify your absurdly fast time. :sleuth_or_spy:", user_given),
      sprintf("%s: Slayer of Puzzles, Breaker of Hearts. :crossed_swords::shield:", user_given)
    )
  } else if (secs < 45) {
    responses <- c(
      sprintf("You might be looking at some ORE today, %s. :sports_medal:", user_given),
      sprintf("And :clap::skin-tone-3: The :clap::skin-tone-6: Crowd :clap::skin-tone-4: Goes :clap::skin-tone-2: Wild! :clap::skin-tone-5:"),
      sprintf("There's an electricity in the room, I can feel it. Ya done good %s.", user_given),
      sprintf("Well done, %s. Well done. :slowclap:", user_given),
      sprintf("You kinda crushed it today %s. :muscle::skin-tone-%i:", user_given, sample(2:6,1))
    )
  } else if (secs == 69) {
    responses <- c(
      sprintf("69 seconds, huh %s :wink:? lol, _nice _.", user_given)
    )
  } else if (secs < 120) {
    responses <- c(
      sprintf("Another day, another crossword. :%%2B1::skin-tone-%i:", sample(2:6,1)),
      sprintf("Good game, good game. :raised_hand_with_fingers_splayed::skin-tone-%i:", sample(2:6,1)),
      sprintf("Nothing more exciting than competive crossword puzzlin'!"),
      sprintf("Make sure to stretch before and after you _puzz_. :athletic_shoe:"),
      sprintf("You put in some _work_ today %s. :briefcase:", user_given)
    )
  } else if (secs < 300) {
    responses <- c(
      sprintf("This one was a real noggin-scratcher, huh? :bow:"),
      sprintf("I hope you're playing for fun today %s. :sweat_smile:", user_given),
      sprintf("Mr. Fagliano's up to his old tricks again, eh %s? :man::skin-tone-4:", user_given),
      sprintf("Geez, tough one today. :confused:"),
      sprintf("Wowzers Bowsers. Good game %s. :%%2B1::skin-tone-%i:", user_given, sample(2:6,1))
    )
  } else {
    responses <- c(
      sprintf("Well %s, at least you finished. :neutral_face:", user_given),
      sprintf("Well, you learned something new today I bet. Maybe? :confused:"),
      sprintf(">I got you %s. I got you good. _Your soul is mine._\n>Joel Fagliano :man::skin-tone-4:", user_given),
      sprintf("Well %s, you didn't fail today at least. Startin' the day off _right_ :%%2B1::skin-tone-%i:", user_given, sample(2:6,1)),
      sprintf("I'm proud of your perseverance %s. :clap::skin-tone-%i: You got a real sticktoitiveness.", user_given, sample(2:6,1)),
      sprintf("You're a winner in _my_ book.\nI'm just a lowly slackbot, and I don't have any books, but if I _did_ I'd definitely write %s and _IS A WINNER_ on the same page.", user_given)
    )
  }

  return(responses[sample(length(responses), 1)])
}

#' A function to build a message and POST it to Slack.
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
                          user_name = NULL,
                          text = NULL,
                          private = TRUE) {

  msg <- c("payload={")

  if (private) {
    msg <- c(msg, "\"response_type\": \"ephemeral\"")
  } else {
    msg <- c(msg, "\"response_type\": \"in_channel\"")
  }

  if (! is.null(channel)) {
    msg <- c(msg, sprintf("\"channel\": \"%s\"", channel))
  }

  if (! is.null(user_name)) {
    msg <- c(msg, sprintf("\"username\": \"%s\"", user_name))
  }

  if (! is.null(text)) {
    msg <- c(msg, sprintf("\"text\": \"%s\"", text))
  }

  msg <- c(msg, "}")

  ml <- length(msg)

  if (ml > 3) {
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

#' A function to build a message using Slack parameters and POST it to a URL.
#'
#' Given inputs, uses httr::POST to post a message to a URL.
#'
#' @param url The URL to post to (character string).
#' @param channel_name The channel to post the message to (character string).
#' @param user_name The username to post in response to (character string).
#' @param text The text that should be posted (character string).
#' @param response_url The response URL provided by Slack.
#' @return NULL
#' @export
slack_forward <- function(url, channel_name, user_name, text, response_url) {

  query <- sprintf("?text=%s&user_name=%s&response_url=%s&channel_name=%s",
                   text, user_name, response_url, channel_name)

  query <- gsub("[+]", "%2B", URLencode(query))

  h <- curl::new_handle(url = URLencode(paste0(url, query)))

  curl::multi_add(h)
  curl::multi_run(timeout = 0)
  curl::multi_cancel(h)

}

# function to format pandoc tables as code blocks in Slack
slack_table <- function(x) {
  paste0("```",
         paste0(matrix(x), collapse="\n"),
         "```")
}

#' A function to parse Slack input.
#'
#' Given text and a user_name, returns a list of variables needed for
#'   \code{link{write_time}}.
#'
#' @param text Input from Slack.
#' @param user_name The username provided by Slack.
#' @return A list containing variables required for \code{link{write_time}}.
#' @export
slack_parse = function(text, user_name){

  text <- trimws(text)

  # should existing cells be overwritten?
  ow <- grepl("-ow", text)
  if (ow) {
    text <- trimws(gsub("-ow", "", text))
  }

  # find out the number of days that the entry should be shifted
  shiftm <- NULL
  shiftp <- NULL

  if (grepl("+", text)) {
    shiftp <- ssplit(text," ")[grep("[+]", ssplit(text," "))]
    if (length(shiftp) > 1) {
      return(paste0("A `%2B` sign can only be used with one number to record ",
                  "the time for a future day. Tomorrow's time, for example, can",
                  " be  recorded using `%2B1`\n",
                  "Please alter your input and try again."))
    } else if (length(shiftp) == 0) {
      shiftp <- NULL
    } else {
      shiftp <- as.numeric(shiftp)
      text <- trimws(gsub(paste0(" [+]", shiftp), "", text))
    }
  }

  if (grepl("-", text)) {
    shiftm <- ssplit(text," ")[grep("[-]", ssplit(text," "))]
    if (length(shiftm) > 1) {
      return(paste0("A `-` sign can only be used with one number to record ",
                  "the dime for a future day. Yesterday's time, for example, ",
                  "can be recorded using `-1`\n",
                  "Please alter your input and try again."))
    } else if (length(shiftm) == 0) {
      shiftm <- NULL
    } else {
      shiftm <- as.numeric(shiftm)
      text <- trimws(gsub(paste0(" ", shiftm), "", text))
    }
  }

  if (is.null(shiftp) & is.null(shiftm)) {
    shift <- 0
  } else if (is.null(shiftp) & !is.null(shiftm)) {
    shift <- shiftm
  } else if (! is.null(shiftp) & is.null(shiftm)) {
    shift <- shiftp
  } else {
    return(paste0("The `%2B` and `-` signs, when followed by a number, can ",
                "only be used to record times on future and past dates.\n",
                "Please alter your input and try again."))
  }

  # find out if the user is posting for somebody else, and grab the time
  if (grepl(" for ", text)) {
    time <- trimws(ssplit(text, " for ")[1])

    user_name <- tolower(ssplit(trimws(ssplit(text, " for ")[2]), " ")[1])

    text <- trimws(gsub(" for ", "", text))
    text <- trimws(gsub(time, "", text))
    text <- trimws(gsub(user_name, "", tolower(text)))
  } else {
    time <- trimws(text)
  }

  return(list(time = time, user_name = user_name, shift = shift, ow = ow))

}
