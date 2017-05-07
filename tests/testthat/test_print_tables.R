library(slackrsheets)
context("print_tables")

r <- get_ws("key.txt", "Ranks")$ws
s <- get_ws("key.txt", "simple_dashboard")$ws

test_that("capwords capitalized the first letter of each word", {
  expect_equal(capwords("don't panic."), "Don't Panic.")
  expect_equal(capwords("DON'T PANIC."), "Don't Panic.")
  expect_equal(capwords("dOn'T pAnIc."), "Don't Panic.")
})

test_that("rank_list generates a list of length 6", {
  expect_equal(length(rank_list(r)), 6)
})

board_headings <- function(tab, r){
  tmp <- print_board(tab, r, given_only = TRUE)
  unlist(strsplit(gsub("[ ]+", ",", trimws(tmp[1])),","))
}

test_that("print_board prints each board with the correct headings", {
  expect_equal(board_headings("A", r), c("Rank", "Player", "Average", "Time"))
  expect_equal(board_headings("B", r), c("Rank", "Player", "Personal", "Best"))
  expect_equal(board_headings("G", r), c("Rank", "Player", "Games", "Played"))
  expect_equal(board_headings("P", r), c("Rank", "Player", "Points"))
  expect_equal(board_headings("W", r), c("Rank", "Player", "Wins"))
  expect_equal(board_headings("M", r)[1:4], c("Rank", "Player", "Points", "In"))
})

test_that("print_board prints all tags within each group the same way", {
  expect_identical(print_board("A", r), print_board("AVG", r))
  expect_identical(print_board("A", r), print_board("AVERAGES", r))
  expect_identical(print_board("B", r), print_board("BEST", r))
  expect_identical(print_board("B", r), print_board("BESTS", r))
  expect_identical(print_board("G", r), print_board("GAME", r))
  expect_identical(print_board("G", r), print_board("GAMES", r))
  expect_identical(print_board("M", r), print_board("MO", r))
  expect_identical(print_board("M", r), print_board("MONTH", r))
  expect_identical(print_board("P", r), print_board("POINT", r))
  expect_identical(print_board("P", r), print_board("POINTS", r))
  expect_identical(print_board("W", r), print_board("WIN", r))
  expect_identical(print_board("W", r), print_board("WINS", r))
})

test_that("print_scoreboard prints a board with length of 33", {
  expect_equal(length(print_scoreboard(s)), 33)
})
