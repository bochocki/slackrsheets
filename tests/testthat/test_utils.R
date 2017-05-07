library(slackrsheets)
context("utilities")

test_that("get_ws returns the correct argument", {
  GS <- get_ws("key.txt", "Ranks")
  expect_equal(length(GS), 2)
  expect_equal(class(GS), "list")
  expect_equal(class(GS[[1]]), c("googlesheet", "list"))
  expect_equal(class(GS[[2]]), c("tbl_df", "tbl", "data.frame"))
})

test <- list(c(42,         TRUE),
             c("42",       TRUE),
             c("162",      TRUE),
             c("0:42",     TRUE),
             c("1:05",     TRUE),
             c("01:05",    TRUE),
             c("2:1:5",    TRUE),
             c("2:1:05",   TRUE),
             c("2:01:05",  TRUE),
             c("02:01:05", TRUE),
             c("fail",     FALSE),
             c("f41l",     FALSE),
             c("FAIL",     FALSE),
             c("FAIL.",    FALSE),
             c("FAIL!",    FALSE))

test_that("check_input returns the correct boolean", {
  for (i in 1:length(test)) {
    expect_equal(check_input(test[[i]][1]), as.logical(test[[i]][2]))
  }
})
