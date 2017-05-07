library(slackrsheets)
context("date and time manipulations")

test_that("round_int provides the expected answers", {
  expect_equal(round_int(42.4),   42)
  expect_equal(round_int(42.5),   43)
  expect_equal(round_int(42.6),   43)
  expect_equal(round_int(-42.4), -42)
  expect_equal(round_int(-42.5), -42) # -42 is closer to 0 than -43
  expect_equal(round_int(-42.6), -43)
})

test_that("s_to_hms works for seconds, minutes, and hours", {
  expect_error(s_to_hms("a"))
  expect_equal(s_to_hms(42),       "00:00:42")
  expect_equal(s_to_hms("42"),     "00:00:42")
  expect_equal(s_to_hms(142),      "00:02:22")
  expect_equal(s_to_hms("142"),    "00:02:22")
  expect_equal(s_to_hms(4242),     "01:10:42")
  expect_equal(s_to_hms("4242"),   "01:10:42")
  expect_equal(s_to_hms(360102),   "100:01:42")
  expect_equal(s_to_hms("360102"), "100:01:42")
})

test_that("format_time works for variable inputs", {
  expect_error(format_time("a"))
  expect_equal(format_time("1:30:05"), "01:30:05")
  expect_equal(format_time(5405),      "01:30:05")
  expect_equal(format_time("5405"),    "01:30:05")
  expect_equal(format_time("30:3605"), "01:30:05")
  expect_equal(format_time("1.5:0:5"), "01:30:05")
})

test_that("dates are being formatted (and altered) correctly", {
  expect_equal(dm_date(date = "2042-04-02", shift =   0), "Wed, Apr 02")
  expect_equal(dm_date(date = "2042-04-02", shift =   1), "Thu, Apr 03")
  expect_equal(dm_date(date = "2042-04-02", shift =  -2), "Mon, Mar 31")
  expect_equal(dm_date(date = "2042-04-02", shift =   7), "Wed, Apr 09")
  expect_equal(dm_date(date = "2042-04-02", shift = 365), "Thu, Apr 02")
})
