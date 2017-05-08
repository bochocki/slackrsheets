library(slackrsheets)

context("write_time")

GS <- get_ws("key.txt", "Data")
ss <- GS$ss
d  <- GS$ws

# a function to pull a range of cells from the spreadsheet.
testrange_data <- function(ss, d, range){
  cells <- paste0(get_cell(d, "X13", "2017-01-04", 0), ":",
                  get_cell(d, "X13", "2017-01-04", range-1))
  suppressWarnings(
    suppressMessages( {
      googlesheets::gs_read(ss,
                            ws        = "Data",
                            range     = cells,
                            verbose   = FALSE,
                            col_names = FALSE)[[1]]
    })
  )
}

test_that("get_cell gets the correct cell coordinates", {
  expect_equal(get_cell(d, "X13", "2017-01-04", -3)[[1]], "M6")
  expect_equal(get_cell(d, "X13", "2017-01-04",  0)[[1]], "M9")
  expect_equal(get_cell(d, "X13", "2017-01-04",  3)[[1]], "M12")
})

test_that("read_cell grabs the correct data", {
  expect_equal(read_cell(d, "X13", "2017-01-02", -1)[[1]], "1:10:42")
  expect_equal(read_cell(d, "X13", "2017-01-02",  0)[[1]], "fail")
  expect_equal(read_cell(d, "X13", "2017-01-02",  1)[[1]], "0:00:42")
})

tmp <- c("FAIL",
         ".fail",
         "fAiL. .",
         "0:00:42",
         "10:42:42",
         "0:01:00",
         "1:00:00")

test_that("edit_data writes data to a cell", {
  for (i in 1:length(tmp)) {
    suppressMessages(
      edit_data(ss, d, tmp[i], "X13", "2017-01-04", i - 1)
    )
  }
  expect_equal(testrange_data(ss, d, length(tmp)), tmp)
})

test_that("edit_data deletes values", {
  for (i in 1:length(tmp)) {
    edit_data(ss, d, " ", "X13", "2017-01-04", i - 1)
  }
  expect_equal(testrange_data(ss, d, length(tmp)), rep(NA, length(tmp)))
})

test_that("write_time writes data to a cell", {
  for (i in 1:length(tmp)) {
    suppressMessages(
      write_time(ss, d, tmp[i], "X13", "2017-01-04", i - 1, TRUE)
    )
  }
  expect_equal(testrange_data(ss, d, length(tmp)), tmp)
})

test <- list(c("00:00:42",  42),
             c("00:02:22",  142),
             c("00:02:22", "142"),
             c("00:02:22", "2:22"),
             c("00:02:22", "02:22"),
             c("00:02:22", "0:2:22"),
             c("00:02:22", "00:02:22"),
             c("01:10:42", "4242"),
             c("01:10:42", "1:10:42"))

test_that("write_time writes various time formats to a cell", {
  for (i in 1:length(test)) {
    suppressMessages(
      write_time(ss, d, test[[i]][2], "X13", "2017-01-04", i - 1, TRUE)
    )
  }
  expect_equal(as.character(testrange_data(ss, d, length(test))),
               unlist(test)[seq(1, 2 * length(test), by = 2)])
})

d <- get_ws("key.txt", "Data")$ws

test_that("write_time doesn't overwrite existing data when overwrite = F", {
  expect_match(
    write_time(ss, d, "Don't Panic.", "X13", "2017-01-04", 0, FALSE),
    "It looks like"
  )
})

test_that("write_time overwrites data in a cell when overwrite = T", {
  for (i in 1:length(test)) {
    write_time(ss, d, " ", "X13", "2017-01-04", i - 1, TRUE)
  }
  expect_equal(testrange_data(ss, d, length(test)), rep(NA, length(test)))
})
