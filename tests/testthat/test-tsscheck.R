# test tsscheck
path <- if(length(list.files("../testthat/"))>0){'../../'} else {'.'}

test_that('seal time is not thrown out upon tsscheck', {
  cat(getwd())
  dtm <- readRDS('tests/testdata/tsscheck_testdat1.RDS')
  dtmt <- tsscheck(dtm = dtm)
  expect_equal(dtmt[!is.na(startend)], dtmt[!is.na(stimedate)])
})
