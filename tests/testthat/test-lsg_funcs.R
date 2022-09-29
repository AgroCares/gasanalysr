# test functions for processing lumasoft data
require(data.table)
# dynamic file location
tloc <- if(length(list.files("../testdata/"))>0){"../testdata/"} else {'tests/testdata/'}

# test data copying
test_that('lsg_copy_raw_data() works',{
  # list files
  files_to_copy <- list.files(tloc)

  # copy files and get location
  new_path <- lsg_copy_raw_data(tloc)

  # check new folder was made
  expect_true(dir.exists(new_path))

  # check that all files have been copied
  expect_equal(files_to_copy, list.files(new_path))

  # cleanup copied mess
  unlink(new_path, recursive = TRUE)
})

# test lsg_xlsx_to_dt ====
test_that('lsg_xlsx_to_dt() can convert documents with channels per sheet', {
  # copy test data
  path_to_file <- lsg_copy_raw_data(tloc)

  # change file types
  lsg_xls_to_xlsx(path_to_file)

  # process channels per sheet file
  dt <- lsg_xlsx_to_dt(f = 'lsg_channels_per_sheet.xlsx', p = path_to_file)

  # load answer
  ans <- readRDS(paste0(tloc, 'answers/dt_channels_per_sheet.rds'))

  # expectation
  expect_identical(dt, ans)

  # cleanup copied mess
  unlink(path_to_file, recursive = TRUE)
})
test_that('lsg_xlsx_to_dt() can convert documents with gasses per sheet', {
  # copy test data
  path_to_file <- lsg_copy_raw_data(tloc)

  # change file types
  lsg_xls_to_xlsx(path_to_file)

  # process channels per sheet file
  dt <- lsg_xlsx_to_dt(f = 'lsg_gasses_per_sheet.xlsx', p = path_to_file)

  # load answer
  ans <- readRDS(paste0(tloc, 'answers/dt_gasses_per_sheet.rds'))

  # expectation
  expect_identical(dt, ans)

  # cleanup copied mess
  unlink(path_to_file, recursive = TRUE)
})
