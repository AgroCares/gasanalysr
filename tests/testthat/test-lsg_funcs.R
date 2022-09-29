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
