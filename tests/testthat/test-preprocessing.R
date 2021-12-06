test_that('preprocessing samplekey works', {
  # load data
  load(paste0(system.file("testdata", package = 'gasanalysr'),
              '/samplekeytest.RData'))
  # test
  expect_equal(ppr_samplekey(dtkey), ndtkey)
})

test_that('preprocessing of measurements works', {
  # load data
  load(paste0(system.file("testdata", package = 'gasanalysr'),
              '/pprmeasurementtest.RData'))
  # test
  expect_equal(ppr_measurement(measurementsdt), nmeasdt)
})

# test that conversion for co2 works
test_that('converting ppm to mgm3 works with one gas at 20C', {
  testdt <- data.table(Timestamp = c('12:41:56', '12:42:00', '12:00:00'),
                       co2.ppm = c(500, 550, 1000))
  ansdt <- conv_ppm(dt = testdt, idcol = 'Timestamp', meascols = 'co2.ppm', temp = 20)
  expect_equal(ansdt$co2.mgm3,c(1829.540, 914.770, 1006.247),
               tolerance = 0.01)
})
