test_that('preprocessing samplekey works', {
  # load data
  load(paste0(system.file("testdata", package = 'gasanalysr'),
              '/samplkeytest.RData'))
  # test
  expect_equal(ppr_samplekey(dtkey), ndtkey)
})

test_that('preprocessing preprocessing of measurements works', {
  # load data
  load(paste0(system.file("testdata", package = 'gasanalysr'),
              '/pprmeasurementtest.RData'))
  # test
  expect_equal(ppr_measurement(measurementsdt), nmeasdt)
})
