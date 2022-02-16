require(data.table)
test_that('preprocessing samplekey works', {
  # load data
  load(paste0(system.file("testdata", package = 'gasanalysr'),
              '/samplekeytest.RData'))
  # test
  expect_equal(ppr_samplekey(dtkey), ndtkey)
})

test_that('preprocessing of measurements works with units in ppm', {
  # load data
  load(paste0(system.file("testdata", package = 'gasanalysr'),
              '/pprmeasurementtest.RData'))
  # test
  expect_equal(ppr_measurement(measurementsdt), nmeasdt)
})

test_that('name formatting in ppr measurement works for units in mgm3', {
  # make data
  tdt <- data.table(
    Timestamp ='2021-11-17 09:44:39',
    CO2..mg.m3. = 900,
    H2O..mg.m3. = 1500,
    N2O..mg.m3. = 0.86,
    NH3..mg.m3. = 0.331,
    inlet = 1,
    x = NA
  )

  # test
  expect_equal(names(ppr_measurement(tdt)), c('Timestamp', 'co2.mgm3',
                                                   'h2o.mgm3', 'n2o.mgm3',
                                                   'nh3.mgm3'))
})

# test that conversion for co2 works
test_that('converting ppm to mgm3 works with one gas at 20C', {
  testdt <- data.table(Timestamp = c('12:41:56', '12:42:00', '12:00:00'),
                       co2.ppm = c(500, 550, 1000))
  ansdt <- conv_ppm(dt = testdt, idcol = 'Timestamp', meascols = 'co2.ppm', temp = 20)
  expect_equal(ansdt$co2.mgm3,c(1829.540, 914.770, 1006.247),
               tolerance = 0.01)
})
