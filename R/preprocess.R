#' Prepare sample key
#'
#' Formats sample key data from a human usable excel file format to a long form
#' R usable format to identify seal start and end times of measurements.
#'
#' @param dt (data.table) A data.table containing the following columns:
#'   sampleid, sealtime, starttime, endtime, sealtimeday, starttimeday,
#'   endtimeday endday, warning.
#'
#' @import data.table
#'
#' @export
ppr_samplekey <- function(dt) {

  # add visual binding
  sample_id = end_timeday = startend = timedate = . = keepcols = NULL

  # check input
  checkmate::assert_data_table(dt)
  checkmate::assert_subset(names(dt),
                           choices = c("sample_id", "sealtime", "starttime",
                                       "endtime", "seal_timeday", "start_timeday",
                                        "end_timeday", "seal_day", "start_day", "end_day", "warn"))
  # remove warning column
  keepcols <- c(names(dt)[grep('id|time|day',names(dt))])
  dt <- dt[,..keepcols]

  # remove rows without sample_id
  dt <- dt[!is.na(sample_id),]

  # get measurevars for melting of time-day columns
  measvars <- names(dt)[grep('timeday$',names(dt))]

  # coerce to double if of type logical
  if(all(is.na(dt[,end_timeday]))) {
    dt <- dt[,end_timeday := as.double(end_timeday)]
    }

  # melt timeday
  ddt <- melt(dt,
              measure.vars = measvars,
              variable.name = 'startend',
              value.name = 'timedate')

  # take only relevant columns
  ddt <- unique(ddt[,.(sample_id, startend, timedate)])

  # format startend
  ddt <- ddt[,startend := gsub('_.*$','', ddt$startend)]

  # remove rows with end an no time
  ddt <- ddt[!is.na(timedate)|!startend == 'end']

  # remove end rows with timestamp duplicated in start rows
  ddt <- ddt[!(startend == 'end'& timedate %in% ddt[startend == 'start',timedate])]

  # return output
  return(ddt)
}
#' Prepare measurement data
#'
#' Formats measurement data. Specifically: Timestamp is turned into POSIXct and
#' redundant columns are removed.
#'
#' @param measurement.dt (data.table) A data.table with measurement data from
#' the gaserone analyser.
#' @param concunit (character) String denoting the unit of the measured
#' concentration, allowed units are ppm and mg/m3.
#'
#' @import data.table
#'
#' @export
ppr_measurement <- function(measurement.dt, concunit = NA_character_) {
  # data table of measurements with column names, can be obtained with:
  # measurement.dt <- read.delim('path/name.meas'), skip = 6) |> setDT()

  # add global binding
  ..cols = Timestamp = NULL

  # copy data table
  dt <- copy(measurement.dt)

  # check data table
  checkmate::assert_data_table(dt)

  # identify unit in which concentrations are expressed
  if(is.na(concunit)) {
    # check that at least one column with a concentration is present
    checkmate::assert_number(sum(grepl('ppm|mg[\\./]{0,1}m3', names(dt))),
                             lower = 1)

    # check that all concentration units are the same
      # number of mg/m3 columns
      mgm3cols <- sum(grepl('mg[\\./]{0,1}m3', names(dt)))
      # number of ppm columns
      ppmcols <- sum(grepl('ppm', names(dt)))
      # check that mgm3cols + ppmcols == max(mgm3cols, ppmcols)
      checkmate::assert_true(mgm3cols + ppmcols == max(mgm3cols, ppmcols))

    # select unit of concentrations
    concunit <- fifelse(mgm3cols > ppmcols, 'mgm3', 'ppm')
  }

  # grep columns with measurements and timestamps
  cols <- (names(dt)[grep(paste0('Time|O2|2O|H3|', concunit),names(dt))])
  dt <- dt[,..cols]

  # check if data is complete (at least a timestamp)
  # check that there is at most 1 Time column
  checkmate::assert_number(sum(grepl('Time', names(dt))), lower = 1, upper = 1)

  # format timestamp as posxct
  dt <- dt[,Timestamp := lubridate::ymd_hms(Timestamp)]

  # format column names
  setnames(dt, names(dt), gsub('\\.\\.', '\\.', names(dt)))
  setnames(dt, names(dt), gsub('\\.$', '', names(dt)))
  setnames(dt, names(dt)[grepl(concunit, names(dt))],
           tolower(names(dt)[grepl(concunit, names(dt))]))

  # return output
  return(dt)
}
#' Convert ppm
#'
#' Converts concentrations given as ppm to mg/m3. The unit ppm is ambiguous as
#' the number of parts in a volume depends on air pressure and temperature.
#'
#' @param dt (data.table) A data.table with gas concentrations expressed in ppm.
#' @param meascols (character) A character vector with columns that need
#' converting, only columns that contain just one of following gases in column
#'  name work: co2, h2o, nh3, or n2o
#' @param idcol (character) Column to identify unique measurements, most likely
#' you will want to use Timestamp for this
#' @param temp (numeric) Temperature in degrees C during measurement default = 25C
#' @param pressure (numeric) Air pressure in Pa, default is 1 atmosphere (1.01325*10^5)
#'
#' @import data.table
#'
#' @export
conv_ppm <- function(dt, idcol, meascols, temp = 25, pressure = 1.01325*10^5) {
  # get some data for calculations
  # molecular weight table
  mw.dt <- data.table(gas = c('co2', 'h2o', 'n2o', 'nh3'),
                      molarmass = c(44.0098, 18.0152, 44.01288, 17.03044))

  # molar gas constant
  R <- 8.314462618

  # volume
  V <- 1

  # convert temperature to K
  tempk <- temp+273.15


  # check inputs
  # check dt
  checkmate::assert_data_table(dt)

  # check idcol
  checkmate::assert_character(idcol, any.missing = FALSE)
  checkmate::assert_true(all(idcol) %in% names(dt))

  # check meascols are in dt
  checkmate::assert_character(miscol, any.missing = FALSE,
                              max.len = 4, min.len = 1)
  checkmate::assert_true(all(meascols %in% names(dt)))
  #check meascols start with allowed gases

  # check temp input
  checkmate::assert_numeric(temp, any.missing = FALSE, len = 1)

  # check pressure
  checkmate::assert_numeric(pressure, lower = 1.2, any.missing = FALSE, len =1)

  # convert ppm cols
  # fact_00C <- 22.14 # PLACEHOLDER needs to be looked up; molar volume is 22.14 at 0C with 1 atmosphere
  # fact_20C <- 24.055 # PLACEHOLDER needs to be looked up; molar volume is 24.055 at 20C with 1 atmosphere
  # fact_25C <- 24.465# PLACEHOLDER needs to be looked up; molar volume is 24.465 at 25C with 1 atmosphere

  # combine idcol and meascol to get vector of relevant cols
  cols <- c(idcol, meascols)

  # make dt to do calculations in
  sdt <- dt[,..cols]

  # melt sdt to get all measurements in one column
  sdt <- melt(sdt, id.vars = idcol)

  # identify gas
  sdt[grepl('co2', variable),gas := 'co2']
  sdt[grepl('h2o', variable),gas := 'h2o']
  sdt[grepl('n2o', variable),gas := 'n2o']
  sdt[grepl('nh3', variable),gas := 'nh3']

  # get vector of gasses in data
  gasses <- unique(sdt$gas)

  # add molar mass
  sdt <- merge(sdt, mw.dt, by = 'gas')

  # calculate value in mg/m3
  sdt[,mgm3 := (pressure*V/R/tempk/1000)*value*molarmass]

  # change variable to mgm3
  # sdt[,variable := gsub('ppm','mgm3', variable)]

  # dcast to
  sdt <- dcast(sdt, get(idcol) ~ gas, value.var = 'mgm3')

  # helper function to change columns in dt
    coloverwrite <- function(dt, gas) {
      # column name of gas
      gascoln <- names(dt)[grepl(gas, names(dt))]

      # make column in dt with converted data from sdt
      dt[,newdatacol := sdt[,get(gas)]]

      # change name to indicate change
      setnames(dt, gascoln, paste0(gas, '_mgm3'))



      # return
      return(dt)
    }

  # change dt columns
  for(i in gasses) {
    dt <- coloverwrite(dt = dt, gas = i)
  }



  dt[, .SDcols := lapply(.SD, function(x)x*fifelse(temp == 20, fact_20C, fact_25C)),
     .SDcols = meascols]



  return(dt)
}
