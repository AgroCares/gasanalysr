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
