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
  sample_id = end_timeday = startend = timedate = . = NULL

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

  # remove rows with eind an no time
  ddt <- ddt[!is.na(timedate)|!startend == 'eind']

  # return output
  return(ddt)
}

