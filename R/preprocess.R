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
  dt <- copy(dt)

  # check input
  checkmate::assert_data_table(dt)
  checkmate::assert_subset(names(dt), choices = c("monsterid", "sluittijd", "starttijd", "eindtijd", "sluit_tijddag", "start_tijddag",
                                                  "eind_tijddag", "sluit_dag", "start_dag", "eind_dag", "warn"))

  # remove warning column
  cols <- c(names(dt)[grep('id|jd|dag',names(dt))])
  dt <- dt[, ..cols]

  # remove rows without monsterid
  dt <- dt[!is.na(monsterid)]

  # get measurevars for melting of time-day columns
  measvars <- names(dt)[grep('tijddag$',names(dt))]

  # coerce to double if of type logical
  if(all(is.na(dt[,eind_tijddag]))) {
    dt <- dt[,eind_tijddag := as.double(eind_tijddag)]
    }

  # melt tijddag
  ddt <- melt(dt,
              measure.vars = measvars,
              variable.name = 'starteind',
              value.name = 'timedate')

  # take only relevant columns
  ddt <- unique(ddt[,.(monsterid, starteind, timedate)])

  # format starteind
  ddt <- ddt[,starteind := gsub('_.*$','', ddt$starteind)]

  # remove rows with eind an no time
  ddt <- ddt[!is.na(timedate)|!starteind == 'eind']

  # return output
  return(ddt)
}
