#' Calculate concentration changes
#'
#' Calculates concentration changes between sealing and after incubation when a
#' reference sample is given as start concentration.
#'
#' @param dt (data.table)
#' @param startconc (character) character vector of sample_id's that act as pre-
#' incubation start concentration for all samples
calc_delta <- function(dt, startconc = NA) {
  # dt should be a data.table processed by fill_inter fill_inter may not be required if one only has single measurements
  # startconc should be a character or character vector indicating the sample_id of the background sample

  # Add global binding
  sample_id = startend = . = Timestamp = mmtime = gas = d_time = sealtime = NULL
  mmv = value = d_conc = d_rate = NULL

  # make sample_id string
  dt <- dt[, sample_id := as.character(sample_id)]

  # remove times that are not a measurement of a sample
  dt <- dt[!is.na(startend)]

  # get measurement columns
  val.cols <- names(dt)[!grepl('ime|date|startend|sample|inlet', names(dt))]

  # check if there are NA's in startconc when its a vector (shouldn't be)
  checkmate::assert_vector(startconc, any.missing = FALSE, min.len = 1,
                           max.len = uniqueN(dt$sample_id))

  # determine start concentrations if not already given
  # if(all(is.na(startconc))) {
  #   startconc <- head(dt[!is.na(get(val.cols[1])),..val.cols],1)
  # } else{

    # checks on startconc
    checkmate::assert_character(startconc)
    checkmate::assert_subset(startconc, choices = unique(dt$sample_id))

    # get startconc values
    startconc <- dt[sample_id %in% startconc & startend %in% c('start', 'inter', 'end')]
    # select measurement columns
    startconc <- startconc[,..val.cols]
    # average values
    startconc <- startconc[,lapply(.SD, function(x)mean(x,na.rm = TRUE)), .SDcols = val.cols]
  # }

  # melt startconc for later merging
  startconc.m <- melt(startconc, measure.vars = names(startconc), value.name = 'startconc')

  # melt dt
  dt.m <- melt(dt, measure.vars = val.cols, variable.name = 'gas', variable.factor = TRUE)

  # add seal time column
  stime <- dt.m[startend == 'seal',.(Timestamp, sample_id)] |> setnames('Timestamp', 'sealtime')
  stime <- unique(stime)
  dt.m <- merge(dt.m, stime, by = 'sample_id')

  # remove seal rows
  dt.m <- dt.m[!startend == 'seal']

  # calculate mean measurement time
  dt.m <- dt.m[,mmtime := mean(Timestamp), by = .(sample_id, gas)]

  # calculate time interval between closing and mean measurement time
  dt.m <- dt.m[, d_time := difftime(mmtime, sealtime, units = 'mins')]

  # calculate change in concentrations
  # merge with startconc.m
  dt.m <- merge(dt.m, startconc.m, by.x = 'gas', by.y = 'variable')

  # calculate mean measured value
  dt.m <- dt.m[, mmv := mean(value), by = .(sample_id, gas)]

  # calculate change in concentration
  dt.m <- dt.m[,d_conc := mmv - startconc]

  # calculate rate
  dt.m <- dt.m[,d_rate := d_conc/as.numeric(d_time)]

  # select relevant output columns
  out <- unique(dt.m[,.(sample_id, gas, sealtime, mmtime, d_time, startconc, mmv, d_conc,d_rate)])

  setorder(out, sample_id, gas)

  return(out)
}
