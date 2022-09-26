#' Merge preprocessed data.tables by timestamps
#'
#' Merge measured data to sample id's and measurement descriptions based on the
#' timestamps of the measurements.
#'
#' @param sdt (data.table) A data.table containing sample data such as ID's, seal
#' start and end times, description
#' @param mdt (data.table) A data.table with the measured data and timestamps
#' @param timestamp_check (logical) Should a check and correction be performed
#' on the merged timestamps using \code{\link{tsscheck}}
#'
#' @import data.table
#' @export
merge_times <- function(sdt, mdt, timestamp_check = TRUE){
  # sdt should be a data.table with columns sample_id, startend, and time (year, month, day, hour, minute)
  # mdt should be a data.table with at least columns Timestamp and a measurement column
  sdt <- copy(sdt)
  mdt <- copy(mdt)

  # add global binding
  stimedate = timedate = Timestamp = NULL

  # checks on sdt
  checkmate::assert_data_table(sdt,min.cols = 3)
  checkmate::assert_true(all(c('sample_id', 'startend', 'timedate') %in% names(sdt)))
  checkmate::assert_character(sdt$startend)
  checkmate::assert_subset(sdt$startend, choices = c('seal', 'start', 'end'))
  checkmate::assert_posixct(sdt$timedate)

  # format sdt stimedate as ymdhm
  sdt <- sdt[,stimedate := format(as.POSIXct(timedate, tz = 'UTC'), format = "%Y-%m-%d %H:%M")]

  # checks on mdt
  checkmate::assert_data_table(mdt, min.cols = 2)
  checkmate::assert_true(all(c('Timestamp' %in% names(mdt)))) # check if timestamp is present
  checkmate::assert_posixct(mdt$Timestamp, any.missing = FALSE)
  checkmate::assert_true(any(c('co2.ppm', 'h2o.ppm', 'n2o.ppm', 'nh3.ppm',
                               'co2.mgm3', 'h2o.mgm3', 'n2o.mgm3', 'nh3.mgm3') %in% names(mdt)))

  # format Timestamp as ymdhm
  mdt <- mdt[,Timestamp := format(as.POSIXct(Timestamp, tz = 'UTC'), format = "%Y-%m-%d %H:%M")]
  mdt <- mdt[,Timestamp := lubridate::ymd_hm(Timestamp, tz = 'UTC')]

  # merge monsternrs with measurements
  dtm <- merge(mdt, sdt, by.x = 'Timestamp', by.y = 'timedate', all.x = TRUE, all.y = TRUE)

  # order dtm op tijd
  setorder(dtm, 'Timestamp')

  # optionally check if timestamps have been shifted and need to be reshifted
  if(timestamp_check) {
    dtm <- tsscheck(dtm = dtm)
  }

  # return merged dt
  return(dtm)
}
#' Check timestamp shifting
#'
#' The gasanalyser displays a different timestamp for a measurement during
#' measuring than the actual timestamp as given in the output file. This is
#' inconvenient for anyone operating the machine on multiple samples in a single
#' measurement round where one has to manually log timestamps to match measurement
#' points with samples.
#'
#' This function attempts to guess whether the operator manually corrected for
#' disparity between displayed and actual measurements -the merge should have
#' been smooth, no correction is needed- or that some post measurement correcting
#' is needed. This is done by comparing the H2O concentrations measured at the
#' beginning of a measurement round (before the first timestamp is matched to a
#' sample) with the H2O concentrations at times merge to a sample. It is assumed
#' that the humidity of an actual measurement is higher than that of the ambient
#' room. So if a sample has comparable humidity to the background it is likely
#' that shifting has taken place and the samples actual measurement was actually
#' the previous measurement.
#'
#' @param dtm (data.table) data table to be checked
#' @param max.amb.h2o (numeric) Maximum background H2O concentration in ppm
#' is 20000 by default
#'
#' @export
tsscheck <- function(dtm, max.amb.h2o = 20000) {
  dtm <- copy(dtm)

  # add global binding
  fmr = max.backg.h2o = endrows = n.likelywrongstart = startrows = n.time = NULL
   h2o.col = startend = sample_id = stimedate = n.startend = n.sample_id = NULL

   # identify a column name with h2o in it
   h2o.col <-  names(dtm)[grepl('h2o',names(dtm))]

   # check h2o col has allowed units
   checkmate::assert_subset(h2o.col, choices = c('h2o.ppm', 'h2o.mgm3'),
                            empty.ok = FALSE)

  # checking of merging timestamps was successful or that timestamps have been shifted by 2 minutes
  if(!h2o.col %in% names(dtm)) {
    warning('column with h2o is missing, a check for correct Timestep merging cannot be performed')
  } else{
    # identify first measurement row
    fmr <- min(which(dtm[!is.na(get(h2o.col)),startend] == 'start'))
    # determine maximum background h2o concentration before measuring samples
    max.backg.h2o <- max(dtm[!is.na(get(h2o.col)),get(h2o.col)][1:(fmr-2)])

    # check if max.backg.h2o isn't very high
    if(max.backg.h2o > max.amb.h2o) {
      warning(paste0('maximum background h2o concentration seems to be over ',
                     max.amb.h2o,
                     'ppm, checking correct Timestep cannot be continued'))
    } else{
      # number of start measurements with h2o concentrations within 10% of max background h2o
      n.likelywrongstart <- nrow(dtm[startend == 'start' &
                                       !is.na(sample_id)&
                                       get(h2o.col) < 1.1*max.backg.h2o])
      if(n.likelywrongstart>0) {
        message('At least one measurement seems to have low h2o concentrations at start, attempting to shift rows')
        # reducing timestamp by 2 minutes is not smart, because there isn't always two minutes between timestamps, would be better to improve imput data or merge with adjusted index (taking other row)

        # split seal rows from dtm
        dtm_seal <- dtm[startend == 'seal']

        # making new startend column
        dtm_seal[,c('n.startend', 'n.sample_id', 'n.time') := list('seal',sample_id, stimedate)]

        # remove seal data from dtm
        dtm <- dtm[startend == 'seal', c('sample_id', 'startend', 'stimedate') := NA]

        # remove duplicated data rows in case of multiple seal rows coinciding with the time of a measurement
        dtm <- unique(dtm)

        # remove rows without measurements
        dtm <- dtm[!is.na(get(h2o.col))]

        # remove rows with a duplicated Timestamp without sample_id
        dtm <- dtm[!(is.na(sample_id) & Timestamp %in% dtm[!is.na(sample_id), Timestamp])]

        # get rows indices with start and end
        startrows <- which(dtm[,startend] == 'start')
        endrows  <- which(dtm[,startend] == 'end')

        # new start
        dtm[startrows-1, c('n.startend', 'n.sample_id','n.time') :=
              list('start', dtm[startrows,sample_id], dtm[startrows, stimedate])]
        # new end
        dtm[endrows-1,  c('n.startend', 'n.sample_id','n.time') :=
              list('end', dtm[endrows,sample_id], dtm[endrows, stimedate])]

        # combine dtm and dtm_seal again
        dtm <- rbindlist(list(dtm, dtm_seal))
        setorder(dtm, Timestamp)

        # checking new start concentrations
        n.likelywrongstart <- nrow(dtm[n.startend == 'start'&!
                                         is.na(n.sample_id)&
                                         get(h2o.col) <1.1*max.backg.h2o])
        # checking new end concentrations
        n.likelywrongend <- nrow(dtm[n.startend == 'end'&!
                                       is.na(n.sample_id)&
                                       get(h2o.col) <1.1*max.backg.h2o])

        # warning when there still seems to be something wrong
        if(n.likelywrongstart>0) {
          warning(paste('There are still start rows with h2o concentration within 10% of background (which is ', max.backg.h2o,'), so youll have to look into why yourself this is for samples with sample_id',
                        list(dtm[n.startend == 'start'&!
                                   is.na(n.sample_id)&
                                   get(h2o.col) <1.1*max.backg.h2o, n.sample_id])))
          # message condition 1
        }else{c1 <- 1}
        if(n.likelywrongend>0) {
          warning('There are still end rows with h2o concentration within 10% of background, so youll have to look into why yourself')
          # message concditoin 2
        } else{c2 <- 1}
        if(all(exists('c1')&
               exists('c2'))) {
          message('shifting of timestamps seems to have been succesfull, no start or end rows have low h2o concentrations anymore')
          }

        # overwrite sample_id and startend with adjusted columns
        dtm[,c('sample_id', 'startend','stimedate') := list(n.sample_id, n.startend, n.time)]
      }
    }
  }
   # remove n columns
   ncols <- names(dtm)[grepl('^n\\.', names(dtm))]
   kcols <- names(dtm)[!names(dtm) %in% ncols]

   dtm <- dtm[,kcols, with = FALSE]

   return(dtm)
}
#' Fill intermediate sample_id's
#'
#' Fill sample_id for timestamps between start and end measurement.
#'
#' @param dt (data.table) A data.table where measurement have been merged with a
#' sample_id data
#' @export
fill_inter <- function(dt) {
  dt <- copy(dt)

  # add global variable binding
  sample_id = Timestamp = startend = NULL

  # check Timestamp
  checkmate::assert_posixct(dt$Timestamp)

  # fill sample_ids and intermeddiate measurements
  for(id in unique(dt$sample_id)) {
    dt <- dt[is.na(sample_id) &
               Timestamp > dt[sample_id == id & startend == 'start',Timestamp] &
               Timestamp < dt[sample_id == id & startend == 'end', Timestamp],
             c('sample_id', 'startend') := list(id, 'inter')]
  }

  return(dt)
}
