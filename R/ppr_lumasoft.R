#' Preprocess lumasoftgas file
#'
#' A function to turn a .xls file exported from Lumasoftgas 7820 or 7880 into a usable data.table or csv
#'
#' #' @param p (character) Path to a directory containing xls files to convert
#'
#' @import data.table
#'
#' @examples
#'
ppr_lsg <- function(p){
  # add visual binding

  # check inputs
  checkmate::assert_character(p, min.chars = 5)
  checkmate::assert_directory(p)

  # copy files to new folder and get path to new folder
  np <- lsg_copy_raw_data(p)

  # change file extensions from xls to xlsx
  lsg_xls_to_xlsx(folder_with_xls_files = np)

  #

}
#' Copy lsg raw data into new location
#'
#' @param p (character) Path to directory
lsg_copy_raw_data <- function(p){
  # add visual binding

  # check inputs

  # make new folder location
  dir.create(file.path(p, 'ppr_lsg'))

  # select files to copy
  filestocopy <- list.files(p, full.names = TRUE)

  # copy files into newdir
  file.copy(from = filestocopy, to = file.path(p, 'ppr_lsg'),
            recursive = FALSE, copy.mode = TRUE)

  # return new dir
  return(paste0(p, '/ppr_lsg'))
}

#' Change file extension to xlsx
#'
#' Changes the file extension of all files with the extension .xls to .xlsx
#'
#' @param folder_with_xls_files (character) path to a directory with .xls files
lsg_xls_to_xlsx <- function(folder_with_xls_files) {
  # add visual binding

  # check input
  checkmate::assert_directory(folder_with_xls_files)

  # change file extensions from xls to xlsx
  file.rename(from = list.files(folder_with_xls_files, full.names = TRUE),
              to = gsub('\\.xls$', '\\.xlsx', list.files(folder_with_xls_files, full.names = TRUE)))
}


#' Read lsg file and turn into formatted dt
#'
#' @param f (character) file name
#' @param p (character) path to directory file is in
lsg_xlsx_to_dt <- function(f, p){
  # add visual binding
  floc = dt = mdt = out = sn = sts = NULL

  # check inputs
  checkmate::assert_directory(p)
  checkmate::assert_file_exists(paste0(p, '/', f))

  # get full file location
  floc <- paste0(p, '/',f)

  # get sheet names
  sts <- readxl::excel_sheets(floc)

  for(sn in sts[-1]) {
    # read sheet
    dt <- readxl::read_excel(path = floc, sheet = sn, skip = 1) |> setDT()

    # select meltable columns (remove non numerics)
    meltable <- c('Date & Time')
    for(cname in names(dt)) {
      if(is.numeric(dt[,get(cname)])){
        meltable <- c(meltable, cname)
      }
    }

    # melt
    mdt <- melt(dt[,..meltable], id.vars = "Date & Time")

    # set value column name
    setnames(mdt, old = 'value', sn)

    # merge in product
    if(is.null(out)){
      out <- mdt
    } else{
      out <- merge(out, mdt, by = c(names(mdt)[1:2]))
    }
  }

  # return
  return(out)
}
