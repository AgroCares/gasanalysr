#' Preprocess lumasoftgas file
#'
#' A function to turn a .xls file exported from Lumasoftgas 7820 or 7880 into a usable data.table or csv
#'
#' #' @param path (character) Path to the file
#'
#' @import data.table
#'
#' @examples
#'
ppr_lsg <- function(path){
  # add visual binding

  # check inputs
  checkmate::assert_character(path, n.chars = 5)
  # checkmate::assert_true(grepl('\\.xls$', path)) # not required of operating on folder

  # copy files to new folder and get path to new folder
  np <- lsg_fileppr(path)

  # change file extensions from xls to xlsx
  file.rename(from = list.files(np, full.names = TRUE),
              to = gsub('\\.xls$', '\\.xlsx', list.files(np, full.names = TRUE)))

}
#' Copy lsg raw data into new location
#'
#' @param p (character) Path to directory
lsg_fileppr <- function(p){
  # add visual binding

  # check inputs

  # get folder location
  fl <- dirname(p)

  # make new folder location
  dir.create(file.path(fl, 'ppr_lsg'))

  # select files to copy
  filestocopy <- list.files(fl, full.names = TRUE)

  # copy files into newdir
  file.copy(from = filestocopy, to = file.path(fl, 'ppr_lsg'),
            recursive = FALSE, copy.mode = TRUE)

  # return new dir
  return(paste0(fl, '/ppr_lsg'))
}
