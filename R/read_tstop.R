#' find_run_tstop
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
return_run_tstop <- function(path, run.filename) {
  prm <- paste0(path, "/", run.filename)
  bio.lines <- readLines(prm)
  pattern = paste0('tstop')
  bio.lines.id = grep(pattern,bio.lines)
  bio.lines.vals1 = bio.lines[bio.lines.id]

  a <- strsplit(bio.lines.vals1, "\t")[[1]][2]
  value <- as.numeric(unlist(strsplit(a, " "))[1])

  return(value)
}


#' return_real_tstop
#'
#' @param path
#' @param txt.filename
#'
#' @return
#' @export
#'
#' @examples
return_real_tstop <- function(path, txt.filename) {
  prm <- paste0(path, "/", txt.filename)
  bio.lines <- read.table(prm)
  last.raw <- length(bio.lines$V1)
  value <- as.numeric(bio.lines$V1[last.raw])
  return(value)
}


#' is.stopped
#'
#' @param path
#' @param txt.filename
#' @param run.filename
#'
#' @return
#' @export
#'
#' @examples
is.stopped <- function(path, txt.filename, run.filename) {

  return(return_real_tstop(path, txt.filename) < (return_run_tstop(path, run.filename)-1)) #TODO: Not hard coded the limit around tstop
}
