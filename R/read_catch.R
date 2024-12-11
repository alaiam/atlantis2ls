# Function to read catch outputs

#' extract_catch_main
#'
#' @param path
#' @param prefix
#' @param fg.file
#' @param fishery
#' @param dt.timeserie
#'
#' @return
#' @export
#'
#' @examples
extract_catch_main <- function(path, prefix = NULL, fg.file, fishery = F, dt.timeserie){
  fg <- process_fg(fg.file)

  if (fishery == F){
    fg <- process_fg(fg.file)
    catch <- rep(list(rep(0,dt.timeserie)),length(fg$Name))
    names(catch) <- fg$Name
  }else{

  }

  return(catch)
}
