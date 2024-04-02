# Function to read catch outputs

#' extract_catch_main
#'
#' @param path
#' @param prefix
#' @param fg.file
#' @param fishery
#'
#' @return
#' @export
#'
#' @examples
extract_catch_main <- function(path, prefix = NULL, fg.file, fishery = F){
  fg <- process_fg(fg.file)
  if (fishery == F){
    catch <- rep(list(rep(0,75)),73)
    names(catch) <- fg$Name
  }else{

  }

  return(catch)
}
