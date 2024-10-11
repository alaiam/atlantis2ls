



#' edit_param_KDENR_sp
#'
#' @param factor  --> factor to multiply the old value of KDENR
#' @param species --> species code in the Atlantis configuration
#' @param bio.lines
#'
#' @return bio.lines, vector with X elements, X = number of lines of biology prm files.
#' Each element is a line of biology prm files
#' @export
#'
#' @examples
#'
edit_param_KDENR_sp = function(bio.lines, factor, species){

  #Get KDENR_XXX bio.prm lines
  bio.lines = bio.lines
  pattern = paste0('KDENR_',species)
  bio.lines.id = grep(pattern,bio.lines)
  bio.lines.vals1 = bio.lines[bio.lines.id]
  if (length(bio.lines.vals1)==0) stop("The species does not have KDENR parameter")

    bio.lines.vals1 = bio.lines[bio.lines.id + 2]
    value <- as.numeric(unlist(strsplit(bio.lines.vals1, "   ")))*factor
    value[is.na(value)] <- ""
    new.line <- paste0(paste(as.character(value), collapse = "   "), "   ")
    bio.lines[bio.lines.id + 2] <- new.line

  return(bio.lines)
}



