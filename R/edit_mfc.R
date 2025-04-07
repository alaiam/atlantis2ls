#' edit_param_mfc
#'
#' @param bio.lines
#' @param factor
#' @param species
#'
#' @return
#' @export
#'
#' @examples

edit_param_mfc = function(harvest.lines, factor, species){

  #Get mfc_XXX harvest.prm lines
  harvest.lines = harvest.lines
  pattern = paste0('mFC_',species)
  harvest.lines.id = grep(pattern,harvest.lines)
  harvest.lines.vals1 = harvest.lines[harvest.lines.id]
  if (length(harvest.lines.vals1)==0) stop("The species does not have mfc parameter")

  harvest.lines.vals1 = harvest.lines[harvest.lines.id + 1]
  value <- as.numeric(unlist(strsplit(harvest.lines.vals1, " ")))*factor
  value[is.na(value)] <- ""
  new.line <- paste0(paste(as.character(value), collapse = " "), " ")
  harvest.lines[harvest.lines.id + 1] <- new.line

  return(harvest.lines)
}
