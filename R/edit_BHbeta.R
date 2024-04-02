#' Title
#'
#' @param bio.prm
#' @param factor
#' @param species
#'
#' @return
#' @export
#'
#' @examples
edit_param_BHbeta_sp = function(bio.prm, factor, species){

  #Get mum_XXX bio.prm lines
  bio.lines = readLines(bio.prm)
  pattern = paste0('BHbeta_',species)
  bio.lines.id = grep(pattern,bio.lines)
  bio.lines.vals1 = bio.lines[bio.lines.id]
  if (length(bio.lines.vals1)==0) stop("The species does not have a BHbeta parameter")
  value <- as.numeric(unlist(strsplit(bio.lines.vals1, "\t"))[2])*factor
  name <- unlist(strsplit(bio.lines.vals1, "\t"))[1]
  new.line <- paste0(name, "\t", value)
  bio.lines[bio.lines.id] <- new.line

  return(bio.lines)
}


