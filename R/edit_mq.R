#' edit_param_mq_sp
#'
#' @param bio.prm  ReadLines(Biology prm files)
#' @param factor  Factor to multiply the old value of mq
#' @param species Species code in the Atlantis configuration
#'
#' @return bio.lines with mq of species multiplied by factor
#' @export
#'
#' @examples
edit_param_mq_sp = function(bio.lines, factor, species){

  bio.lines = bio.lines
  pattern = paste0(species,'_mQ')
  bio.lines.id = grep(pattern,bio.lines)
  bio.lines.vals1 = bio.lines[bio.lines.id]
  if (length(bio.lines.vals1)==0) stop("The species does not have a mQ parameter")

  value <- as.numeric(unlist(strsplit(bio.lines.vals1, "\t"))[2])
  if (is.na(value)) stop("The function is not ready yet to deal with mQ vector")
  value <- value*factor
  name <- unlist(strsplit(bio.lines.vals1, "\t"))[1]
  new.line <- paste0(name, "\t", value)
  bio.lines[bio.lines.id] <- new.line

  return(bio.lines)
}

