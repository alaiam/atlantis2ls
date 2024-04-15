#' edit_param_sp
#'
#' @param bio.lines
#' @param param
#' @param factor
#' @param species
#'
#' @return bio.lines with a "factor" applied to the parameter "param" of the "species"
#' @export
#'
#' @examples
edit_param_sp = function(bio.lines, param, factor, species){
  if (tolower(param) == "mum"){
    bio.lines <- edit_param_mum_sp(bio.prm, factor, species)
  }
  if (tolower(param) == "bhbeta"){
    bio.lines <- edit_param_BHbeta_sp(bio.prm, factor, species)
  }
  if (tolower(param) == "bhaphla"){
    bio.lines <- edit_param_BHalpha_sp(bio.prm, factor, species)
  }
  return(bio.lines)

}
