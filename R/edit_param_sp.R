#' edit_param_sp
#'
#' @param bio.prm
#' @param param
#' @param factor
#' @param species
#'
#' @return
#' @export
#'
#' @examples
edit_param_sp = function(bio.prm, param, factor, species){
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
