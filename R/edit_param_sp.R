
#' edit_param_sp
#'
#' @param bio.lines
#' @param param
#' @param factor
#' @param species
#'
#' @return
#' @export
#'
#' @examples
edit_param_sp = function(bio.lines, param, factor, species){
  if (tolower(param) == "mum"){
    bio.lines <- edit_param_mum_sp(bio.lines, factor, species)
  }
  if (tolower(param) == "bhbeta"){
    bio.lines <- edit_param_BHbeta_sp(bio.lines, factor, species)
  }
  if (tolower(param) == "bhaphla"){
    bio.lines <- edit_param_BHalpha_sp(bio.lines, factor, species)
  }
  if (tolower(param) == "mq"){
    bio.lines <- edit_param_mq_sp(bio.lines, factor, species)
  }

  return(bio.lines)

}
