# Function to read biomass outputs


#' extract_Nbiomass_main
#'
#' @param path
#' @param prefix
#' @param fg.file
#'
#' @return
#' @export
#'
#' @examples
extract_Nbiomass_main <- function(path, prefix = NULL, fg.file){
  outputs.nc <-open_main_nc(path, prefix = prefix)
  fg <- process_fg(fg.file)
  vn <- c(paste0(fg$Name,"_N")) # variable name is functional group + "_N"
  Ndat <- purrr::map(1:length(vn), function(x) ncdf4::ncvar_get(outputs.nc,vn[x]))
  names(Ndat)<- vn

  biomass <- lapply(X = Ndat, FUN = calculate_Nbiomass)

  names(biomass) <- fg$Name

  return(biomass)
}

#' calculate_Nbiomass
#'
#' @param biomass_array
#'
#' @return
#' @export
#'
#' @examples
#'
calculate_Nbiomass <- function(biomass_array) {


  if(length(dim(biomass_array))==3) {
    totbio <- apply(biomass_array, 3, sum, na.rm = TRUE)
  } else {
    totbio <- apply(biomass_array, 2, sum, na.rm = TRUE)
  }
  return(totbio)
}
