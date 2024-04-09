# Function to read abundance outputs

#' extract_abundance_grp
#'
#' @param path
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples
extract_abundance_grp <- function(path, prefix = NULL){
  outputs.nc <-open_main_nc(path, prefix = prefix)
  vn <- names(outputs.nc$var)[grep("_Nums",outputs.nc$var)]
  Ndat <- purrr::map(1:length(vn), function(x) ncdf4::ncvar_get(outputs.nc,vn[x]))
  names(Ndat)<- vn
  abund_allgrp <- lapply(X = Ndat, FUN = calculate_abund)
  names(abund_allgrp) <- names(Ndat)
  return(abund_allgrp)
}

#' extract_abundance_sp
#'
#' @param path
#' @param prefix
#' @param species
#'
#' @return
#' @export
#'
#' @examples
extract_abundance_sp <- function(path, prefix = NULL, species){
  data <- extract_abundance_grp(path, prefix = prefix)
  i.species = grep(species,names(data))
  if (length(i.species)>0){
    return(Reduce('+', data[i.species]))
  }else{
    return(NA)
  }
}

#' extract_abund_main
#'
#' @param path
#' @param prefix
#' @param fg.file
#'
#' @return
#' @export
#'
#' @examples
extract_abund_main <- function(path, prefix = NULL, fg.file){
  fg <- process_fg(fg.file)
  abund <- lapply(X = fg$Name, FUN = extract_abundance_sp, path = path, prefix = prefix)
  names(abund) <- fg$Name
  return(abund)
}

#' calculate_abund
#'
#' @param biomass_array
#' @param fg.file
#' @param name
#'
#' @return
#' @export
#'
#' @examples
calculate_abund <- function(abund_array) {
  return(apply(abund_array , 3, sum, na.rm = TRUE))
}
