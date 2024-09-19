# Function to read biomass outputs


#' extract_biomass_main
#'
#' @param path
#' @param prefix
#' @param fg.file
#' @param sp
#'
#' @return
#' @export
#'
#' @examples
extract_biomass_main <- function(path, prefix = NULL, fg.file){
  outputs.nc <-open_main_nc(path, prefix = prefix)

  fg <- process_fg(fg.file)


  vn <- c(paste0(fg$Name,"_N")) # variable name is functional group + "_N"
  ts <- ncdf4::ncvar_get(outputs.nc,varid = "t") %>% as.numeric
  tyrs <- ts/(60*60*24*365) # from s to yrs




  Ndat <- purrr::map(1:length(vn), function(x) ncdf4::ncvar_get(outputs.nc,vn[x]))
  names(Ndat)<- vn

  log_file <- "parallel_log_names_var.txt"
  write(paste("Running on core:", vn, "\n"),
        file = log_file, append = TRUE)

  btype <- fg$BiomassType
  biomass <- lapply(X = Ndat, FUN = calculate_biomass, fg.file = fg, name = names(Ndat), outputs.nc = outputs.nc)

  names(biomass) <- fg$Name

  return(biomass)
}

#' calculate_biomass
#'
#' @param i
#'
#' @return
#' @export
#'
#' @examples
#'
calculate_biomass <- function(biomass_array, fg.file, name, outputs.nc) {

  #pas opti car appelé pleins de fois je crois: TODO: tester si c'est long
  volumes <- ncdf4::ncvar_get(outputs.nc, "volume")
  volumes_arr <- array(data = unlist(volumes),dim = dim(volumes)[c(1,2)]) # box/layer volumes

  # btype <- fg.file$BiomassType[fg.file$Name == sub("_N", "", name)]
  # areas_vec <- areas$area
  areas_vec <- volumes_arr[7,]/20 #TODO: true calculation
  log_file <- paste0("debug_dim.txt")
  write(paste(dim(biomass_array)),
        file = log_file, append = TRUE)

  if(length(dim(biomass_array))==3) {
    totbio <- apply(biomass_array * c(volumes_arr) * (5.7 * 20 / 10^9), 3, sum, na.rm = TRUE)
  } else {
    if (length(dim(biomass_array))==0) {
      totbio = 0
    }else {

      totbio <- apply(biomass_array * areas_vec * (5.7 * 20 / 10^9), 2, sum, na.rm = TRUE)
    }
  }


  # if(btype == "vertebrate" | btype == "plankton") {
  #   totbio <- apply(biomass_array * c(volumes_arr) * (5.7 * 20 / 10^9), 3, sum, na.rm = TRUE)
  # } else if(btype == "2D") {
  #   totbio <- apply(biomass_array * areas_vec * (5.7 * 20 / 10^9), 2, sum, na.rm = TRUE)
  # } else if(btype == "other") {
  #   sum_volumes <- sum(volumes$volume)
  #   totbio <- apply(biomass_array * c(volumes_arr), 3, sum, na.rm = TRUE) / sum_volumes
  # }
  return(totbio)
}
