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
extract_biomass_main <- function(path, prefix = NULL, fg.file, txt = F, dt.timeserie = NA){
  if (txt == F){
  outputs.nc <-open_main_nc(path, prefix = prefix)

  fg <- process_fg(fg.file)


  vn <- c(paste0(fg$Name,"_N")) # variable name is functional group + "_N"
  ts <- ncdf4::ncvar_get(outputs.nc,varid = "t") %>% as.numeric
  tyrs <- ts/(60*60*24*365) # from s to yrs

  Ndat <- purrr::map(1:length(vn), function(x) ncdf4::ncvar_get(outputs.nc,vn[x]))
  names(Ndat)<- vn

  biomass <- lapply(X = Ndat, FUN = calculate_biomass, fg.file = fg, name = names(Ndat), outputs.nc = outputs.nc)

  names(biomass) <- fg$Name
  }
  if (txt==T){
    fg <- process_fg(fg.file)
    num <- length(fg$Code)

    outputs.txt <- read.table(paste0(path, "/",prefix, "_OUTBiomIndx.txt"), header = T)[,2:(num+1)]
    code_indices <- match(names(outputs.txt), fg$Code)
    names(outputs.txt) <- fg$Name[code_indices]

    if(!is.na(dt.timeserie)){
      length.ts <- dim(outputs.txt)[1]
      outputs.txt <- outputs.txt[(length.ts-dt.timeserie+1):length.ts,]}
    biomass <- lapply(X = outputs.txt, FUN = as.numeric)

  }

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
  areas_vec <- volumes_arr[7,] #layer 7 = 1m deep; so volume = area

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


