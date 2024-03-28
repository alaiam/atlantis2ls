library("stringr")

#' read_atlantis
#'
#' @param path : the path of the atlantis folder
#'
#' @return : An object of class Atlantis
#' @export
#'
#' @examples
#'
read_atlantis = function(path, prefix = NULL, fg.file, fishery = F, spatial = F,  ...) {
  if(!dir.exists(path)) stop("The output directory does not exist.")

  print("Reading Atlantis outputs")

  # a lot of lines to actually read the data
  output = list(biomass= extract_biomass_main(path = path, prefix = prefix, fg.file = fg.file),
                landings=extract_catch_main(path, prefix = NULL, fg.file, fishery = F),
                abundance= NA)


  class(output$biomass) = "atlantis.biomass"
  class(output$landings) = "atlantis.landings"
  class(output$abundance) = "atlantis.abundance"
  class(output) = "atlantis"
  return(output)
}


#' open_main_nc
#' Open the "XXX_OUT.nc" file
#' @param path
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples
open_main_nc <- function(path, prefix = NULL) {
  if(is.null(prefix)) stop("Add prefix argument for your Atlantis configuration")
  if(str_sub(path,-1)=="/"){
    outputs.nc <- ncdf4::nc_open(paste(path, "/", prefix, "_OUT.nc", sep = ""))
  }else{
    outputs.nc <- ncdf4::nc_open(paste(path, "/", prefix, "_OUT.nc", sep = ""))
  }
  return(outputs.nc)
}


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



  btype <- fg$BiomassType
  biomass <- lapply(X = Ndat, FUN = calculate_biomass, fg.file = fg, name = names(Ndat))

    names(biomass) <- fg$Name

  return(biomass)
  }

#' extract_catch_main
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
extract_catch_main <- function(path, prefix = NULL, fg.file, fishery = F){
  if (fishery == F){
    catch <- rep(list(rep(0,75)),73)
    names(catch) <- fg$Name
  }else{

  }

  return(catch)
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
calculate_biomass <- function(biomass_array, fg.file, name) {

  #pas opti car appelé pleins de fois je crois: TODO: tester si c'est long
  volumes <- ncdf4::ncvar_get(outputs.nc, "volume")
  volumes_arr <- array(data = unlist(volumes),dim = dim(volumes)[c(1,2)]) # box/layer volumes

  # btype <- fg.file$BiomassType[fg.file$Name == sub("_N", "", name)]
  # areas_vec <- areas$area
  areas_vec <- volumes_arr[7,]/20 #TODO: true calculation

  if(length(dim(biomass_array))==3) {
      totbio <- apply(biomass_array * c(volumes_arr) * (5.7 * 20 / 10^9), 3, sum, na.rm = TRUE)
    } else {
      totbio <- apply(biomass_array * areas_vec * (5.7 * 20 / 10^9), 2, sum, na.rm = TRUE)
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



#' open_fg_file
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
open_fg_file <- function(fg.file) {
  fg <- read.csv(fg.file)
  fg <- fg %>% dplyr::filter(IsTurnedOn==1)
  return(fg)
}


#' process_fg
#'
#' @param fg.file
#'
#' @return
#' @export
#'
#' @examples
process_fg <- function(fg.file){

  fg <- open_fg_file(fg.file)
  fg$BiomassType <- fg$GroupType

  fg1 <- fg %>%
    dplyr::filter(GroupType%in%c("FISH","SHARK","BIRD","MAMMAL")) %>%
    dplyr::mutate(BiomassType="vertebrate")
  fg2 <- fg %>%
    dplyr::filter(GroupType%in%c("PWN",'CEP','LG_ZOO','MED_ZOO','SM_ZOO','LG_PHY','SM_PHY')) %>%
    dplyr::mutate(BiomassType="plankton")
  fg3 <- fg %>%
    dplyr::filter(GroupType%in%c("MOB_EP_OTHER",'SED_EP_FF','SED_EP_OTHER','SEAGRASS','PHYTOBEN')) %>%
    dplyr::mutate(BiomassType="2D")
  fg4 <- fg %>%
    dplyr::filter(GroupType%in%c("LG_INF","MICROPHTYBENTHOS","SED_BACT","PL_BACT","SM_INF","CARRION","LAB_DET","REF_DET")) %>%
    dplyr::mutate(BiomassType="other")

  return(rbind(fg1,fg2,fg3,fg4))
}


