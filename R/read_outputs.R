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
                landings=NA,
                abundance= NA)


  class(output$biomass) = "atlantis.biomass"
  class(output$landings) = "atlantis.landings"
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
    outputs.nc <- ncdf4::nc_open(paste(path, "/", prefix, "_AMPS_OUT.nc", sep = ""))
  }else{
    outputs.nc <- ncdf4::nc_open(paste(path, "/", prefix, "_AMPS_OUT.nc", sep = ""))
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
  outputs.nc <-open_main_nc(path, prefix = NULL)

  fg <- process_fg(fg.file)

  volumes <- ncdf4::ncvar_get(outputs.nc, volume)
  vn <- c(paste0(fg$LongName,"_N")) # variable name is functional group + "_N"
  ts <- ncdf4::ncvar_get(outputs.nc,varid = "t") %>% as.numeric
  tyrs <- ts/(60*60*24*365) # from s to yrs


  Ndat <- purrr::map(1:length(vn), function(x) ncdf4::ncvar_get(outputs.nc,vn[x])) %>%
    reduce(`+`) %>%
    setNA()
  volumes_arr <- array(data = unlist(volumes),dim = dim(Ndat)[c(1,2)]) # box/layer volumes
  areas_vec <- areas$area

  btype <- fg$BiomassType

  output = list(biomass= extract_biomass_main(path = path, prefix = prefix, fg.file = fg.file),
                landings=NA,
                abundance= NA)
  # if it's a 3D critter like a fish, or it's plankton-like, biomass is N*volume*(5.7*20/10^9), in metric tons
  if(btype=="vertebrate"|btype=="plankton") {
    totbio <- apply(Ndat*c(volumes_arr)*(5.7*20/10^9),3,sum,na.rm=T) # multiply by volume of each layer and then sum across boxes and layers for each time step
  }

  # if it's a 2D critter like a benthic invert, biomass is N*area*(5.7*20/10^9), in metric tons
  if(btype=="2D") {
    totbio <- apply(Ndat*areas_vec*(5.7*20/10^9),2,sum,na.rm=T) # multiply by area of each layer and then sum across boxes and layers for each time step
  }

  # if it's other (like detritus and bacteria), leave it as a density (biomass/total volume), mg N/m^3
  if(btype=="other") {
    sum_volumes <- sum(volumes$volume)
    totbio <- apply(Ndat*c(volumes_arr),3,sum,na.rm=T)/sum_volumes
  }

  totbio$Year <- tyrs

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
  fg <- fg %>% filter(IsTurnedOn==1)
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

  fg <- fg %>%
    select(GroupType%in%c("FISH","SHARK","BIRD","MAMMAL")) %>%
    mutate(BiomassType="vertebrate")
  fg <- fg %>%
    select(GroupType%in%c("PWN",'CEP','LG_ZOO','MED_ZOO','SM_ZOO','LG_PHY','SM_PHY')) %>%
    mutate(BiomassType="plankton")
  fg <- fg %>%
    select(GroupType%in%c("MOB_EP_OTHER",'SED_EP_FF','SED_EP_OTHER','SEAGRASS','PHYTOBEN')) %>%
    mutate(BiomassType="2D")
  fg <- fg %>%
    select(GroupType%in%c("LG_INF","MICROPHTYBENTHOS","SED_BACT","PL_BACT","SM_INF","CARRION","LAB_DET","REF_DET")) %>%
    mutate(BiomassType="other")

  return(fg)
}
# extract_volume <- function(outputs.nc){
#   ncdf4::ncvar_get(outputs.nc, volume)
# }

