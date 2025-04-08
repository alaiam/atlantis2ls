
##########
## MAIN ##
##########

#' read_atlantis
#'
#' @param path : the path of the atlantis folder
#'
#' @param prefix Prefix of the configuration files (ex: for Puget Sound, it is AMPS)
#' @param fg.file ".csv" file with the functional groups information
#' @param fishery Is fishery activated? Default is FALSE
#' @param spatial Load the data per polygon. Default is FALSE. Option TRUE is non-available
#' @param N_only  Load the row biomass data in Nitrogen
#' @param txt.filename Localisation of the biomass files ("outputFolder/XXXX_OUTBiomIndx.txt"). Default is for Puget Sound ecosystem
#' @param run.filename Localisation of the run.prm files ("PugetSound_run.prm"). Default is for Puget Sound ecosystem
#' @param dt.timeserie Number of timestep written in the NC files. Default is 2. Option needed to create data when the model is broken during calibration process. The artificial data created are equal to 0 for all species and all indicators.
#' @param ...
#'
#' @return : An object of class Atlantis
#' @export
#'
#' @examples
#'
read_atlantis = function(path, prefix = NULL, fg.file, fishery, spatial = F, N_only = F,
                         txt.filename = "outputFolder/AMPS_OUTBiomIndx.txt",
                         run.filename = "PugetSound_run.prm",
                         dt.timeserie = 2, ...) {
  if(!dir.exists(path)) stop("The output directory does not exist.")

  print("Reading Atlantis outputs")

  if (N_only == T){
    output = list(Nbiomass = extract_Nbiomass_main(path = path, prefix = prefix, fg.file = fg.file))
    class(output$Nbiomass) = "atlantis.Nbiomass"
    class(output) = "atlantis"
  }else{
    if (is.stopped(path, txt.filename, run.filename)){
      fg.data = process_fg(fg.file)
      output <- list(
        biomass = setNames(lapply(fg.data$Name, function(x) rep(0, dt.timeserie)), fg.data$Name),
        landings = setNames(lapply(fg.data$Name, function(x) rep(0, dt.timeserie)), fg.data$Name),
        abundance = setNames(lapply(fg.data$Name, function(x) rep(0, dt.timeserie)), fg.data$Name),
        waa = setNames(lapply(fg.data$Name, function(x) rep(0, dt.timeserie)), fg.data$Name)
      )

    }else{
      output.path = paste0(path, "/outFolder")
      output = list(biomass= extract_biomass_main(path = path, prefix = prefix, fg.file = fg.file),
                    landings=extract_catch_main(path, prefix = prefix, fg.file, fishery),
                    abundance= extract_abund_main(path, prefix = prefix, fg.file),
                    waa = extract_waa(path=path, prefix = prefix))
    }

    class(output$biomass) = "atlantis.biomass"
    class(output$landings) = "atlantis.landings"
    class(output$abundance) = "atlantis.abundance"
    class(output) = "atlantis"
  }



  return(output)
}


#################################
## GENERAL TOOLS TO OPEN FILES ##
#################################

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

#' open_catch_nc
#' Open the "XXX_OUT.nc" file
#' @param path
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples
open_catch_nc <- function(path, prefix = NULL) {
  if(is.null(prefix)) stop("Add prefix argument for your Atlantis configuration")
  if(str_sub(path,-1)=="/"){
    outputs.nc <- ncdf4::nc_open(paste(path, "/", prefix, "_OUTCATCH.nc", sep = ""))
  }else{
    outputs.nc <- ncdf4::nc_open(paste(path, "/", prefix, "_OUTCATCH.nc", sep = ""))
  }
  return(outputs.nc)
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

