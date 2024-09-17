library("stringr") # TODO: verifier que c'est bien en entrée du package puis sortir cette ligne
# Main read_atlantis function and general tools


##########
## MAIN ##
##########

#' read_atlantis
#'
#' @param path : the path of the atlantis folder
#'
#' @return : An object of class Atlantis
#' @export
#'
#' @examples
#'
read_atlantis = function(path, prefix = NULL, fg.file, fishery = F, spatial = F, N_only = F,
                         txt.filename = "outputFolder/AMPS_OUTBiomIndx.txt",
                         run.filename = "PugetSound_run.prm",...) { #TODO: generalize
  if(!dir.exists(path)) stop("The output directory does not exist.")

  print("Reading Atlantis outputs")

  # a lot of lines to actually read the data
  if (N_only == T){
    output = list(Nbiomass = extract_Nbiomass_main(path = path, prefix = prefix, fg.file = fg.file))
    class(output$Nbiomass) = "atlantis.Nbiomass"
    class(output) = "atlantis"
  }else{
    if (is.stopped(path, txt.filename, run.filename)){ #TODO: improve this to have it not hardcoded
      output = list(biomass= rep(0,2),
                    landings=rep(0,2),
                    abundance= rep(0,2),
                    waa = rep(0,2))
    }else{
      output.path = paste0(path, "/outFolder")
      output = list(biomass= extract_biomass_main(path = path, prefix = prefix, fg.file = fg.file),
                    landings=extract_catch_main(path, prefix = prefix, fg.file, fishery = F),
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

