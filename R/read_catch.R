# Function to read catch outputs

#' extract_catch_main
#'
#' @param path
#' @param prefix
#' @param fg.file
#' @param fishery
#' @param dt.timeserie
#'
#' @return
#' @export
#'
#' @examples
extract_catch_main <- function(path, prefix = NULL, fg.file, fishery = F, dt.timeserie){
  fg <- process_fg(fg.file)

  if (fishery == F){
    fg <- process_fg(fg.file)
    catch <- rep(list(rep(0,dt.timeserie)),length(fg$Name))
    names(catch) <- fg$Name
  }else{
    outputs.nc <-open_catch_nc(path, prefix = prefix)

    fg <- process_fg(fg.file)

    all.var <- names(outputs.nc$var)
    vn <-  all.var[grep("_Catch", all.var)]
    vn <-  vn[grep("_FC", vn)]
    ts <- ncdf4::ncvar_get(outputs.nc,varid = "t") %>% as.numeric
    tyrs <- ts/(60*60*24*365) # from s to yrs




    Ndat <- purrr::map(1:length(vn), function(x) ncdf4::ncvar_get(outputs.nc,vn[x]))
    names(Ndat)<- vn
    catch<-list()

    species_code <- unique(str_extract(names(Ndat), "^[^_]*"))
    for (i in unique(str_extract(names(Ndat), "^[^_]*"))){
      Name <- fg$Name[fg$Code==i]
      catch[[Name]] <- apply(X=as.array(Ndat[str_extract(names(Ndat), "^[^_]*")==i])[[1]], FUN = sum, MARGIN = 2)
    }
  }
  return(catch)
}
