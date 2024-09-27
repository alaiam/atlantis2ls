# Function to read waa outputs


extract_waa <- function(path, prefix = NULL){
  outputs.nc <-open_main_nc(path, prefix = prefix)
  vn <- names(outputs.nc$var)[grep("_ResN",outputs.nc$var)]
  data_Res <- purrr::map(1:length(vn), function(x) ncdf4::ncvar_get(outputs.nc,vn[x]))
  names(data_Res)<- vn

  vn <- names(outputs.nc$var)[grep("_StructN",outputs.nc$var)]
  data_Str <- purrr::map(1:length(vn), function(x) ncdf4::ncvar_get(outputs.nc,vn[x]))
  names(data_Str)<- vn

  vn <- names(outputs.nc$var)[grep("_Nums",outputs.nc$var)]
  data_Nums <- purrr::map(1:length(vn), function(x) ncdf4::ncvar_get(outputs.nc,vn[x]))
  names(data_Nums)<- vn

  totnums <-data_Nums %>% purrr::map(apply,MARGIN=3,FUN=sum) # total numbers by age group, time
  relnums <- purrr::map2(data_Nums,totnums,sweep,MARGIN=3,FUN=`/`) # divide nums by totnums along the time axis to get relative annual nums per age group/box/layer



  wgt = lapply(purrr::map2(purrr::map2(data_Str,relnums,`*`), purrr::map2(data_Res,relnums,`*`), `+`),"*",20*5.7/1e6)%>%
    purrr::map(apply,MARGIN=3,FUN=sum)
  names(wgt)  <- sub( "_StructN", "", names(wgt))

  return(wgt)
}
