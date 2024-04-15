
#' edit_param_mum_group_age
#'
#' @param bio.prm Biology prm file path
#' @param new.mum Table of new mum for the species with vector.size age classes
#' @param overwrite Boolean. T = create a new biology prm files, called new.file.name
#' @param new.file.name Path of the new file
#' @param single.group  Boolean. T if vector = 1 (useless then...)
#' @param vector.size   Size of the age class vector to edit
#'
#' @return  Nothing, write a new biology prm files
#' @export
#'
#' @examples
edit_param_mum_group_age = function(bio.lines, new.mum, overwrite = F,new.file.name,single.group = F, vector.size = 10 ){

  #Get mum_XXX bio.prm lines
  bio.lines = bio.lines
  pattern = paste('^mum_.*',vector.size, sep = "")
  bio.lines.id = grep(pattern,bio.lines)
  bio.lines.vals1 = bio.lines[bio.lines.id]

  if (vector.size>1){
    if(length(grep("T15",bio.lines.vals1))!=0){
      bio.lines.id    <- bio.lines.id[   -grep("T15",bio.lines.vals1)]
      bio.lines.vals1 <- bio.lines.vals1[-grep("T15",bio.lines.vals1)]
    }
    pattern = paste('mum_|  ',vector.size,'\t\t\t\t\t\t\t\t\t', sep = "")
    group.names =unname(sapply(bio.lines.vals1,function(x) strsplit(x,pattern)[[1]][2]))

  }else{
    bio.lines.id    <- bio.lines.id[   grep("T15",bio.lines.vals1)]
    bio.lines.vals1 <- bio.lines.vals1[grep("T15",bio.lines.vals1)]
    group.names = unname(sapply(bio.lines.vals1,function(x) strsplit(x,'_T15')[[1]][1]))
    group.names = unname(sapply(group.names,function(x) strsplit(x,'mum_')[[1]][2]))
  }

  if(single.group){

    ind = which(group.name == group.names)
    C.string = paste(new.mum,collapse = '\t')
    bio.lines[bio.lines.id[ind]+1] = C.string
  }else{
    for(i in 1:nrow(new.mum)){

      ind = which(row.names(new.mum)[i] == group.names)
      C.string = paste(new.mum[i,1:(vector.size)],collapse='\t')
      bio.lines[bio.lines.id[i] + 2] = C.string
    }
    return(bio.lines)
  }

  #overwrite or make copy of biology file
  if(overwrite){
    writeLines(bio.lines, con = bio.prm)
  }else{
    file.copy(bio.prm, new.file.name, overwrite = T)
    writeLines(bio.lines, con = new.file.name )
  }

}


#' edit_param_mum_sp
#'
#' @param bio.prm  --> biology prm files
#' @param factor  --> factor to multiply the old value of mum
#' @param species --> species code in the Atlantis configuration
#'
#' @return bio.lines, vector with X elements, X = number of lines of biology prm files.
#' Each element is a line of biology prm files
#' @export
#'
#' @examples
edit_param_mum_sp = function(bio.lines, factor, species){

  #Get mum_XXX bio.prm lines
  bio.lines = bio.lines
  pattern = paste0('mum_',species)
  bio.lines.id = grep(pattern,bio.lines)
  bio.lines.vals1 = bio.lines[bio.lines.id]
  if (length(bio.lines.vals1)==0) stop("The species does not have mum parameter")

  if (length(grep("T15",bio.lines.vals1)) == 1){
    value <- as.numeric(unlist(strsplit(bio.lines.vals1, "\t"))[2])*factor
    name <- unlist(strsplit(bio.lines.vals1, "\t"))[1]
    new.line <- paste0(name, "\t", value)
    bio.lines[bio.lines.id] <- new.line
  }else{

    bio.lines.vals1 = bio.lines[bio.lines.id + 2]
    value <- as.numeric(unlist(strsplit(bio.lines.vals1, "\t")))*factor
    new.line <- paste(value, sep = "\t")
    bio.lines[bio.lines.id + 2] <- new.line
  }

  return(bio.lines)
}



