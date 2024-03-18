


#' run_atlantis
#'
#' @return
#' @export
#'
#' @examples
run_atlantis <- function(path, sh.file) {
  system(paste0("cd ",path ," sudo flip -uv *; sudo chmod +x ", sh.file,"; sudo sh ./", sh.file), wait = TRUE)
  print("Atlantis run is done!")
}
