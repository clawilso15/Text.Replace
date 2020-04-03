.onUnload <- function (libpath) {
  library.dynam.unload("Text.Replace", libpath)
}