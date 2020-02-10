.onUnload <- function (libpath) { # nolint
  library.dynam.unload("rmo", libpath)
}
