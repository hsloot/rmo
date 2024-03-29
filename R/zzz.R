.onUnload <- function(libpath) { # nolint # nocov start
  library.dynam.unload("rmo", libpath)
} # nocov end
