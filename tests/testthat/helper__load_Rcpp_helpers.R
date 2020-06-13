files <- normalizePath(sort(dir("..", "^helper.*\\.cpp$", full.names = TRUE)))
invisible(lapply(files, Rcpp::sourceCpp, rebuild=TRUE))
