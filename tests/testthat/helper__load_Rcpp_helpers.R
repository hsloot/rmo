tests_dir <- file.path(testthat::test_path(), "..")
files <- normalizePath(sort(dir(tests_dir, "^helper.*\\.cpp$", full.names = TRUE)))
invisible(lapply(files, Rcpp::sourceCpp, rebuild=TRUE))
