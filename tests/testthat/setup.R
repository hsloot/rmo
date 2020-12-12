lib_path_restore <- .libPaths()
local_lib <- tempdir()
remotes::install_local("testutils.rmo", type = "source", quiet = TRUE,
                       lib = local_lib)
.libPaths(c(local_lib, .libPaths()))

# The auxiliary functions for parameter generation are relocated to
# the Rsource directory such that they are available in interactive R
# mode (e.g. for profiling).
source(system.file("Rsource", "parameter_generator.R",
                   package="rmo", mustWork=TRUE))

withr::defer({
  .libPaths(lib_path_restore)
  try({
    detach("testutils.rmo", unload = TRUE)
  }, silent = TRUE)
  package_dir <- file.path(local_lib, "testutils.rmo")
  if (dir.exists(package_dir)) unlink(package_dir)
  },
  teardown_env()
)
