lib_path_restore <- .libPaths()
local_lib <- tempfile("RLIB_")
dir.create(local_lib)
.libPaths(c(local_lib, .libPaths()))
message("Loading testutils.rmo")
remotes::install_local("testutils.rmo", type = "source", quiet = TRUE,
    dependencies = FALSE, build = FALSE, upgrade = FALSE)
if (!"testutils.rmo" %in% installed.packages()[, "Package"]) {
    message(sprintf(
        "`testutils.rmo` installation failed on %s; retry with `upgrade=TRUE`",
        .Platform$OS.type
    ))
    ## TODO: This is required due to some weird bug in rcmdcheck or remotes;
    ## find a better solution
    remotes::install_local("testutils.rmo", type = "source", repos = NULL,
        quiet = TRUE, dependencies = TRUE, build = FALSE,
        upgrade = TRUE)
}

# The auxiliary functions for parameter generation are relocated to
# the Rsource directory such that they are available in interactive R
# mode (e.g. for profiling).
source(system.file("Rsource", "parameter_generator.R", package = "rmo",
    mustWork = TRUE))

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
