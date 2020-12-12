# The auxiliary functions for parameter generation are relocated to
# the Rsource directory such that they are available in interactive R
# mode (e.g. for profiling).
source(system.file("Rsource", "parameter_generator.R",
    package="rmo", mustWork=TRUE))
