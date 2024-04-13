#' Return the Stieltjes density of a Bernstein function
#'
#' Returns the *Stieltjes density* with `lower`, `upper`, and `type` attributes
#' if continuous and returns a `data.frame` with named columns `x` (atoms) and
#' `y` (weights) as well as a type attribute if discrete. The `type` attribute
#' is either `"continuous"` or `"discrete"`.
#'
#' @param object An object deriving from [CompleteBernsteinFunction-class].
#'
#' @importFrom methods setGeneric
#' @family Bernstein function generics
#' @export
setGeneric(
  "stieltjesDensity",
  function(object) {
    standardGeneric("stieltjesDensity")
  }
)
