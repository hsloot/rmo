#' Deprecated functions in rmo
#'
#' @description
#' These functions are deprecated and will be removed in future versions of rmo.
#'
#' @keywords internal
#' @details
#' - [exIntensities()] is deprecated in favor of
#'   [calcExShockSizeArrivalIntensities()].
#' - [uexIntensities()] is deprecated in favor of
#'   [calcExShockArrivalIntensities()].
#' - [exQMatrix()] is deprecated in favor of [calcMDCMGeneratorMatrix()].
#' - [intensities()] is deprecated in favor of [calcShockArrivalIntensities()].
#' - [defaultMethod()] is deprecated in favor of [getDefaultMethodString()].
#' - [levyDensity()] is deprecated in favor of [getLevyDensity()].
#' - [stieltjesDensity()] is deprecated in favor of [getStieltjesDensity()].
#' - [valueOf()] is deprecated in favor of [calcIterativeDifference()].
#' - [valueOf0()] is deprecated in favor of [calcValue()].
#' @name rmo-deprecated
#' @include s4-BernsteinFunction.R
NULL

#' @rdname rmo-deprecated
#' @aliases exIntensities-deprecated
#' @export
setGeneric(
  "exIntensities",
  function(object, d, cscale = 1, ...) {
    .Deprecated("calcExShockSizeArrivalIntensities", "rmo")
    standardGeneric("exIntensities")
  }
)

#' @rdname rmo-deprecated
#' @usage NULL
#' @include s4-calcExShockSizeArrivalIntensities.R
#' @export
setMethod(
  "exIntensities",
  "BernsteinFunction",
  function(object, d, cscale = 1, ...) {
    calcExShockSizeArrivalIntensities(object, d, cscale = cscale, ...)
  }
)

#' @rdname rmo-deprecated
#' @aliases uexIntensities-deprecated
#' @export
setGeneric(
  "uexIntensities",
  function(object, d, cscale = 1, ...) {
    .Deprecated("calcExShockArrivalIntensities", "rmo")
    standardGeneric("uexIntensities")
  }
)

#' @rdname rmo-deprecated
#' @usage NULL
#' @include s4-calcExShockArrivalIntensities.R
#' @export
setMethod(
  "uexIntensities",
  "BernsteinFunction",
  function(object, d, cscale = 1, ...) {
    calcExShockArrivalIntensities(object, d, cscale = cscale, ...)
  }
)

#' @rdname rmo-deprecated
#' @aliases exQMatrix-deprecated
#' @export
setGeneric(
  "exQMatrix",
  function(object, d, cscale = 1, ...) {
    .Deprecated("calcMDCMGeneratorMatrix", "rmo")
    standardGeneric("exQMatrix")
  }
)

#' @rdname rmo-deprecated
#' @usage NULL
#' @include s4-calcMDCMGeneratorMatrix.R
#' @export
setMethod(
  "exQMatrix",
  "BernsteinFunction",
  function(object, d, cscale = 1, ...) {
    calcMDCMGeneratorMatrix(object, d, cscale = cscale, ...)
  }
)

#' @rdname rmo-deprecated
#' @aliases intensities-deprecated
#' @export
setGeneric(
  "intensities",
  function(object, d, cscale = 1, ...) {
    .Deprecated("calcShockArrivalIntensities", "rmo")
    standardGeneric("intensities")
  }
)

#' @rdname rmo-deprecated
#' @usage NULL
#' @include s4-calcShockArrivalIntensities.R
#' @export
setMethod(
  "intensities",
  "BernsteinFunction",
  function(object, d, cscale = 1, ...) {
    calcShockArrivalIntensities(object, d, cscale = cscale, ...)
  }
)

#' @rdname rmo-deprecated
#' @aliases defaultMethod-deprecated
#' @export
setGeneric(
  "defaultMethod",
  function(object) {
    .Deprecated("getDefaultMethodString", "rmo")
    standardGeneric("defaultMethod")
  }
)

#' @rdname rmo-deprecated
#' @usage NULL
#' @include s4-getDefaultMethodString.R
#' @export
setMethod(
  "defaultMethod",
  "BernsteinFunction",
  function(object) {
    getDefaultMethodString(object)
  }
)

#' @rdname rmo-deprecated
#' @aliases levyDensity-deprecated
#' @export
setGeneric(
  "levyDensity",
  function(object) {
    .Deprecated("getLevyDensity", "rmo")
    standardGeneric("levyDensity")
  }
)

#' @rdname rmo-deprecated
#' @usage NULL
#' @include s4-getLevyDensity.R
#' @export
setMethod(
  "levyDensity",
  "BernsteinFunction",
  function(object) {
    getLevyDensity(object)
  }
)

#' @rdname rmo-deprecated
#' @aliases stieltjesDensity-deprecated
#' @export
setGeneric(
  "stieltjesDensity",
  function(object) {
    .Deprecated("getStieltjesDensity", "rmo")
    standardGeneric("stieltjesDensity")
  }
)

#' @rdname rmo-deprecated
#' @usage NULL
#' @include s4-getStieltjesDensity.R
#' @export
setMethod(
  "stieltjesDensity",
  "BernsteinFunction",
  function(object) {
    getStieltjesDensity(object)
  }
)

#' @rdname rmo-deprecated
#' @aliases valueOf-deprecated
#' @export
setGeneric(
  "valueOf",
  function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) {
    .Deprecated("calcIterativeDifference", "rmo")
    standardGeneric("valueOf")
  }
)

#' @rdname rmo-deprecated
#' @usage NULL
#' @include s4-calcIterativeDifference.R
#' @export
setMethod(
  "valueOf",
  "BernsteinFunction",
  function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) {
    calcIterativeDifference(
      object, x,
      difference_order = difference_order, n = n, k = k, cscale = cscale, ...
    )
  }
)

#' @rdname rmo-deprecated
#' @aliases valueOf0-deprecated
#' @export
setGeneric(
  "valueOf0",
  function(object, x, cscale = 1, ...) {
    .Deprecated("calcValue", "rmo")
    standardGeneric("valueOf0")
  }
)

#' @rdname rmo-deprecated
#' @usage NULL
#' @include s4-calcValue.R
#' @export
setMethod(
  "valueOf0",
  "BernsteinFunction",
  function(object, x, cscale = 1, ...) {
    calcValue(object, x, cscale = cscale, ...)
  }
)
