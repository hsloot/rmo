#' Class for composite scaled Bernstein functions
#'
#' Bernstein functions are stable under composition, i.e. if \eqn{\psi} is
#' a Bernstein function and `c > 0`, then
#' \deqn{
#'   x \mapsto \psi(c x)
#' }
#' is also a Bernstein function.
#'
#' @slot cscale The scale of the inner linear Bernstein function of the
#'   composition.
#' @slot original The original Bernstein function.
#'
#' @seealso [valueOf()], [intensities()], [calcExShockArrivalIntensities()],
#'   [calcExShockSizeArrivalIntensities()], [calcMDCMGeneratorMatrix()],
#'   [rextmo()], [rpextmo()]
#'
#' @docType class
#' @name CompositeScaledBernsteinFunction-class
#' @rdname CompositeScaledBernsteinFunction-class
#' @include s4-BernsteinFunction.R
#' @family Bernstein function classes
#' @family Bernstein function transformer classes
#' @export CompositeScaledBernsteinFunction
#' @examples
#' # Create an object of class CompositeScaleBernsteinFunction
#' CompositeScaledBernsteinFunction()
#' CompositeScaledBernsteinFunction(
#'   cscale = 0.5,
#'   original = AlphaStableBernsteinFunction(0.5)
#' )
CompositeScaledBernsteinFunction <- setClass("CompositeScaledBernsteinFunction", # nolint
  contains = "BernsteinFunction",
  slots = c(cscale = "numeric", original = "BernsteinFunction")
)

#' @rdname hidden_aliases
#'
#' @inheritParams methods::initialize
#' @param cscale Positive number.
#' @param original Derives from [BernsteinFunction-class].
setMethod(
  "initialize", "CompositeScaledBernsteinFunction",
  function(.Object, cscale, original) { # nolint
    if (!(missing(cscale) || missing(original))) {
      .Object@cscale <- cscale # nolint
      .Object@original <- original # nolint
      validObject(.Object)
    }

    invisible(.Object)
  }
)

#' @include error.R
#' @importFrom checkmate qtest
setValidity(
  "CompositeScaledBernsteinFunction",
  function(object) {
    if (!qtest(object@cscale, "N1[0,)")) {
      return(error_msg_domain("cscale", "N1[0,)"))
    }

    invisible(TRUE)
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams methods::show
#'
#' @importFrom utils capture.output
#' @export
setMethod( # nocov start
  "show", "CompositeScaledBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- cscale: %s\n", format(object@cscale)))
      cat("- original:\n")
      writeLines(
        paste0("\t", capture.output(show(object@original)))
      )
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  }
) # nocov end

#' @rdname hidden_aliases
#'
#' @inheritParams valueOf0
#'
#' @include s4-valueOf0.R
#' @export
setMethod(
  "valueOf0", "CompositeScaledBernsteinFunction",
  function(object, x, cscale = 1, ...) {
    valueOf(object, x, cscale = cscale)
  }
)

#' @rdname hidden_aliases
#'
#' @inheritParams valueOf
#'
#' @include s4-valueOf.R
#' @export
setMethod(
  "valueOf", "CompositeScaledBernsteinFunction",
  function(object, x, difference_order = 0L, n = 1, k = 0, cscale = 1, ...) {
    valueOf(
      object@original, x,
      difference_order = difference_order,
      n = n, k = k,
      cscale = cscale * object@cscale,
      ...
    )
  }
)
