#' Class for convex combinations of Bernstein functions
#'
#' Bernstein functions are stable under convex combinations, i.e. if
#' \eqn{\psi_1, \ldots, \psi_n} are Bernstein functions and
#' \eqn{c_1, \ldots, c_n > 0} are positive real values,
#' \deqn{
#'   x \mapsto c_1 \psi_1(x) + \cdots + c_n \psi_n(x) , x>0,
#' }
#' is also a Bernstein function.
#'
#' @slot coefficients Numeric vector of positive real values.
#' @slot points List of Bernstein functions.
#'
#' @seealso [valueOf()], [intensities()], [uexIntensities()], [exIntensities()],
#'   [exQMatrix()], [rextmo()], [rpextmo()]
#'
#' @docType class
#' @name ConvexCombinationOfBernsteinFunctions-class
#' @rdname ConvexCombinationOfBernsteinFunctions-class
#' @aliases ConvexCombinationOfBernsteinFunctions
#' @include s4-BernsteinFunction.R
#' @family Bernstein function classes
#' @family Bernstein function transformer classes
#' @export ConvexCombinationOfBernsteinFunctions
#' @examples
#' # Create an object of class ConvexCombinationOfBernsteinFunctions
#' ConvexCombinationOfBernsteinFunctions()
#' ConvexCombinationOfBernsteinFunctions(
#'   coefficients = c(0.2, 0.5, 0.1),
#'   points = list(
#'     LinearBernsteinFunction(scale = 0.2),
#'     ConstantBernsteinFunction(constant = 0.5),
#'     AlphaStableBernsteinFunction(alpha = 0.5))
#' )
ConvexCombinationOfBernsteinFunctions <- setClass( # nolint
  "ConvexCombinationOfBernsteinFunctions",
  contains = "BernsteinFunction",
  slots = c(
    coefficients = "numeric",
    points = "list"
  )
)

#' @rdname hidden_aliases
#'
#' @inheritParams methods::initialize
#' @param coefficients Derives from
#'   [ConvexCombinationOfBernsteinFunctions-class].
#' @param points Derives from [ConvexCombinationOfBernsteinFunctions-class].
setMethod(
  "initialize",
  "ConvexCombinationOfBernsteinFunctions",
  function(.Object, coefficients, points) { # nolint
    if (!(missing(coefficients) || missing(points))) {
      .Object@coefficients <- coefficients # nolint
      .Object@points <- points # nolint
      validObject(.Object)
    }

    invisible(.Object)
  }
)

#' @include error.R
#' @importFrom checkmate qtest test_list
setValidity(
  "ConvexCombinationOfBernsteinFunctions",
  function(object) {
    if (!qtest(object@coefficients, "R+(0,)")) {
      return(error_msg_domain("coefficients", "R+(0,)"))
    }
    if (!(
          test_list(
            object@points,
            types = "BernsteinFunction",
            any.missing = FALSE,
            len = length(object@coefficients)
          ) &&
            all(
              sapply(
                object@points,
                function(object) {
                  isTRUE(
                    validObject(
                      object,
                      test = TRUE, complete = TRUE
                    )
                  )
                }
              )
            ))) {
      return(
        error_msg_domain(
          "points",
          sprintf(
            "list of valid Bernstein functions of length %i",
            length(object@coefficients)
          )
        )
      )
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
  "show",
  "ConvexCombinationOfBernsteinFunctions",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      for (i in seq_along(object@coefficients)) {
        cat(
          sprintf(
            "- coefficient: %s\n", format(object@coefficients[[i]])
          )
        )
        cat("- point:\n")
        writeLines(
          paste0("\t", capture.output(show(object@points[[i]])))
        )
      }
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  }
) # nocov end

#' @rdname hidden_aliases
#'
#' @inheritParams valueOf
#'
#' @include s4-valueOf.R
#' @export
setMethod(
  "valueOf",
  "ConvexCombinationOfBernsteinFunctions",
  function(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) { # nolint
    drop(
      t(object@coefficients) %*%
        drop(t(sapply(
          object@points,
          valueOf,
          x = x,
          difference_order = difference_order,
          n = n,
          k = k,
          cscale = cscale,
          ...
        )))
    )
  }
)
