#' @include error.R
#' @include s4-BernsteinFunction.R
NULL

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
#' @export ConvexCombinationOfBernsteinFunctions
ConvexCombinationOfBernsteinFunctions <- setClass( # nolint
  "ConvexCombinationOfBernsteinFunctions",
  contains = "BernsteinFunction",
  slots = c(
    coefficients = "numeric",
    points = "list"
  )
)

#' @describeIn ConvexCombinationOfBernsteinFunctions-class Constructor
#' @aliases initialize,ConvexCombinationOfBernsteinFunctions-method
#' @aliases initialize,ConvexCombinationOfBernsteinFunctions,ANY-method
#'
#' @inheritParams methods::initialize
#' @param coefficients Derives from
#'   [ConvexCombinationOfBernsteinFunctions-class].
#' @param points Derives from [ConvexCombinationOfBernsteinFunctions-class].
#'
#' @examples
#' ConvexCombinationOfBernsteinFunctions()
#' bf1 <- LinearBernsteinFunction(scale = 0.2)
#' bf2 <- ConstantBernsteinFunction(constant = 0.5)
#' bf3 <- AlphaStableBernsteinFunction(alpha = 0.5)
#' ConvexCombinationOfBernsteinFunctions(
#'   coefficients = c(0.2, 0.5, 0.1),
#'   points = list(bf1, bf2, bf3)
#' )
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

#' @describeIn ConvexCombinationOfBernsteinFunctions-class Display the object.
#' @aliases show,ConvexCombinationOfBernsteinFunctions-method
#'
#' @importFrom utils capture.output
#'
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

#' @describeIn ConvexCombinationOfBernsteinFunctions-class
#'   Calculates the iterated differences of the Bernstein function,
#'   see [valueOf()]
#' @aliases valueOf,ConvexCombinationOfBernsteinFunctions,ANY-method
#'
#' @inheritParams valueOf
#'
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
