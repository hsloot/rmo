#' @importFrom utils capture.output
#' @include allClass-S4.R
NULL

#' @describeIn LinearBernsteinFunction-class Display the object.
#' @aliases show,LinearBernsteinFunction-method
#'
#' @export
setMethod("show", "LinearBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- scale: %s\n", format(object@scale)))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  })

#' @describeIn ConstantBernsteinFunction-class Display the object.
#' @aliases show,ConstantBernsteinFunction-method
#'
#' @export
setMethod("show", "ConstantBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- constant: %s\n", format(object@constant)))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  })

#' @describeIn ScaledBernsteinFunction-class Display the object.
#' @aliases show,ScaledBernsteinFunction-method
#'
#' @export
setMethod("show", "ScaledBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- scale: %s\n", format(object@scale)))
      cat("- original:\n")
      writeLines(
        paste0("\t", capture.output(show(object@original))))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  })

#' @describeIn SumOfBernsteinFunctions-class Display the object.
#' @aliases show,SumOfBernsteinFunctions-method
#'
#' @export
setMethod("show", "SumOfBernsteinFunctions",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat("- first:\n")
      writeLines(
        paste0("\t", capture.output(show(object@first))))
      cat("- second:\n")
      writeLines(
        paste0("\t", capture.output(show(object@second))))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  })

#' @describeIn CompositeScaledBernsteinFunction-class Display the object.
#' @aliases show,CompositeScaledBernsteinFunction-method
#'
#' @export
setMethod("show", "CompositeScaledBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- cscale: %s\n", format(object@cscale)))
      cat("- original:\n")
      writeLines(
        paste0("\t", capture.output(show(object@original))))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  })

#' @describeIn PoissonBernsteinFunction-class Display the object.
#' @aliases show,PoissonBernsteinFunction-method
#'
#' @export
setMethod("show", "PoissonBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- lambda: %s\n", format(object@lambda)))
      cat(sprintf("- eta: %s\n", format(object@eta)))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  })

#' @describeIn AlphaStableBernsteinFunction-class Display the object.
#' @aliases show,AlphaStableBernsteinFunction-method
#'
#' @export
setMethod("show", "AlphaStableBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- alpha: %s\n", format(object@alpha)))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  })

#' @describeIn InverseGaussianBernsteinFunction-class Display the object.
#' @aliases show,InverseGaussianBernsteinFunction-method
#'
#' @export
setMethod("show", "InverseGaussianBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- eta: %s\n", format(object@eta)))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  })

#' @describeIn ExponentialBernsteinFunction-class Display the object.
#' @aliases show,ExponentialBernsteinFunction-method
#'
#' @export
setMethod("show", "ExponentialBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- lambda: %s\n", format(object@lambda)))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  })

#' @describeIn GammaBernsteinFunction-class Display the object.
#' @aliases show,GammaBernsteinFunction-method
#'
#' @export
setMethod("show", "GammaBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- a: %s\n", format(object@a)))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  })

#' @describeIn ParetoBernsteinFunction-class Display the object.
#' @aliases show,ParetoBernsteinFunction-method
#'
#' @export
setMethod("show", "ParetoBernsteinFunction",
  function(object) {
    cat(sprintf("An object of class %s\n", classLabel(class(object))))
    if (isTRUE(validObject(object, test = TRUE))) {
      cat(sprintf("- alpha: %s\n", format(object@alpha)))
      cat(sprintf("- x0: %s\n", format(object@x0)))
    } else {
      cat("\t (invalid or not initialized)\n")
    }

    invisible(NULL)
  })
