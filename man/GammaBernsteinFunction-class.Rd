% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s4-GammaBernsteinFunction.R
\docType{class}
\name{GammaBernsteinFunction-class}
\alias{GammaBernsteinFunction-class}
\alias{GammaBernsteinFunction}
\alias{initialize,GammaBernsteinFunction-method}
\alias{initialize,GammaBernsteinFunction,ANY-method}
\alias{show,GammaBernsteinFunction-method}
\alias{levyDensity,GammaBernsteinFunction-method}
\alias{stieltjesDensity,GammaBernsteinFunction-method}
\title{Class for Gamma Bernstein functions}
\usage{
\S4method{initialize}{GammaBernsteinFunction}(.Object, a)

\S4method{show}{GammaBernsteinFunction}(object)

\S4method{levyDensity}{GammaBernsteinFunction}(object)

\S4method{stieltjesDensity}{GammaBernsteinFunction}(object)
}
\arguments{
\item{.Object}{ An object:  see the \dQuote{Initialize Methods} section.}

\item{a}{Positive number.}

\item{object}{An object deriving from \linkS4class{LevyBernsteinFunction}
(for \code{levyDensity}) or \linkS4class{CompleteBernsteinFunction}
(for \code{stieltjesDensity}).}
}
\description{
The \emph{Gamma Bernstein function}, is the Bernstein function of a
subordinator with a (scaled) Gamma distribution. The representation is for
\eqn{a > 0}
\deqn{
  \psi(x) = \log(1 + \frac{x}{a}), x > 0.
}
}
\details{
For this Bernstein function, the higher-order alternating iterated forward
differences are known in closed form but cannot be evaluated numerically
without the danger of loss of significance. But we can use numerical
integration (here: \code{\link[stats:integrate]{stats::integrate()}}) to approximate it with the
following representation:
\deqn{
  {(-1)}^{k-1} \Delta^{k} \psi(x)
    = \int_{0}^{\infty} e^{-ux} {(1 - e^{-u})}^{k}
      \frac{e^{-au}}{u} du, x>0, k>0.
}

This Bernstein function is no. 26 in the list of complete Bernstein functions
in Chp. 16 of \insertCite{Schilling2012a}{rmo}.
}
\section{Functions}{
\itemize{
\item \code{initialize(GammaBernsteinFunction)}: Constructor

\item \code{show(GammaBernsteinFunction)}: Display the object.

\item \code{levyDensity(GammaBernsteinFunction)}: see \linkS4class{LevyBernsteinFunction}

\item \code{stieltjesDensity(GammaBernsteinFunction)}: see \linkS4class{CompleteBernsteinFunction}

}}
\section{Slots}{

\describe{
\item{\code{a}}{Scale parameter for the Lévy measure.}
}}

\section{Lévy density}{

\deqn{
  \nu(du)
    = \frac{\operatorname{e}^{-a u}}{u}, \quad u > 0 .
}
}

\section{Stieltjes density}{

\deqn{
  \sigma(du)
    = 1 / u du, u > a .
}
}

\examples{
GammaBernsteinFunction()
GammaBernsteinFunction(a = 2)
}
\references{
\insertAllCited{}
}
\seealso{
\linkS4class{BernsteinFunction}, \linkS4class{LevyBernsteinFunction},
\linkS4class{CompleteBernsteinFunction}, \code{\link[=valueOf]{valueOf()}}
}
