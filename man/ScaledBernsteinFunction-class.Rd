% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bernstein.R
\docType{class}
\name{ScaledBernsteinFunction-class}
\alias{ScaledBernsteinFunction-class}
\alias{ScaledBernsteinFunction}
\title{Class for \emph{scaled Bernstein functions}}
\description{
Berstein functions are stable under (non-negative) scalar multiplication,
i.e. if \eqn{\psi} is a Bernstein function and \eqn{\lambda \geq 0} , then
\deqn{
  x \mapsto \lambda \psi(x), x > 0,
}
is also a Bernstein function.
}
\section{Slots}{

\describe{
\item{\code{scale}}{The scalar factor with which the original Bernstein function
is to be multiplied.}

\item{\code{original}}{The original Bernstein function which is to be multiplied.}
}}

\examples{
original_bf <- AlphaStableBernsteinFunction(alpha=0.5)
bf <- ScaledBernsteinFunction(scale=2, original=original_bf)

}
\seealso{
\linkS4class{BernsteinFunction}
}