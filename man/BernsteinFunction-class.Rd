% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bernstein.R
\docType{class}
\name{BernsteinFunction-class}
\alias{BernsteinFunction-class}
\title{Virtual Class \code{BernsteinFunction} for Bernstein Functions}
\description{
A virtual superclass for all implementations of the various classes of
Bernstein functions which have a representaion
\deqn{
 \psi(x) = a + b x + \int_0^\infty (1 - e^{-ux}) \nu(du) , x > 0
}
for non-negative constants \eqn{a, b \geq 0}, called \emph{killing rate} and
\emph{drift}, and a \emph{Lévy measure} \eqn{\nu} on \eqn{(0, \infty)}.
}
\details{
Bernstein functions have a 1-1 relationship to Lévy subordinators s.t. for
every Bernstein function \eqn{\psi}, there exists a Lévy subordinator
\eqn{\Lambda} with
\deqn{
  E[e^{-x \Lambda_t}]
    = e^{-t \psi(x)} , t, x > 0 .
}
\itemize{
\item \eqn{a} Describes the (exponential) \emph{killing rate}, which
determines the rate of an exponentially distributed event that sends
the subordinator to infinity.
\item \eqn{b} Describes the linear \emph{drift} of the Lévy subordinator.
\item \eqn{\nu} determines the arrival-rate and shock-size distribution
for the jumps of the Lévy subordinator.
}

For a theoretic treatment of Bernstein function, we refer to
\insertCite{Schilling2012a}{rmo}.
}
\references{
\insertAllCited{}
}
\seealso{
\linkS4class{ConstantBernsteinFunction},
\linkS4class{LinearBernsteinFunction},
\linkS4class{PoissonBernsteinFunction},
\linkS4class{AlphaStableBernsteinFunction},
\linkS4class{ScaledBernsteinFunction},
\linkS4class{SumOfBernsteinFunctions}
}