% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample-rmo.R
\name{rmo}
\alias{rmo}
\title{Sample from Marshall–Olkin distributions}
\usage{
rmo(n, d, intensities, method = c("AM", "ESM"))
}
\arguments{
\item{n}{an integer for the \emph{number of samples}.}

\item{d}{an integer for the \emph{dimension}.}

\item{intensities}{a numeric vector for the \emph{shock-arrival intensities}.}

\item{method}{a string indicating which sampling algorithm should be used.
Use "AM" for the \emph{Arnold model} and "ESM" for the \emph{exogenous shock model}.
We recommend using the \emph{ESM} for small dimensions only; the \emph{AM} can be
used up until dimension \eqn{30}.}
}
\value{
\code{rmo} returns a numeric matrix with \code{n} rows and \code{d} columns, with
the rows corresponding to iid distributed samples of a \code{d}-variate
\emph{Marshall–Olkin distribution} with \emph{shock-arrival intensities}
\code{intensities}.
}
\description{
Draws \code{n} iid samples from a \code{d}-variate \emph{Marshall–Olkin distribution}
parametrized by \emph{shock-arrival intensities}.
}
\details{
The \emph{Marshall–Olkin distribution} was introduced in
\insertCite{Marshall1967a}{rmo} and has the survival function
\deqn{
    \bar{F}{(t)}
        = \exp{\left\{ - \sum_{I} \lambda_I \max_{i \in I} t_i \right\}} ,
            \quad t = {(t_{1}, \ldots, t_{d})} > 0 ,
}
for \emph{shock-arrival intensities} \eqn{\lambda_I \geq 0},
\eqn{\emptyset \neq I \subseteq {\{ 1 , \ldots, d \}}}.
They are called \emph{shock-arrival intensities} as they correspond to the rates
of independent exponential random variables from the
\emph{exogenous shock model (ESM)}, and a shock-arrival intensity
\eqn{\lambda_{I}} of shock \eqn{I} equal to zero implies that the the shock
\eqn{I} never arrives. We use the following binary representation to map a
subsets of \eqn{\{ 1, \ldots, d\}}{{1, \ldots, d}} to integers
\eqn{0, \ldots, 2^d-1}:
\deqn{
  I \equiv \sum_{k \in I}{ 2^{k-1} }
}
\subsection{Simulation algorithms}{
\subsection{Exogenous shock model}{

The \emph{exogenous shock model (ESM)} simulates a Marshall–Olkin distributed
random vector via independent exponentially distributed shock times for all
non-empty subsets of components and defines each component as the minimum of
all shock times corresponding to a subset containing this component, see
\insertCite{@see pp. 104 psqq. @Mai2017a}{rmo} and
\insertCite{Marshall1967a}{rmo}.
}

\subsection{Arnold model}{

The \emph{Arnold model (AM)} simulates a Marshall–Olkin distributed random vector
by simulating a marked homogeneous Poisson process with set-valued marks.
The process is stopped when all components are \emph{hit} by a shock,
see \insertCite{@see Sec. 3.1.2 @Mai2017a}{rmo} and
\insertCite{Arnold1975a}{rmo}.
}

}
}
\examples{
rmo(
    10, 3,
    c(0.4, 0.4, 0.1, 0.4, 0.1, 0.1, 0.4)
)
## independence
rmo(
    10, 3,
    c(1, 1, 0, 1, 0, 0, 0)
)
## comonotone
rmo(
    10, 3,
    c(0, 0, 0, 0, 0, 0, 1)
)

rmo(
    10, 3,
    c(0.4, 0.4, 0.1, 0.4, 0.1, 0.1, 0.4),
    method = "ESM"
)
## independence
rmo(
    10, 3,
    c(1, 1, 0, 1, 0, 0, 0),
    method = "ESM"
)
## comonotone
rmo(
    10, 3,
    c(0, 0, 0, 0, 0, 0, 1),
    method = "ESM"
)

rmo(
    10, 3,
    c(0.4, 0.4, 0.1, 0.4, 0.1, 0.1, 0.4),
    method = "AM"
)
## independence
rmo(
    10, 3,
    c(1, 1, 0, 1, 0, 0, 0),
    method = "AM"
)
## comonotone
rmo(
    10, 3,
    c(0, 0, 0, 0, 0, 0, 1),
    method = "AM"
)

}
\references{
\insertAllCited{}
}
\seealso{
Other sampling-algorithms: 
\code{\link{rexmo}()},
\code{\link{rextmo}()},
\code{\link{rpextmo}()}
}
\concept{sampling-algorithms}
