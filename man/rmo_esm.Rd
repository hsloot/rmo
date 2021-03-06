% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample.R
\name{rmo_esm}
\alias{rmo_esm}
\alias{rmo_arnold}
\title{Sample from a Marshall--Olkin distribution}
\usage{
rmo_esm(n, d, intensities)

rmo_arnold(n, d, intensities)
}
\arguments{
\item{n}{Number of samples}

\item{d}{Dimension}

\item{intensities}{Marshall-Olkin shock intensities}
}
\value{
\code{rmo_esm} implements the \emph{exogenous shock model} algorithm and
returns an \eqn{n \times d}{n x d} numeric matrix with the rows
corresponding to independent and identically distributed samples of a
\eqn{d} variate Marshall-Olkin distribution with parameters \code{intensities}.

\code{rmo_arnold} implements the \emph{Arnold model} algorithm and returns
an \eqn{n \times d}{n x d} numeric matrix with the rows corresponding to
independent and identically distributed samples of a \eqn{d} variate
Marshall-Olkin distribution with parameters \code{intensities}.
}
\description{
Draws \code{n} independent samples from a \code{d}-variate Marshall-Olkin distribution
with shock rates \code{intensities}.
}
\details{
\strong{Parameterisation}:
The Marshall--Olkin distribution has the survival function
\deqn{
  \bar{F}(t)
  \bar{F}(t)
     = \exp{\left\{ - \sum_{I} \lambda_I \max_{i \in I} t_i \right\}} , \quad t > 0 ,
}
for \emph{shock intensities} \eqn{\lambda_I \geq 0},
\eqn{\emptyset \neq I \subseteq {\{ 1 , \ldots, d \}}}.

\strong{The shock intensities}:
\itemize{
\item The shock \code{intensities} must be stored in a vector of length
\eqn{2^d-1}.
\item A shock intensity of zero corresponds to an almost surely infinite
shock.
\item We use a binary representation to map a non-empty subset \eqn{J}
of \eqn{\{ 1, \ldots, d\}}{{1, \ldots, d}} to integers \eqn{j} of
\eqn{1, \ldots, 2^d-1}.
In particular, \eqn{i} is a component in the set \eqn{J} corresponding to
the integer \eqn{j} if, and only if,
\eqn{j = \sum_{k=0}^\infty a_k 2^k}{\sum a[k] * 2^k}
and \eqn{a_{i-1} = 1}{a[i-1] = 1}.
}

\strong{The exogenous shock model} simulates a Marshall--Olkin distributed random
vector via exponentially distributed shock times for all non-empty subsets,
see \insertCite{@see pp. 104 psqq. @Mai2017a}{rmo} and
\insertCite{Marshall1967a}{rmo}.

\strong{The Arnold model} simulates a Marshall--Olkin distributed random variable
by simulating a marked homogeneous Poisson process and where the
inter-arrival times correspond to shock shock-arrival times and the marks to
the specific shocks, see \insertCite{@see Sec. 3.1.2 @Mai2017a}{rmo} and
\insertCite{Arnold1975a}{rmo}.
}
\examples{
rmo_esm(10L, 2L, c(0.4, 0.3, 0.2))
rmo_esm(10L, 2L, c(1, 1, 0))         ## independence
rmo_esm(10L, 2L, c(0, 0, 1))         ## comonotone

rmo_arnold(10L, 2L, c(0.4, 0.3, 0.2))
rmo_arnold(10L, 2L, c(1, 1, 0))         ## independence
rmo_arnold(10L, 2L, c(0, 0, 1))         ## comonotone

}
\references{
\insertAllCited{}
}
\seealso{
Other sampling-algorithms: 
\code{\link{rarmextmo_esm}()},
\code{\link{rexmo_markovian}()},
\code{\link{rextmo_lfm}()}

Other sampling-algorithms: 
\code{\link{rarmextmo_esm}()},
\code{\link{rexmo_markovian}()},
\code{\link{rextmo_lfm}()}
}
\concept{sampling-algorithms}
