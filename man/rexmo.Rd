% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample-rexmo.R
\name{rexmo}
\alias{rexmo}
\title{Simulate from exchangeable Marshall–Olkin distributions}
\usage{
rexmo(n, d, theta, method = c("MDCM", "AM", "ESM"))
}
\arguments{
\item{n}{An integer for the \emph{number of samples}.}

\item{d}{An integer for the \emph{dimension}.}

\item{theta}{A numeric vector with the exchangeable \emph{shock-size
arrival intensities}.}

\item{method}{A string indicating which sampling algorithm should be used.
Use \code{"MDCM"} for the \emph{Markovian death-counting model}, \code{"AM"} for the
\emph{Arnold model}, and \code{"ESM"} for the \emph{exogenous shock model}. We recommend
using the \emph{ESM} only for small dimensions; the \emph{AM} can be used up until
dimension \eqn{30}.}
}
\value{
\code{rexmo} returns a numeric matrix of size \code{n} x \code{d}. Each row corresponds to
an independently and identically (iid) distributed sample of a \code{d}-variate
\emph{exchangeable Marshall–Olkin distribution} with specified parameters.
}
\description{
Draws \code{n} iid samples from a \code{d}-variate \emph{exchangeable Marshall–Olkin
distribution} parametrized by a vector of exchangeable \emph{shock-size arrival
intensities}.
}
\details{
The \emph{exchangeable Marshall–Olkin distribution} has the survival function
\deqn{
    \bar{F}{(t)}
        = \exp{\left\{
            -\sum_{i=1}^{d}{
                {\left[
                    \sum_{j=0}^{d-i}{ \binom{d-i}{j} \lambda_{j+1} }
                \right]}
                \tau_{[i]}
            }
        \right\}} ,
          \quad t = {(t_{1}, \ldots, t_{d})} > 0 ,
}
for \emph{exchangeable shock arrival intensities}
\eqn{\lambda_{i} \geq 0}, \eqn{1 \leq i \leq d}
and \eqn{t_{[1]} \geq \cdots \geq t_{[d]}}, see \insertCite{Mai2017a}{rmo}.

The relationship of \emph{exchangeable shock-size arrival intensities} to the
\emph{shock-arrival intensities} of the \emph{Marshall–Olkin distribution} is given by:
\deqn{
    \eta_{i}
        = \binom{d}{i} \lambda_{i} ,
          \quad i \in {\{1, \ldots, n\}} .
}
The exchangeable shock-size arrival intensities correspond to the initial
transition rates of independent exponential random variables in the
\emph{Markovian death-counting model (MDCM)}.
\subsection{Simulation algorithms}{
\itemize{
\item The \emph{Markovian death-counting model (MDCM)} is a simulation algorithm used
to generate samples from an exchangeable Marshall-Olkin distribution. It
simulates the death-counting process of the random vector, which is a
Markov process, until all components are "dead". This process defines an
order statistic that is then used to obtain a sample through a random
permutation. For more details on this algorithm, refer to
\insertCite{Sloot2022a}{rmo}.
\item The \emph{exogenous shock model (ESM)} and \emph{Arnold model (AM)} simulation
algorithms can be used to generate samples from the general \emph{Marshall–Olkin
distribution}. In these algorithms, the exchangeable \emph{shock-size arrival
intensities} are converted to the corresponding \emph{shock-arrival intensities}
and passed to the \code{\link[=rmo]{rmo()}} function.
}
}
}
\examples{
rexmo(
  10, 3,
  c(1.2, 0.3, 0.4)
)
## independence
rexmo(
  10, 3,
  c(3, 0, 0)
)
## comonotone
rexmo(
  10, 3,
  c(0, 0, 1)
)

rexmo(
  10, 3,
  c(1.2, 0.3, 0.4),
  method = "MDCM"
)
## independence
rexmo(
  10, 3,
  c(3, 0, 0),
  method = "MDCM"
)
## comonotone
rexmo(
  10, 3,
  c(0, 0, 1),
  method = "MDCM"
)

rexmo(
  10, 3,
  c(1.2, 0.3, 0.4),
  method = "AM"
)
## independence
rexmo(
  10, 3,
  c(3, 0, 0),
  method = "AM"
)
## comonotone
rexmo(
  10, 3,
  c(0, 0, 1),
  method = "AM"
)

rexmo(
  10, 3,
  c(1.2, 0.3, 0.4),
  method = "ESM"
)
## independence
rexmo(
  10, 3,
  c(3, 0, 0),
  method = "ESM"
)
## comonotone
rexmo(
  10, 3,
  c(0, 0, 1),
  method = "ESM"
)
}
\references{
\insertAllCited{}
}
\seealso{
Other sampling-algorithms: 
\code{\link{rextmo}()},
\code{\link{rmo}()},
\code{\link{rpextmo}()}
}
\concept{sampling-algorithms}
