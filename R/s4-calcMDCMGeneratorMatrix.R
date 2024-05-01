#' Calculate the MDCM Markovian generator matrix
#'
#' Calculates the *infinitesimal Markov generator matrix* of the corresponding
#' (Markovian) default-counting process, used internally by [rexmo()].
#'
#' @inheritParams calcExShockArrivalIntensities
#'
#' @details
#' For a given Bernstein function, the Markov generator matrix is defined as the
#' upper triangular matrix with elements
#' \deqn{
#'   q_{i, j}^\ast
#'     = \binom{d-i}{j-i} \begin{cases}
#'       -\psi{(d-i)} & \text{if } i = j , \\
#'       {(-1)}^{j-i-1} \Delta^{j-i}{ \psi{(d-i)} } & \text{if } i < j , \\
#'      0 & \text{otherwise} .
#'    \end{cases}
#' }
#' The calculation of the Markov generator matrix using this formula is usually
#' not numerically stable. Consequently, the various alternative approaches are
#' used dependent on the class of the Bernstein function.
#'
#' The (upper triangular) infinitesimal Markov generator of the associated
#' death-counting process is calculated recursively:
#' \deqn{
#'   q_{0, i}^\ast
#'     = \eta_{i} ,
#'       \quad i \in {\{ 1 , \ldots , d \}} ,
#' }
#' and
#' \deqn{
#'   q_{i+1, j+1}^\ast
#'     = \frac{d-j}{d-i} q_{i,j}^\ast + \frac{j+1-i}{d-i} q_{i, j+1}^\ast ,
#'      \quad 0 \leq i < j \leq d .
#' }
#'
#' @seealso [rexmo()]
#'
#' @importFrom methods setGeneric
#' @family Bernstein function generics
#' @export
#' @examples
#' bf <- AlphaStableBernsteinFunction(alpha = 0.7)
#' calcMDCMGeneratorMatrix(bf, 3)
setGeneric(
  "calcMDCMGeneratorMatrix",
  function(object, d, cscale = 1, ...) {
    standardGeneric("calcMDCMGeneratorMatrix")
  }
)
