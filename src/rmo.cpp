#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;

static const unsigned int C_CHECK_USR_INTERRUP = 100000;

//' @keywords internal
//' @noRd
bool is_within(unsigned int i, unsigned int j) {
	if (1 + log2(j+1) < i) // j >= 2^(i-1)-1  <=> 1+log2(j+1) >= i
		return false;

	int count = 1;
	while (j>0) {
		if (1 == (j%2) && count == i)
			return true;
		j /= 2;
		count++;
	}

	return false;
} // bool is_within(int i, int j);

//' @keywords internal
//' @noRd
double min2(double a, double b) {
	if (a == R_PosInf && b == R_PosInf) {
		return R_PosInf;
	} else if (a == R_PosInf) {
		return b;
	} else if (b == R_PosInf) {
		return a;
	}
	return (a < b ? a : b);
} // double min2(double a, double b);




//' Sample from a Marshall--Olkin distribution
//'
//' Draws `n` independent samples from a `d` variate Marshall-Olkin distribution
//' with shock rates `intensities`.
//'
//' - The shock intensities must be stored in a vector of length \eqn{2^d-1}.
//' - A shock intensity of zero corresponds to an almost surely infinite shock.
//' - We use a binary representation to map a non-empty subset \eqn{I} of \eqn{\{
//' 1, \ldots, d\}}{{1, \ldots, d}} to integers \eqn{1, \ldots, 2^d-1}. In
//' particular, \eqn{i} is a component in the set corresponding for \eqn{j} iff
//' \eqn{j = \sum_{k=0}^d a_k 2^k}{\sum a[k] * 2^k} and \eqn{a_i = 0}{a[i] = 0}.
//'
//' @param n number of samples
//' @param d dimension
//' @param intensities Marshall-Olkin intensity rates
//'
//' @return `rmo_esm` implements the *exogenous shock model* representation and
//' returns an \eqn{n \times d}{n x d} numeric matrix with the rows corresponding
//' to independent and identically disctributed samples of a \eqn{d} variate
//' Marshall-Olkin distribution with parameters `intensities`.
//'
//' @section References: For more information on these algorithms, see J.-F. Mai,
//' M. Scherer, "Simulating Copulas", World Scientific (2017), pp. 104 psqq.
//'
//' @family samplers
//'
//' @examples
//' rmo_esm(10L, 2L, c(0.4, 0.3, 0.2))
//' rmo_esm(10L, 2L, c(1, 1, 0))         ## independence
//' rmo_esm(10L, 2L, c(0, 0, 1))         ## comonotone
//'
//' @include assert.R sets.R
//' @importFrom stats rexp
//' @importFrom assertthat assert_that is.count
//'
//' @export
//' @name rmo_esm
// [[Rcpp::export]]
NumericMatrix rmo_esm(unsigned int n, unsigned int d, NumericVector intensities) {
	double intensity, shock_time;
	NumericVector value;

	NumericMatrix out(n, d);
	for (unsigned int k=0; k<n; k++) {
		if ((d*k) % C_CHECK_USR_INTERRUP == 0)
			checkUserInterrupt();

		value = NumericVector(d, R_PosInf);
		for (unsigned int j=0; j < pow(2., d)-1; j++) {
			intensity = intensities[j];
			shock_time = (intensity == 0 ? R_PosInf : R::exp_rand() / intensity);
			for (unsigned int i=0; i<d; i++) {
				if (is_within(i+1, j+1)) {
					value[i] = min2(value[i], shock_time);
				}
			}
		}

		out(k, _) = value;
	}
	return wrap( out );
}


//' @rdname rmo_esm
//'
//' @return `rmo_arnold` implements the *Arnold model* representation and returns
//' an \eqn{n \times d}{n x d} numeric matrix with the rows corresponding to
//' independent and identically disctributed samples of a \eqn{d} variate
//' Marshall-Olkin distribution with parameters `intensities`.
//'
//' @examples
//' rmo_arnold(10L, 2L, c(0.4, 0.3, 0.2))
//' rmo_arnold(10L, 2L, c(1, 1, 0))         ## independence
//' rmo_arnold(10L, 2L, c(0, 0, 1))         ## comonotone
//'
//' @include assert.R sets.R
//' @importFrom stats rexp
//' @importFrom assertthat assert_that is.count
//'
//' @export
// [[Rcpp::export]]
NumericMatrix rmo_arnold(unsigned int n, unsigned int d, NumericVector intensities) {
	double total_intensity, waiting_time;
	unsigned int affected;
	LogicalVector destroyed;
	NumericVector transition_probs, values;

	total_intensity = sum(intensities);
	transition_probs = intensities / total_intensity;

	NumericMatrix out(n, d);
	for (unsigned int k=0; k<n; k++) {
		if ((d*k) % C_CHECK_USR_INTERRUP == 0)
			checkUserInterrupt();

		destroyed = LogicalVector(d, false);
		values = NumericVector(d, 0.);

		while (is_false(all(destroyed))) {
			waiting_time = R::exp_rand() / total_intensity;
			affected = sample((int) pow(2., d)-1, 1, false, transition_probs, true)[0];

			for (unsigned int i=0; i<d; i++) {
				if (!destroyed[i]) {
					values[i] += waiting_time;
					if (is_within(i+1, affected)) {
						destroyed[i] = true;
					}
				}
			}
		}

		out(k, _) = values;
	}

	return wrap( out );
}




//' Sample from an exchangeable MO distribution
//'
//' Draws `n` independent samples from a `d` variate exchangeable Marshall-Olkin
//' distribution with shock rates `ex_intensities`.
//'
//' - The *exchangeable* shock intensities must be stored in a vector of length
//' \eqn{d}.
//' - The entry \eqn{{exintensities}_{i}}{ex_intensities[i]} is the
//' intensity of a shock corresponding to a set with \eqn{i} elements.
//'
//' @section References:
//' For more information on this algorithm, see J.-F. Mai, M. Scherer,
//' "Simulating Copulas", World Scientific (2017), pp. 122 psqq.
//'
//' @param n number of samples
//' @param d dimension
//' @param ex_intensities exchangeable Marshall-Olkin intensity rates
//'
//' @return `rmo_ex_arnold` implements the modified Arnold model for the
//' exchangeable subclass and returns an \eqn{n \times d}{n x d} numeric matrix
//' with the rows corresponding to independent and identically disctributed
//' samples of a \eqn{d} variate exchangeable Marshall-Olkin distribution with
//' exchangeable parameters `ex_intensities`.
//'
//' @family samplers
//'
//' @examples
//' rmo_ex_arnold(10, 2, c(0.4, 0.2))
//' rmo_ex_arnold(10, 2, c(1, 0))      ## independence
//' rmo_ex_arnold(10, 2, c(0, 1))      ## comonotone
//'
//' @include assert.R
//' @importFrom assertthat assert_that is.count
//'
//' @export
//' @name rmo_ex_arnold
// [[Rcpp::export]]
NumericMatrix rmo_ex_arnold(unsigned int n, unsigned int d, NumericVector ex_intensities) {
	double tmp, waiting_time;
	unsigned int state;
	double total_intensity;
	NumericVector transition_probs;
	NumericVector values;
	IntegerVector perm;

	NumericMatrix generator_matrix(d+1, d+1);
	for (unsigned int i=0; i<d+1; i++) {
		for (unsigned int j=0; j<d+1; j++) {
			if (j < i) {
				generator_matrix(i, j) = 0.;
			} else if (j > i) {
				tmp = 0.;
				for (unsigned int k=0; k<i+1; k++) {
					tmp += R::choose(i, k) * ex_intensities[k+j-i-1];
				}
				tmp *= R::choose(d-i, (j-i));
				generator_matrix(i, j) = tmp;
			}
		}
		generator_matrix(i, i) = -sum(generator_matrix(i, _));
	}

	NumericMatrix out(n, d);
	for (unsigned int k=0; k<n; k++) {
		if ((d*k) % C_CHECK_USR_INTERRUP == 0)
			checkUserInterrupt();

		values = NumericVector(d, 0.);
		state = 0;
		while (state < d) {
			total_intensity = -generator_matrix(state, state);
			transition_probs = NumericVector(d-state, 0.);
			for (unsigned int i=state+1; i<d+1; i++) {
				transition_probs[i-state-1] = generator_matrix(state, i) / total_intensity;
			}
			waiting_time = R::exp_rand() / total_intensity;
			for (unsigned int i=state; i<d; i++) {
				values[i] += waiting_time;
			}
			state += 1 + sample(d-state, 1, false, transition_probs, false)[0];
		}

		perm = sample(d, d, false, R_NilValue, false); // Use `RNGkind(sample.kind="Rounding")` for comparison, since R.3.6.x not implemented in Rcpp
		values = values[perm];
		out(k, _) = values;
	}
	return wrap( out );
}