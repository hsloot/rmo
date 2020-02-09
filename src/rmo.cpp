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
//' Draws `n` independent samples from a `d`-variate Marshall-Olkin distribution
//' with shock rates `intensities`.
//'
//' - The shock intensities must be stored in a vector of length \eqn{2^d-1}.
//' - A shock intensity of zero corresponds to an almost surely infinite shock.
//' - We use a binary representation to map a non-empty subset \eqn{J} of \eqn{\{
//' 1, \ldots, d\}}{{1, \ldots, d}} to integers \eqn{j} of \eqn{1, \ldots, 2^d-1}. In
//' particular, \eqn{i} is a component in the set \eqn{J} corresponding to the integer \eqn{j} iff
//' \eqn{j = \sum_{k=0}^\infty a_k 2^k}{\sum a[k] * 2^k} and \eqn{a_{i-1} = 1}{a[i-1] = 1}.
//'
//' @param n number of samples
//' @param d dimension
//' @param intensities Marshall-Olkin intensity rates
//'
//' @return `rmo_esm` implements the *exogenous shock model* representation and
//' returns an \eqn{n \times d}{n x d} numeric matrix with the rows corresponding
//' to independent and identically distributed samples of a \eqn{d} variate
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
//' independent and identically distributed samples of a \eqn{d} variate
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




//' Sample from Cuadras-Auge distribution
//'
//' Draws `n` independent samples from a `d` variate Cuadras-Augé distribution
//' with parameters `alpha` and `beta`.
//'
//' - `alpha` is the shock intensity of shocks that affect only single
//' components.
//' - `beta` is the shock intensity of the global shock that affects all
//' components.
//'
//' @param n number of samples
//' @param d dimension
//' @param alpha rate of individual shocks
//' @param beta rate of global shock
//'
//' @return `rmo_esm_cuadras_auge` implements an optimized version of the
//' *exogenous shock model* representation for the Cuadras-Augé family and
//' returns an \eqn{n \times d}{n x d} array matrix with rows corresponding to
//' the independent samples of size \eqn{d}.
//'
//' @seealso \code{\link{rmo_esm}}
//' @family samplers
//'
//' @examples
//' rmo_esm_cuadras_auge(10L, 2L, 0.5, 0.2)
//' rmo_esm_cuadras_auge(10L, 2L, 0, 1)      ## comonotone
//' rmo_esm_cuadras_auge(10L, 2L, 1, 0)      ## independence
//'
//' @include assert.R
//' @importFrom assertthat assert_that is.count
//'
//' @export
//' @name rmo_esm_cuadras_auge
// [[Rcpp::export]]
NumericMatrix rmo_esm_cuadras_auge(unsigned int n, unsigned int d, double alpha, double beta) { // alpha, beta >= 0
	NumericVector individual_shocks;
	double global_shock;

	NumericMatrix out(n, d);
	for (unsigned int k=0; k<n; k++) {
		if ((d*k) % C_CHECK_USR_INTERRUP == 0)
			checkUserInterrupt();

		individual_shocks = Rcpp::rexp(d, alpha);
		global_shock = ((0 == beta) ? R_PosInf : exp_rand() / beta);

		for (unsigned int i=0; i<d; i++) {
			out(k, i) = min2(individual_shocks[i], global_shock);
		}
	}

	return wrap( out );
}



//' @rdname rmo_lfm_cpp
//'
//' A sampling function for a (possibly killed) compound Poisson subordinator
//' with non-negative jump distribution.
//'
//' @inheritParams rmo_lfm_cpp
//' @param barrier_values a vector of barrier values from the LFM to properly
//' incorporate first exit times over these `barrier_values` if killing or drift
//' is present.
//'
//' @return A named `k x 2` array with names `c("t", "value")`, where `k` is
//' random and each row represents a time-value tupel for a jump in the compound
//' Poisson subordinator.
//'
//' @include assert.R
//' @importFrom stats rexp
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix sample_cpp(double rate, double rate_killing, double rate_drift, Function rjump, List rjump_arg_list, NumericVector barrier_values) {
	barrier_values = clone(barrier_values);
	if (rate_drift>0) {
		std::sort(barrier_values.begin(), barrier_values.end());
	} else {
		barrier_values = NumericVector(1, max(barrier_values));
	}

	double waiting_time;
	double jump_value;
	double killing_time;

	double current_value;

	double intermediate_time;
	double intermediate_value;

	Function do_call("do.call");
	rjump_arg_list.push_back(1, "n");

	std::vector<double> times(1);
	std::vector<double> values(1);
	for (unsigned int i=0; i<barrier_values.size(); i++) {
		current_value = std::accumulate(values.begin(), values.end(), 0.);
		while (current_value < barrier_values[i]) {
			waiting_time = ((0 == rate) ? R_PosInf : R::exp_rand()/rate);
			// requires RNGstate synchronisation
			PutRNGstate();
			jump_value = as<double>(do_call(rjump, rjump_arg_list));
			GetRNGstate();
			killing_time = ((0 == rate_killing) ? R_PosInf : R::exp_rand()/rate_killing);

			if (killing_time < R_PosInf && killing_time <= waiting_time) {
				if (rate_drift > 0 && (barrier_values[i] - current_value)/rate_drift <= killing_time) {
					intermediate_time = (barrier_values[i] - current_value) / rate_drift;
					intermediate_value = intermediate_time * rate_drift;
					times.push_back(intermediate_time);
					values.push_back(intermediate_value);
					killing_time -= intermediate_time;
				}

				times.push_back(killing_time);
				values.push_back(R_PosInf);
			} else {
				if (rate_drift > 0 && (barrier_values[i] - current_value)/rate_drift <= waiting_time) {
					intermediate_time = (barrier_values[i] - current_value)/rate_drift;
					intermediate_value = intermediate_time * rate_drift;
					times.push_back(intermediate_time);
					values.push_back(intermediate_value);
					waiting_time -= intermediate_time;
				}

				times.push_back(waiting_time);
				values.push_back(waiting_time * rate_drift + jump_value);
			}
			current_value = std::accumulate(values.begin(), values.end(), 0.);
		}
	}

	NumericMatrix out(times.size(), 2);
	for (unsigned int i=0; i<times.size(); i++) {
	  out(i, 0) = (0 == i ? times[0] : out(i-1, 0) + times[i]);
	  out(i, 1) =(0 == i ? values[0] : out(i-1, 1) + values[i]);
	}
	colnames(out) = CharacterVector::create("t", "value");

	return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_lfm_cpp(unsigned int n, unsigned int d, double rate, double rate_killing, double rate_drift, Function rjump, List rjump_arg_list) {
  NumericVector unit_exponentials(d);
  NumericMatrix cpp_subordinator;
  IntegerVector idx;
  NumericVector tmp;
  unsigned int count;

  NumericMatrix out(n, d);
  for (unsigned int k=0; k<n; k++) {
    unit_exponentials = Rcpp::rexp(d);
    cpp_subordinator = sample_cpp(rate, rate_killing, rate_drift, rjump, rjump_arg_list, unit_exponentials);
    for (unsigned int i=0; i<d; i++) {
      count = 0;
      while (cpp_subordinator(count, 1) < unit_exponentials[i] && count < cpp_subordinator.nrow())
        count += 1;

      if (cpp_subordinator.nrow() == count)
        stop("internal error: exponential value out of subordinator range");

      out(k, i) = cpp_subordinator(count, 0);
    }
  }
  return out;
}
