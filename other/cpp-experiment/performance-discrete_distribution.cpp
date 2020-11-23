// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::depends(BH, rmo)]]

#include <random>

#include <Rcpp.h>
#include <boost/random.hpp>
#include <rmolib/random/univariate/r_discrete_distribution.hpp>

// [[Rcpp::export]]
void construct_rmolib(Rcpp::NumericVector probs) {
  using uniform_distribution = std::uniform_real_distribution<double>;
  using discrete_distribution =
      rmolib::random::r_discrete_distribution<int, double,
                                              uniform_distribution>;

  [[maybe_unused]] discrete_distribution dist{probs.begin(), probs.end()};
}

// [[Rcpp::export]]
void construct_std(Rcpp::NumericVector probs) {
  using discrete_distribution = std::discrete_distribution<int>;

  [[maybe_unused]] discrete_distribution dist{probs.begin(), probs.end()};
}

// [[Rcpp::export]]
void construct_boost(Rcpp::NumericVector probs) {
  using discrete_distribution = boost::random::discrete_distribution<int>;

  [[maybe_unused]] discrete_distribution dist{probs.begin(), probs.end()};
}

// [[Rcpp::export]]
Rcpp::NumericVector construct_and_sample_rmolib(int n,
                                                Rcpp::NumericVector probs) {
  using uniform_distribution = std::uniform_real_distribution<double>;
  using discrete_distribution =
      rmolib::random::r_discrete_distribution<int, double,
                                              uniform_distribution>;

  discrete_distribution dist{probs.begin(), probs.end()};
  std::random_device device{};
  std::mt19937 engine{device()};

  Rcpp::NumericVector out(Rcpp::no_init(n));
  std::generate(out.begin(), out.end(),
                [&dist = dist, &engine = engine]() { return dist(engine); });

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector construct_and_sample_std(int n, Rcpp::NumericVector probs) {
  using discrete_distribution = std::discrete_distribution<int>;

  discrete_distribution dist{probs.begin(), probs.end()};
  std::random_device device{};
  std::mt19937 engine{device()};

  Rcpp::NumericVector out(Rcpp::no_init(n));
  std::generate(out.begin(), out.end(),
                [&dist = dist, &engine = engine]() { return dist(engine); });

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector construct_and_sample_boost(int n,
                                               Rcpp::NumericVector probs) {
  using discrete_distribution = boost::random::discrete_distribution<int>;

  discrete_distribution dist{probs.begin(), probs.end()};
  std::random_device device{};
  std::mt19937 engine{device()};

  Rcpp::NumericVector out(Rcpp::no_init(n));
  std::generate(out.begin(), out.end(),
                [&dist = dist, &engine = engine]() { return dist(engine); });

  return out;
}

/*** R
results_construct <- bench::press(
  size = c(125L,2L^10-1L, 2L^15-1L),
  {
    probs <- rexp(size)
    bench::mark(
      construct_rmolib(probs),
      construct_std(probs),
      construct_boost(probs),
      check=FALSE,
      min_iterations=1e2L
    )
  }
)
summary(results_construct)


results_construct_and_sample <- bench::press(
  size =  2L^15-1L,
  num = c(1e2L, 1e3L, 1e4L),
  {
    n = num
    probs <- rexp(size)
    bench::mark(
      construct_and_sample_rmolib(n, probs),
      construct_and_sample_std(n, probs),
      construct_and_sample_boost(n, probs),
      check=FALSE,
      min_iterations=1e2L
    )
  }
)
summary(results_construct_and_sample)
*/
