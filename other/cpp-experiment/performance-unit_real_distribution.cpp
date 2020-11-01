// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::depends(BH, rmo)]]

#include <random>

#include <Rcpp.h>
#include <boost/random.hpp>
#include <rmolib/random/uniform_real_distribution.hpp>

// // [[Rcpp::export]]
// void construct_rmolib() {
//   using uniform_real_distribution =
//       rmolib::random::uniform_real_distribution<double>;
//
//   [[maybe_unused]] uniform_real_distribution dist{};
// }

// [[Rcpp::export]]
void construct_std() {
  using uniform_real_distribution = std::uniform_real_distribution<double>;

  [[maybe_unused]] uniform_real_distribution dist{};
}

// [[Rcpp::export]]
void construct_boost() {
  using uniform_real_distribution =
      boost::random::uniform_real_distribution<double>;

  [[maybe_unused]] uniform_real_distribution dist{};
}

// // [[Rcpp::export]]
// Rcpp::NumericVector construct_and_sample_rmolib(int n) {
//   using uniform_real_distribution =
//       rmolib::random::uniform_real_distribution<double>;
//
//   uniform_real_distribution dist{};
//   std::random_device device{};
//   std::mt19937 engine{device()};
//
//   Rcpp::NumericVector out(Rcpp::no_init(n));
//   std::generate(out.begin(), out.end(),
//                 [&dist = dist, &engine = engine]() { return dist(engine); });
//
//   return out;
// }

// [[Rcpp::export]]
Rcpp::NumericVector construct_and_sample_std(int n) {
  using uniform_real_distribution = std::uniform_real_distribution<double>;

  uniform_real_distribution dist{};
  std::random_device device{};
  std::mt19937 engine{device()};

  Rcpp::NumericVector out(Rcpp::no_init(n));
  std::generate(out.begin(), out.end(),
                [&dist = dist, &engine = engine]() { return dist(engine); });

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector construct_and_sample_boost(int n) {
  using uniform_real_distribution =
      boost::random::uniform_real_distribution<double>;

  uniform_real_distribution dist{};
  std::random_device device{};
  std::mt19937 engine{device()};

  Rcpp::NumericVector out(Rcpp::no_init(n));
  std::generate(out.begin(), out.end(),
                [&dist = dist, &engine = engine]() { return dist(engine); });

  return out;
}

/*** R
results_construct <-
    bench::mark(
      ##construct_rmolib(),
      construct_std(),
      construct_boost(),
      check=FALSE,
      min_iterations=1e2L
    )
summary(results_construct, relative = TRUE)


results_construct_and_sample <- bench::press(
  num = c(1e2L, 1e3L, 1e4L),
  {
    n = num
    bench::mark(
      ##construct_and_sample_rmolib(n),
      construct_and_sample_std(n),
      construct_and_sample_boost(n),
      check=FALSE,
      min_iterations=1e2L
    )
  }
)
summary(results_construct_and_sample, relative = TRUE)
*/
