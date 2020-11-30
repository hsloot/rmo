#include <type_traits>

// clang-format off
#include <Rcpp.h>
#include <r_engine.hpp> // must be included before <rmolib/*>
// clang-format on

#include <rmo.hpp>
#include <rmolib/distribution.hpp>

static const R_xlen_t C_CHECK_USR_INTERRUP = 100000;

using namespace Rcpp;
using namespace mo::stats;

template <typename T>
using is_vector = std::is_same<
    T, std::vector<typename T::value_type, typename T::allocator_type>>;

template <typename T>
constexpr bool is_vector_v = is_vector<T>::value;

template <typename _Distribution, bool sample_dummy = false>
class rcpp_distribution_caller {
  using dist_t = _Distribution;
  using parm_t = typename dist_t::param_type;

  static constexpr bool is_multivariate_v =
      is_vector_v<typename dist_t::result_type>;

  template <typename _T, class = void>
  struct rtype : public std::integral_constant<int, -1> {};  // not implemented

  template <typename _T>
  struct rtype<_T, std::enable_if_t<std::is_same_v<bool, std::remove_cv_t<_T>>>>
      : public std::integral_constant<int, 12> {};  // LGLSXP

  template <typename _T>
  struct rtype<_T,
               std::enable_if_t<std::is_integral_v<_T> &&
                                !std::is_same_v<bool, std::remove_cv_t<_T>>>>
      : public std::integral_constant<int, 13> {};  // INTSXP

  template <typename _T>
  struct rtype<_T, std::enable_if_t<std::is_floating_point_v<_T>>>
      : public std::integral_constant<int, 14> {};  // REALSXP

  template <typename _T>
  static constexpr int rtype_v = rtype<_T>::value;

  using value_type =
      std::conditional_t<is_multivariate_v,
                         typename dist_t::result_type::value_type,
                         typename dist_t::result_type>;

  using result_type =
      std::conditional_t<is_multivariate_v, Matrix<rtype_v<value_type>>,
                         Vector<rtype_v<value_type>>>;

  static constexpr std::size_t __dim(const parm_t& parm) {
    if constexpr (is_multivariate_v) {
      return parm.dim();
    } else {
      return std::size_t{1};
    }
  }

  static constexpr result_type __init(const std::size_t n,
                                      const std::size_t d) {
    if constexpr (is_multivariate_v) {
      return NumericMatrix(no_init(n, d));  // result_type == NumericMatrix
    } else {
      return NumericVector(no_init(n));  // result_type == NumericVector
    }
  }

 public:
  template <typename _Engine, typename... Args>
  static result_type call(_Engine&& engine, const std::size_t n,
                          Args&&... args) {
    using dist_t = _Distribution;
    using parm_t = typename dist_t::param_type;
    auto dist = dist_t{};
    const auto parm = parm_t{std::forward<decltype(args)>(args)...};

    const auto d = __dim(parm);
    auto out = __init(n, d);

    for (auto k = R_xlen_t{0}; k < n; ++k) {
      if constexpr (is_multivariate_v) {
        if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();
        auto values = out(k, _);
        dist(engine, parm, values);
      } else {
        if (k % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();
        out[k] = dist(engine, parm);
      }

      if constexpr (sample_dummy)
        [[maybe_unused]] auto dummy = ::R_unif_index(1.);
    }
    return out;
  }
};

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_esm(const R_xlen_t n, const R_xlen_t d,
                            const NumericVector& intensities) {
  using caller_t =
      rcpp_distribution_caller<rmolib::esm_mo_distribution<double>>;

  return caller_t::call(r_engine{}, n, static_cast<std::size_t>(d),
                        intensities.begin(), intensities.end());
}

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_arnold(const std::size_t n, const std::size_t d,
                               const NumericVector& intensities) {
  using caller_t =
      rcpp_distribution_caller<rmolib::arnold_mo_distribution<double>>;

  return caller_t::call(r_engine{}, n, d, intensities.begin(),
                        intensities.end());
}

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_ex_arnold(const std::size_t n, const std::size_t d,
                                  const NumericVector& ex_intensities) {
  // R's sample.int produces a final (redundant) selection of the
  // last remaining value see
  // https://github.com/wch/r-source/blob/613bdfd0e1d3fc9984142d5da3da448adf2438c7/src/main/random.c#L461
  using caller_t =
      rcpp_distribution_caller<rmolib::markovian_exmo_distribution<double>,
                               true>;

  return caller_t::call(r_engine{}, n, d, ex_intensities.begin(),
                        ex_intensities.end());
}

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_esm_cuadras_auge(const std::size_t n,
                                         const std::size_t d,
                                         const double alpha,
                                         const double beta) {
  using caller_t =
      rcpp_distribution_caller<rmolib::cuadras_auge_distribution<double>>;

  return caller_t::call(r_engine{}, n, d, alpha, beta);
}

std::unique_ptr<RealUnivariateGenerator<double, RRNGPolicy>>
get_univariate_generator(const std::string name, const List& args);

//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix Rcpp__rmo_lfm_cpp(const R_xlen_t n, const R_xlen_t d,
                                const double rate, const double rate_killing,
                                const double rate_drift,
                                const std::string rjump_name,
                                const List& rjump_arg_list) {
  std::unique_ptr<RealUnivariateGenerator<double, RRNGPolicy>> jump_generator =
      get_univariate_generator(rjump_name, rjump_arg_list);
  LFMCPPGenerator<MatrixRow<REALSXP>, RRNGPolicy> lfm_cpp_generator(
      d, rate, rate_killing, rate_drift, jump_generator);

  NumericMatrix out(no_init(n, d));
  for (R_xlen_t k = 0; k < n; k++) {
    if ((d * k) % C_CHECK_USR_INTERRUP == 0) checkUserInterrupt();

    MatrixRow<REALSXP> values = out(k, _);
    lfm_cpp_generator(values);
  }
  return out;
}

//' @keywords internal
//' @noRd
std::unique_ptr<RealUnivariateGenerator<double, RRNGPolicy>>
get_univariate_generator(const std::string name, const List& args) {
  std::unique_ptr<RealUnivariateGenerator<double, RRNGPolicy>> out;
  if ("rexp" == name) {
    double rate = args["rate"];
    out.reset(new ExpGenerator<RRNGPolicy>(rate));
  } else if ("rposval" == name) {
    double value = args["value"];
    out.reset(new FixedDblGenerator<RRNGPolicy>(value));
  } else if ("rpareto" == name) {  // #nocov start
    double alpha = args["alpha"];
    double x0 = args["x0"];
    out.reset(new ParetoGenerator<RRNGPolicy>(alpha, x0));
  } else {                            // # nocov end
    std::logic_error("wrong input");  // # nocov
  }

  return out;
}
