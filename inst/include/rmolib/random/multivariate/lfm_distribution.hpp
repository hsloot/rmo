#pragma once

#include <vector>

#include "rmolib/type_traits/is_safe_numeric_cast.hpp"

namespace rmolib {

namespace random {

namespace internal {

template <typename _T, class = void>
struct __is_lfm_param_type : public std::false_type {};

template <typename _T>
struct __is_lfm_param_type<
    _T,
    std::enable_if_t<
        decltype(std::declval<_T>().dim(), std::true_type())::value&& decltype(
            std::declval<_T>().killing(),
            std::true_type())::value&& decltype(std::declval<_T>().drift(),
                                                std::true_type())::
            value&& decltype(std::declval<_T>().intensity(), std::true_type())::
                value&& decltype(std::declval<_T>().jump_param(),
                                 std::true_type())::value>>
    : public std::true_type {};

}  // namespace internal

template <typename _T>
struct is_lfm_param_type
    : public internal::__is_lfm_param_type<std::remove_cv_t<_T>> {};

template <typename _T>
constexpr bool is_lfm_param_type_v = is_lfm_param_type<_T>::value;

template <typename _RealType, typename _JumpDistribution,
          typename _ExponentialDistribution>
class lfm_distribution {
 private:
  using exponential_parm_t = typename _ExponentialDistribution::param_type;
  using jump_parm_t = typename _JumpDistribution::param_type;
  using compound_poisson_parm_t = std::pair<exponential_parm_t, jump_parm_t>;

 public:
  using result_type = std::vector<_RealType>;

  class param_type {
   public:
    using distribution_type = lfm_distribution;

    param_type() { __validate_input(); };

    param_type(const std::size_t dim, const exponential_parm_t& killing_parm,
               const _RealType drift, const exponential_parm_t& intensity_parm,
               const jump_parm_t& jump_parm)
        : dim_{dim},
          killing_parm_{killing_parm},
          drift_{drift},
          compound_poisson_parm_{std::make_pair(intensity_parm, jump_parm)} {
      __validate_input();
    }

    param_type(const std::size_t dim, const _RealType killing,
               const _RealType drift, const _RealType intensity,
               const jump_parm_t& jump_parm)
        : param_type{dim, exponential_parm_t{killing}, drift,
                     exponential_parm_t{intensity}, jump_parm} {}

    template <
        typename... _Args,
        std::enable_if_t<
            !std::is_convertible_v<
                std::decay_t<std::tuple_element_t<0, std::tuple<_Args...>>>,
                jump_parm_t>,
            int> = 0>
    param_type(const std::size_t dim, const _RealType killing,
               const _RealType drift, const _RealType intensity,
               _Args&&... jump_args)
        : param_type{dim, killing, drift, intensity,
                     jump_parm_t{std::forward<_Args>(jump_args)...}} {}

    // Used for construction from a different specialization
    template <
        typename _LFMParamType,
        std::enable_if_t<!std::is_convertible_v<_LFMParamType, param_type> &&
                             is_lfm_param_type_v<_LFMParamType>,
                         int> = 0>
    explicit param_type(_LFMParamType&& parm)
        : param_type{parm.dim(), parm.killing(), parm.drift(), parm.intensity(),
                     parm.jump_param()} {}

    // compiler generated ctor and assignment op is sufficient

    auto dim() const { return dim_; }

    auto killing() const { return killing_parm_.lambda(); }
    auto drift() const { return drift_; }

    auto intensity() const {
      return std::get<0>(compound_poisson_parm_).lambda();
    }

    auto jump_param() const { return std::get<1>(compound_poisson_parm_); }

    friend class lfm_distribution;

    friend bool operator==(const param_type& lhs, const param_type& rhs) {
      return lhs.dim_ == rhs.dim_ && lhs.killing_parm_ == rhs.killing_parm_ &&
             lhs.drift_ == rhs.drift_ &&
             lhs.compound_poisson_parm_ == rhs.compound_poisson_parm_;
    }

    friend bool operator!=(const param_type& lhs, const param_type& rhs) {
      return !(lhs == rhs);
    }

   private:
    std::size_t dim_{1};
    exponential_parm_t killing_parm_{};
    _RealType drift_{0};
    compound_poisson_parm_t compound_poisson_parm_{};

    void __validate_input() const {
      using jump_dist_t = typename jump_parm_t::distribution_type;

      const auto& [intensity_parm, jump_parm] = compound_poisson_parm_;
      const auto killing = killing_parm_.lambda();
      const auto drift = drift_;
      const auto intensity = intensity_parm.lambda();

      const auto jump_dist = jump_dist_t{jump_parm};
      const auto jump_min = jump_dist.min();
      const auto jump_max = jump_dist.max();

      if (drift < 0) throw std::domain_error("drift must not be negative");

      if (0 == killing && 0 == drift && (0 == intensity || 0 == jump_max))
        throw std::domain_error("Lévy subordinator must not be constant");

      if (jump_min < 0)
        throw std::domain_error("jump values must not be negative");
    }

    static_assert(std::is_floating_point_v<_RealType>,
                  "Class template rmolib::random::lfm_distribution<> must be "
                  "parametrized with floating point type");
  };

  lfm_distribution() = default;

  lfm_distribution(const std::size_t dim,
                   const exponential_parm_t& killing_parm,
                   const _RealType drift,
                   const exponential_parm_t& intensity_parm,
                   const jump_parm_t& jump_parm)
      : param_type{dim, killing_parm, drift, intensity_parm, jump_parm} {}

  lfm_distribution(const std::size_t dim, const _RealType killing,
                   const _RealType drift, const _RealType intensity,
                   const jump_parm_t& jump_parm)
      : param_type{dim, killing, drift, intensity, jump_parm} {}

  template <typename... _Args,
            std::enable_if_t<
                !std::is_convertible_v<
                    std::decay_t<std::tuple_element_t<0, std::tuple<_Args...>>>,
                    jump_parm_t>,
                int> = 0>
  lfm_distribution(const std::size_t dim, const _RealType killing,
                   const _RealType drift, const _RealType intensity,
                   _Args&&... jump_args)
      : param_type{dim, killing, drift, intensity,
                   std::forward<_Args>(jump_args)...} {}

  // Used for construction from a different specialization
  template <typename _LFMParamType,
            std::enable_if_t<
                !std::is_convertible_v<_LFMParamType, lfm_distribution> &&
                    !std::is_convertible_v<_LFMParamType, param_type> &&
                    is_lfm_param_type_v<_LFMParamType>,
                int> = 0>
  explicit lfm_distribution(_LFMParamType&& parm)
      : parm_{std::forward<_LFMParamType>(parm)} {}

  // compiler generated ctor and assignment op is sufficient

  void reset() {}

  auto min() const { return result_type(dim(), _RealType{0}); }
  auto max() const {
    return result_type(dim(), std::numeric_limits<_RealType>::infinity());
  }

  auto dim() const { return parm_.dim(); }
  auto killing() const { return parm_.killing(); }
  auto drift() const { return parm_.drift(); }
  auto intensity() const { return parm_.intensity(); }
  auto jump_param() const { return parm_.jump_param(); }

  param_type param() const { return parm_; }
  void param(const param_type& parm) { parm_ = parm; }

  template <typename _Engine>
  result_type operator()(_Engine&& engine) {
    return (*this)(std::forward<_Engine>(engine), parm_);
  }

  template <typename _Engine>
  result_type operator()(_Engine&& engine, const param_type& parm) {
    result_type out(parm.dim_);
    (*this)(std::forward<_Engine>(engine), parm, out);
    return out;
  }

  template <typename _Engine, typename _Container>
  void operator()(_Engine&& engine, const param_type& parm, _Container& out) {
    const auto dim = out.size();
    const auto sorted_barriers_with_indeces = __iid_barriers(engine, dim);
    const auto killing_time = exponential_dist_(engine, parm.killing_parm_);

    auto state = std::make_pair(_RealType{0}, _RealType{0});
    auto& [time, value] = state;
    auto next_state = state;
    auto& [next_time, next_value] = next_state;
    for (const auto& [index, barrier] : sorted_barriers_with_indeces) {
      while (value < barrier) {
        if (next_value < barrier) {
          state = next_state;
          next_state = __compound_poisson_process(
              engine, parm.compound_poisson_parm_, state);
          if (killing_time <= next_time) {
            next_time = killing_time;
            next_value = std::numeric_limits<_RealType>::infinity();
          } else {
            next_value += (next_time - time) * parm.drift_;
          }
        } else if (value + (next_time - time) * parm.drift_ >= barrier) {
          time += (barrier - value) / parm.drift_;
          value = barrier;
        } else {
          state = next_state;
        }
      }
      out[index] = time;
    }
  }

  friend bool operator==(const lfm_distribution& lhs,
                         const lfm_distribution& rhs) {
    return lhs.parm_ == rhs.parm_;
  }

  friend bool operator!=(const lfm_distribution& lhs,
                         const lfm_distribution& rhs) {
    return !(lhs == rhs);
  }

 private:
  param_type parm_{};

  _ExponentialDistribution exponential_dist_{};
  _JumpDistribution jump_dist_{};

  template <typename _Engine>
  auto __iid_barriers(_Engine&& engine, const std::size_t size) {
    std::vector<std::pair<std::size_t, _RealType>> out(size);
    std::generate(out.begin(), out.end(),
                  [&engine, &dist = exponential_dist_,
                   parm = exponential_parm_t{_RealType{1}},
                   i = std::size_t{0}]() mutable {
                    return std::make_pair(i++, dist(engine, parm));
                  });
    std::sort(out.begin(), out.end(), [](const auto& l, const auto& u) {
      return std::get<1>(l) < std::get<1>(u);
    });

    return out;
  }

  template <typename _Engine>
  auto __compound_poisson_process(_Engine&& engine,
                                  const compound_poisson_parm_t& parm,
                                  std::pair<_RealType, _RealType> state) {
    const auto& [poisson_parm, jump_parm] = parm;
    auto& [time, location] = state;
    time += exponential_dist_(engine, poisson_parm);
    location += jump_dist_(engine, jump_parm);
    return state;
  }

  static_assert(type_traits::is_safe_numeric_cast_v<
                    _RealType, typename _ExponentialDistribution::result_type>,
                "Class template rmolib::random::lfm_distribution<> must be "
                "parametrized with exponential_distribution-type with suitable "
                "result_type");
  static_assert(
      type_traits::is_safe_numeric_cast_v<
          _RealType, typename _JumpDistribution::result_type>,
      "Class template rmolib::random::lfm_distribution<> must be "
      "parametrized with jump distribution-type with suitable result_type");
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, typename _RealType, typename
  _ExponentialDistribution> std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            lfm_distribution<_RealType, _ExponentialDistribution,
  _JumpDistribution>& dist);

  template <class _CharType, class _Traits, typename _RealType, typename
  _ExponentialDistribution> std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             lfm_distribution<_RealType, _ExponentialDistribution,
  _JumpDistribution>& dist);
*/

}  // namespace random

}  // namespace rmolib
