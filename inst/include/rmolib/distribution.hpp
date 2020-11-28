#pragma once

#include <type_traits>
#include <vector>

#include "random/multivariate/arnold_mo_distribution.hpp"
#include "random/multivariate/cuadras_auge_distribution.hpp"
#include "random/multivariate/esm_mo_distribution.hpp"
#include "random/multivariate/markovian_exmo_distribution.hpp"
#include "random/univariate/deterministic_distribution.hpp"
#include "random/univariate/exponential_distribution.hpp"
#include "random/univariate/pareto_distribution.hpp"
#include "random/univariate/r_discrete_distribution.hpp"
#include "random/univariate/uniform_int_distribution.hpp"
#include "random/univariate/uniform_real_distribution.hpp"

namespace rmolib {

// -----------------------------------------------------------------------------
// public interfaces to distributions
// -----------------------------------------------------------------------------

template <typename _RealType = double>
using uniform_real_distribution = random::uniform_real_distribution<_RealType>;

template <typename _IntType = std::size_t>
using uniform_int_distribution = random::uniform_int_distribution<_IntType>;

template <typename _RealType = double>
using exponential_distribution = random::exponential_distribution<_RealType>;

template <typename _RealType = double>
using pareto_distribution =
    random::pareto_distribution<_RealType,
                                random::uniform_real_distribution<_RealType>>;

template <typename _RealType = double>
using deterministic_distribution =
    random::deterministic_distribution<_RealType>;

template <typename _IntType = std::size_t, typename _WeightType = double>
using r_discrete_distribution =
    random::r_discrete_distribution<_IntType, _WeightType,
                                    uniform_real_distribution<_WeightType>>;

template <typename _RealType = double>
using esm_mo_distribution =
    random::esm_mo_distribution<_RealType, exponential_distribution<_RealType>>;

template <typename _RealType = double>
using arnold_mo_distribution = random::arnold_mo_distribution<
    _RealType, exponential_distribution<_RealType>,
    r_discrete_distribution<std::size_t, _RealType>>;

template <typename _RealType = double>
using markovian_exmo_distribution = random::markovian_exmo_distribution<
    _RealType, exponential_distribution<_RealType>,
    uniform_int_distribution<std::size_t>,
    r_discrete_distribution<std::size_t, _RealType>>;

template <typename _RealType = double>
using cuadras_auge_distribution =
    random::cuadras_auge_distribution<_RealType,
                                      exponential_distribution<_RealType>>;

// -----------------------------------------------------------------------------
// public interfaces to alternative distributions sampling methods à la abseil
// -----------------------------------------------------------------------------

template <typename _ValueType, typename _EngineType>
typename std::enable_if<std::is_floating_point<_ValueType>::value,
                        _ValueType>::type
Uniform(_EngineType& engine, const _ValueType lower, const _ValueType upper) {
  using distribution_type = uniform_real_distribution<_ValueType>;
  using param_type = typename distribution_type::param_type;

  distribution_type dist{};
  return dist(engine, param_type{lower, upper});
}

template <typename _ValueType, typename _EngineType>
typename std::enable_if<std::is_integral<_ValueType>::value, _ValueType>::type
Uniform(_EngineType& engine, const _ValueType lower, const _ValueType upper) {
  using distribution_type = uniform_int_distribution<_ValueType>;
  using param_type = typename distribution_type::param_type;

  distribution_type dist{};
  return dist(engine, param_type{lower, upper});
}

template <typename _RealType, typename _EngineType>
_RealType Exponential(_EngineType& engine, const _RealType lambda) {
  using distribution_type = exponential_distribution<_RealType>;
  using param_type = typename distribution_type::param_type;

  distribution_type dist{};
  return dist(engine, param_type{lambda});
}

template <typename _RealType, typename _EngineType>
_RealType Pareto(_EngineType& engine, const _RealType alpha,
                 const _RealType lower_bound) {
  using distribution_type = pareto_distribution<_RealType>;
  using param_type = typename distribution_type::param_type;

  distribution_type dist{};
  return dist(engine, param_type{alpha, lower_bound});
}

template <typename _RealType, typename _EngineType>
_RealType Deterministic(_EngineType& engine, const _RealType value) {
  using distribution_type = deterministic_distribution<_RealType>;
  using param_type = typename distribution_type::param_type;

  distribution_type dist{};
  return dist(engine, param_type{value});
}

template <typename _IntType, typename _EngineType, typename _InputIterator>
_IntType RDiscrete(_EngineType& engine, _InputIterator first,
                   _InputIterator last) {
  using weight_type =
      std::remove_reference_t<typename _InputIterator::value_type>;
  using distribution_type = r_discrete_distribution<_IntType, weight_type>;
  using param_type = typename distribution_type::param_type;

  distribution_type dist{};
  return dist(engine, param_type{first, last});
}

template <typename _EngineType, typename _InputIterator>
auto ExogenousShock(_EngineType& engine, const std::size_t dim,
                    _InputIterator first, _InputIterator last) {
  using std::iterator_traits;
  using value_t = std::remove_reference_t<
      typename iterator_traits<_InputIterator>::value_type>;
  using dist_t = esm_mo_distribution<value_t>;
  using parm_t = typename dist_t::param_type;

  dist_t dist{};
  return dist(engine, parm_t{dim, first, last});
}

template <typename _EngineType, typename _InputIterator>
void Arnold(_EngineType& engine, const std::size_t dim, _InputIterator first,
            _InputIterator last) {
  using std::iterator_traits;
  using value_t = std::remove_reference_t<
      typename iterator_traits<_InputIterator>::value_type>;
  using dist_t = arnold_mo_distribution<value_t>;
  using parm_t = typename dist_t::param_type;

  dist_t dist{};
  return dist(engine, parm_t{dim, first, last});
}

template <typename _EngineType, typename _InputIterator>
void Markovian(_EngineType& engine,
                    const std::size_t dim,
                    _InputIterator first, _InputIterator last) {
  using value_t =
      std::remove_reference_t<typename _InputIterator::value_type>;
  using dist_t =
      markovian_exmo_distribution<value_t>;
  using parm_t = typename dist_t::param_type;

  dist_t dist{};
  return dist(engine, parm_t{dim, first, last});
}

template <typename _EngineType, typename _RealType>
void CuadrasAuge(_EngineType& engine, const std::size_t dim,
                 const _RealType alpha, const _RealType beta) {
  using dist_t = cuadras_auge_distribution<_RealType>;
  using parm_t = typename dist_t::param_type;

  dist_t dist{};
  return dist(engine, parm_t{dim, alpha, beta});
}

}  // namespace rmolib
