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

template <typename _Container = std::vector<double>>
using esm_mo_distribution = random::esm_mo_distribution<
    _Container, exponential_distribution<typename _Container::value_type>>;

template <typename _Container = std::vector<double>>
using arnold_mo_distribution = random::arnold_mo_distribution<
    _Container, exponential_distribution<typename _Container::value_type>,
    r_discrete_distribution<typename _Container::size_type,
                            typename _Container::value_type>>;

template <typename _Container = std::vector<double>>
using markovian_exmo_distribution = random::markovian_exmo_distribution<
    _Container, exponential_distribution<typename _Container::value_type>,
    uniform_int_distribution<typename _Container::size_type>,
    r_discrete_distribution<typename _Container::size_type,
                            typename _Container::value_type>>;

template <typename _Container = std::vector<double>>
using cuadras_auge_distribution = random::cuadras_auge_distribution<
    _Container, exponential_distribution<
                    std::remove_reference_t<typename _Container::value_type>>>;

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

template <typename _EngineType, typename _InputIterator,
          typename _OutputContainer>
void ExogenousShockModel(_EngineType& engine,
                         const typename _OutputContainer::size_type dim,
                         _InputIterator first, _InputIterator last,
                         _OutputContainer& out) {
  using value_type =
      std::remove_reference_t<typename _InputIterator::value_type>;
  using distribution_type = esm_mo_distribution<std::vector<value_type>>;
  using param_type = typename distribution_type::param_type;

  distribution_type dist{};
  dist(engine, param_type{dim, first, last}, out);
}

template <typename _EngineType, typename _InputIterator,
          typename _OutputContainer>
void ArnoldModel(_EngineType& engine,
                 const typename _OutputContainer::size_type dim,
                 _InputIterator first, _InputIterator last,
                 _OutputContainer& out) {
  using value_type =
      std::remove_reference_t<typename _InputIterator::value_type>;
  using distribution_type = arnold_mo_distribution<std::vector<value_type>>;
  using param_type = typename distribution_type::param_type;

  distribution_type dist{};
  dist(engine, param_type{dim, first, last}, out);
}

template <typename _EngineType, typename _InputIterator,
          typename _OutputContainer>
void MarkovianModel(_EngineType& engine,
                 const typename _OutputContainer::size_type dim,
                 _InputIterator first, _InputIterator last,
                 _OutputContainer& out) {
  using value_type =
      std::remove_reference_t<typename _InputIterator::value_type>;
  using distribution_type = markovian_exmo_distribution<std::vector<value_type>>;
  using param_type = typename distribution_type::param_type;

  distribution_type dist{};
  dist(engine, param_type{dim, first, last}, out);
}

template <typename _EngineType, typename _RealType, typename _OutputContainer>
void CuadrasAuge(_EngineType& engine, const typename std::size_t dim,
                 const _RealType alpha, const _RealType beta,
                 _OutputContainer& out) {
  using value_type = std::remove_reference_t<_RealType>;
  using distribution_type = cuadras_auge_distribution<std::vector<value_type>>;
  using param_type = typename distribution_type::param_type;

  distribution_type dist{};
  dist(engine, param_type{dim, alpha, beta}, out);
}

}  // namespace rmolib
