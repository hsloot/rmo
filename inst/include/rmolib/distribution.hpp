#pragma once

#include <type_traits>
#include <vector>

#include "random/deterministic_distribution.hpp"
#include "random/esm_mo_distribution.hpp"
#include "random/arnold_mo_distribution.hpp"
#include "random/exponential_distribution.hpp"
#include "random/pareto_distribution.hpp"
#include "random/r_discrete_distribution.hpp"
#include "random/uniform_int_distribution.hpp"
#include "random/uniform_real_distribution.hpp"

namespace rmolib {

// -----------------------------------------------------------------------------
// public interfaces to distributions
// -----------------------------------------------------------------------------

template <typename _RealType>
using uniform_real_distribution = random::uniform_real_distribution<_RealType>;

template <typename _IntType>
using uniform_int_distribution = random::uniform_int_distribution<_IntType>;

template <typename _RealType>
using exponential_distribution = random::exponential_distribution<_RealType>;

template <typename _RealType>
using pareto_distribution =
    random::pareto_distribution<_RealType,
                                random::uniform_real_distribution<_RealType>>;

template <typename _RealType>
using deterministic_distribution =
    random::deterministic_distribution<_RealType>;

template <typename _IntType, typename _WeightType = double>
using r_discrete_distribution =
    random::r_discrete_distribution<_IntType, _WeightType,
                                    uniform_real_distribution<_WeightType>>;

template <typename _Container>
using esm_mo_distribution = random::esm_mo_distribution<
    _Container, exponential_distribution<typename _Container::value_type>>;

template <typename _Container>
using arnold_mo_distribution = random::arnold_mo_distribution<
    _Container, exponential_distribution<typename _Container::value_type>, r_discrete_distribution<typename _Container::size_type, typename _Container::value_type>>;

// -----------------------------------------------------------------------------
// public interfaces to alternative distributions sampling methods à la abseil
// -----------------------------------------------------------------------------

template <typename _ValueType, typename _EngineType>
typename std::enable_if<std::is_floating_point<_ValueType>::value,
                        _ValueType>::type
Uniform(_EngineType& engine, const _ValueType lower, const _ValueType upper) {
  uniform_real_distribution<_ValueType> dist{lower, upper};
  return dist(engine);
}

template <typename _ValueType, typename _EngineType>
typename std::enable_if<std::is_integral<_ValueType>::value, _ValueType>::type
Uniform(_EngineType& engine, const _ValueType lower, const _ValueType upper) {
  uniform_int_distribution<_ValueType> dist{lower, upper};
  return dist(engine);
}

template <typename _RealType, typename _EngineType>
_RealType Exponential(_EngineType& engine, const _RealType lambda) {
  exponential_distribution<_RealType> dist{lambda};
  return dist(engine);
}

template <typename _RealType, typename _EngineType>
_RealType Pareto(_EngineType& engine, const _RealType alpha,
                 const _RealType lower_bound) {
  pareto_distribution<_RealType> dist{alpha, lower_bound};
  return dist(engine);
}

template <typename _RealType, typename _EngineType>
_RealType Deterministic(_EngineType& engine, const _RealType value) {
  deterministic_distribution<_RealType> dist{value};
  return dist(engine);
}

template <typename _IntType, typename _EngineType, typename _InputIterator,
          typename _WeightType = double>
_IntType RDiscrete(_EngineType& engine, _InputIterator first,
                   _InputIterator last) {
  r_discrete_distribution<_IntType, _WeightType> dist{first, last};
  return dist(engine);
}

template <typename _Container, typename _EngineType>
_Container ExogenousShockModel(_EngineType& engine, const typename _Container::size_type dim,
                 const _Container intensities) {
  esm_mo_distribution<_Container> dist{dim, intensities};
  return dist(engine);
}

template <typename _Container, typename _EngineType>
_Container ArnoldModel(_EngineType& engine, const typename _Container::size_type dim,
                 const _Container intensities) {
  arnold_mo_distribution<_Container> dist{dim, intensities};
  return dist(engine);
}

}  // namespace rmolib
