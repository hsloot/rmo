#pragma once

#include <cmath>
#include <limits>
#include <string>
#include <vector>

#include <testthat.h>

template <typename T, typename Compare>
struct CompareMatcher
    : Catch::Matchers::Impl::MatcherBase<std::vector<T>, std::vector<T> > {
  CompareMatcher(const std::vector<T> &comparator, const Compare &compare)
      : m_comparator(comparator), m_compare(compare) {}

  bool match(const std::vector<T> &v) const CATCH_OVERRIDE {
    if (m_comparator.size() != v.size()) {
      return false;
    }
    for (size_t i = 0; i < v.size(); ++i) {
      if (!m_compare(m_comparator[i], v[i])) {
        return false;
      }
    }
    return true;
  }

  virtual std::string describe() const CATCH_OVERRIDE {
    return "Equals: " + Catch::toString(m_comparator);
  }

  const std::vector<T> &m_comparator;
  Compare const &m_compare;
};

template <typename T, typename C>
CompareMatcher<T, C> Compare(const std::vector<T> &comparator,
                             const C &compare) {
  return CompareMatcher<T, C>(comparator, compare);
}

inline auto EqualsApprox(const std::vector<double> &comparator) {
  return Compare(comparator, [](double actual, double expected) {
    return (std::abs(actual) == std::numeric_limits<double>::infinity() &&
            actual == expected) ||
           actual == Approx(expected);
  });
}
