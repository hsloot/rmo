#pragma once

#include <cmath>
#include <limits>
#include <stdexcept>
#include <type_traits>

namespace rmolib {

namespace random {

namespace internal {

template <typename _T, class = void>
struct __is_uniform_int_param_type : public std::false_type {};

template <typename _T>
struct __is_uniform_int_param_type<
    _T, std::enable_if_t<decltype(
            std::declval<_T>().lower(),
            std::true_type())::value&& decltype(std::declval<_T>().upper(),
                                                std::true_type())::value>>
    : public std::true_type {};

template <typename _IntType, typename _Engine>
_IntType unit_uniform_int_distribution(_Engine&& engine, const _IntType& n) {
    typename std::remove_reference_t<_Engine> engine_t;
    return unit_uniform_int_distribution<_IntType>(
        std::forward<_Engine>(engine), n, engine_t);
}

}  // namespace internal

template <typename _T>
struct is_uniform_int_param_type
    : public internal::__is_uniform_int_param_type<std::remove_cv_t<_T>> {};

//! true, if _T can be used to construct uniform_int_distribution<>::param_type
template <typename _T>
constexpr bool is_uniform_int_param_type_v =
    is_uniform_int_param_type<_T>::value;

template <typename _IntType>
class uniform_int_distribution {
   public:
    using result_type = _IntType;

    class param_type {
       public:
        using distribution_type = uniform_int_distribution;

        param_type() = default;
        explicit param_type(const _IntType lower, const _IntType upper)
            : lower_{lower}, length_{upper - lower} {
            __validate_input(lower_, lower_ + length_);
        }

        // Used for construction from a different specialization
        template <
            typename _UniformIntParamType,
            std::enable_if_t<
                !std::is_convertible_v<_UniformIntParamType, param_type> &&
                    is_uniform_int_param_type_v<_UniformIntParamType>,
                int> = 0>
        explicit param_type(_UniformIntParamType&& parm)
            : param_type{parm.lower(), parm.upper()} {}

        // compiler generated ctor and assignment op is sufficient

        auto lower() const { return lower_; }
        auto upper() const { return lower_ + length_; }

        friend class uniform_int_distribution;

        friend bool operator==(const param_type& lhs, const param_type& rhs) {
            return lhs.lower_ == rhs.lower_ && lhs.length_ == rhs.length_;
        }

        friend bool operator!=(const param_type& lhs, const param_type& rhs) {
            return !(lhs == rhs);
        }

       private:
        _IntType lower_{0};
        _IntType length_{std::numeric_limits<_IntType>::max()};

        void __validate_input(const _IntType lower,
                              const _IntType upper) const {
            if (upper <= lower)
                throw std::domain_error("upper - lower must be positive");
        }

        static_assert(
            std::is_integral_v<_IntType>,
            "Class template rmolib::random::uniform_int_distribution<> must be "
            "parametrized with integral type");
    };

    uniform_int_distribution() = default;

    explicit uniform_int_distribution(const _IntType lower,
                                      const _IntType upper)
        : parm_{lower, upper} {}
    explicit uniform_int_distribution(const param_type& parm) : parm_{parm} {}

    // Used for construction from a different specialization
    template <
        typename _UniformIntParamType,
        std::enable_if_t<
            !std::is_convertible_v<_UniformIntParamType,
                                   uniform_int_distribution> &&
                !std::is_convertible_v<_UniformIntParamType, param_type> &&
                is_uniform_int_param_type_v<_UniformIntParamType>,
            int> = 0>
    explicit uniform_int_distribution(_UniformIntParamType&& parm)
        : parm_{std::forward<_UniformIntParamType>(parm)} {}

    // compiler generated ctor and assignment op is sufficient

    void reset() {}

    auto min() const { return lower(); }
    auto max() const { return upper(); }

    auto lower() const { return parm_.lower(); }
    auto upper() const { return parm_.upper(); }

    param_type param() const { return parm_; }
    void param(const param_type& parm) { parm_ = parm; }

    template <typename _Engine>
    result_type operator()(_Engine&& engine) {
        return (*this)(std::forward<_Engine>(engine), parm_);
    }

    template <typename _Engine>
    result_type operator()(_Engine&& engine, const param_type& parm) {
        return parm.lower_ + internal::unit_uniform_int_distribution<_IntType>(
                                 std::forward<_Engine>(engine), parm.length_);
    }

    friend bool operator==(const uniform_int_distribution& lhs,
                           const uniform_int_distribution& rhs) {
        return lhs.parm_ == rhs.parm_;
    }

    friend bool operator!=(const uniform_int_distribution& lhs,
                           const uniform_int_distribution& rhs) {
        return !(lhs == rhs);
    }

   private:
    param_type parm_{};
};

/*
  // TODO: implement

  template <class _CharType, class _Traits, class _IntType>
  std::basic_ostream<_CharType, _Traits>&
  operator<<(std::basic_ostream<_CharType, _Traits>& os,
            uniform_int_distribution<_IntType>& dist);

  template <class _CharType, class _Traits, class _IntType>
  std::basic_istream<_CharType, _Traits>&
  operator>>(std::basic_istream<_CharType, _Traits>& is,
             uniform_int_distribution<_IntType>& dist);
*/

}  // namespace random

}  // namespace rmolib
