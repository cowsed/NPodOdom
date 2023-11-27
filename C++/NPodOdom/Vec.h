#pragma once

#include <array>
#include <cassert>
#include <cstddef>
#include <string>

template<size_t n>
class Vec {
public:
    constexpr Vec() {
        for (size_t i = 0; i < n; i++) {
            els[i] = 0.0;
        }
    }

    template<typename... Ts>
    constexpr Vec(Ts &&...ts) : els{std::forward<Ts>(ts)...} {}

    constexpr Vec(const std::array<double, n> scalars) : els(scalars) {}
    constexpr Vec operator+(const Vec &b) const {
        Vec r;
        for (size_t i = 0; i < n; i++) {
            r[i] = els[i] + b[i];
        }
        return r;
    }

    constexpr Vec operator-(const Vec &b) const {
        Vec r;
        for (size_t i = 0; i < n; i++) {
            r[i] = els[i] - b[i];
        }
        return r;
    }
    constexpr Vec<n> operator*(double s) {
        Vec r;
        for (size_t i = 0; i < n; i++) {
            r[i] = els[i] * s;
        }
    }

    constexpr double dot(const Vec &b) const {
        double s = 0;
        for (size_t i = 0; i < n; i++) {
            s += els[i] * b.const_index(i);
        }

        return s;
    }

    std::string to_string() const {
        std::string s = "[";
        for (size_t i = 0; i < n; i++) {
            s += std::to_string(els[i]);
            if (i != n - 1) {
                s += " ";
            }
        }
        s += "]";

        return s;
    }
    double const_index(size_t i) const { return els[i]; }
    double &operator[](size_t i) { return els[i]; }
    constexpr const std::array<double, n> &scalars() { return els; }

private:
    std::array<double, n> els;
};

template<size_t n>
constexpr Vec<n> operator*(double s, const Vec<n> &v) {
    return v * s;
}
