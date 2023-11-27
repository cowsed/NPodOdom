#pragma once

#include "Vec.h"
#include <array>
#include <cstddef>
#include <iostream>
template<size_t n, size_t m>
class Mat {
public:
    Mat() : els() {}
    static Mat<n, m> from_rows(std::array<Vec<m>, n> rows) {
        Mat<n, m> res;
        size_t i = 0;
        for (auto &vec : rows) {
            for (int j = 0; j < m; j++) {
                res.els[i] = vec[j];
                i++;
            }
        }
        return res;
    }

    template<size_t r>
    Mat<n, r> operator*(const Mat<m, r> &other) const {
        Mat<n, r> res;
        for (size_t i = 0; i < n; i++) {
            for (size_t j = 0; j < r; j++) {
                double val = row(i).dot(other.col(j));
                res.index(i, j) = val;
            }
        }
        return res;
    }

    Vec<m> row(size_t i) const {
        std::array<double, m> cels;
        for (int j = 0; j < m; j++) {
            cels[j] = const_index(i, j);
        }
        return Vec<m>{cels};
    }

    Vec<n> col(size_t j) const {
        std::array<double, n> cels;

        for (int i = 0; i < n; i++) {
            cels[i] = const_index(i, j);
        }
        return Vec<n>{cels};
    }
    void zero() {
        for (size_t i = 0; i < n; i++) {
            for (size_t j = 0; j < m; j++) {
                index(i, j) = 0.0;
            }
        }
    }

    double &index(size_t i, size_t j) { return els[i * m + j]; }

    double const_index(size_t i, size_t j) const { return els[i * m + j]; }

private:
    std::array<double, n * m> els;
};
