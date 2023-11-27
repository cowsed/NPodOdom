#include "Mat.h"
#include "Vec.h"
#include <iostream>

using namespace std;

constexpr Vec<2> v{1.0, 2.0};
constexpr auto r1 = Vec<2>{0.0, 1.0};
constexpr auto r2 = Vec<2>{1.0, 0.0};

struct NeedPermutationError {};

template<size_t N>
std::pair<Mat<N, N>, Mat<N, N>> LU_Factor(Mat<N, N> m) {
    Mat<N, N> L;
    L.zero();
    for (int j = 0; j < N; j++) {
        double tl_entry = m.const_index(j, j);
        for (int i = j + 1; i < N; i++) {
            double entry = m.const_index(i, j);
            double coef = entry / tl_entry;
            L.index(i, j) = coef;
            m.index(i, j) = 0.0;
        }
    }
    return {L, m};
}

Mat<2, 2> m = Mat<2, 2>::from_rows({r1, r2});

int main() {
    Mat<2, 2> vm = Mat<2, 2>::from_rows({Vec<2>{1.0, 2.0}, Vec<2>{3.0, 4.0}});

    auto res = LU_Factor(vm);
    Mat<2, 2> L = res.first;
    Mat<2, 2> U = res.second;
    //    Mat<2, 1> res = m * vm;
    cout << (L.row(0).to_string()) << endl;
    return 0;
}
