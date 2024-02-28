#include "../../../../digitama/gydm/physics/algebra/matrix.hpp"

using namespace GYDM;

/*************************************************************************************************/
namespace GYDM {
    template<size_t N>
    void matrix_traces(int* src, size_t nn, long long* fx, double* fl, double* dl) {
        (*fx) = SquareMatrix<N, int>(src, nn).trace();
        (*fl) = SquareMatrix<N, float>(src, nn).trace();
        (*dl) = SquareMatrix<N, double>(src, nn).trace();
    }

    template<size_t N>
    void matrix_determinants(int* src, size_t nn, long long* fx, double* fl, double* dl) {
        (*fx) = SquareMatrix<N, int>(src, nn).determinant();
        (*fl) = SquareMatrix<N, float>(src, nn).determinant();
        (*dl) = SquareMatrix<N, double>(src, nn).determinant();
    }
}

extern "C" {
    __ffi__ FlMatrix4x4* make_square_flmatrix(double* src, size_t N) {
        return new FlMatrix4x4(src, N);
    }

    /*********************************************************************************************/
    __ffi__ size_t flmatrix_data(FlMatrix4x4* self, double* dest, size_t size) {
        return self->extract(dest, size);
    }

    __ffi__ FlMatrix4x4* flmatrix_multiply(const FlMatrix4x4* lhs, const FlMatrix4x4* rhs) {
        return new FlMatrix4x4((*lhs) * (*rhs));
    }
    
    /*********************************************************************************************/
    __ffi__ bool flmatrix_trace(int* src, size_t order, long long* fx_tr, double* fl_tr, double* dl_tr) {
        size_t nn = order * order;

        switch (order) {
        case 1: matrix_traces<1>(src, nn, fx_tr, fl_tr, dl_tr); break;
        case 2: matrix_traces<2>(src, nn, fx_tr, fl_tr, dl_tr); break;
        case 3: matrix_traces<3>(src, nn, fx_tr, fl_tr, dl_tr); break;
        case 4: matrix_traces<4>(src, nn, fx_tr, fl_tr, dl_tr); break;
        case 5: matrix_traces<5>(src, nn, fx_tr, fl_tr, dl_tr); break;
        case 6: matrix_traces<6>(src, nn, fx_tr, fl_tr, dl_tr); break;
        default: return false;
        }

        return true;
    }

    __ffi__ bool flmatrix_determinant(int* src, size_t order, long long* fx_det, double* fl_det, double* dl_det) {
        size_t nn = order * order;

        switch (order) {
        case 1: matrix_determinants<1>(src, nn, fx_det, fl_det, dl_det); break;
        case 2: matrix_determinants<2>(src, nn, fx_det, fl_det, dl_det); break;
        case 3: matrix_determinants<3>(src, nn, fx_det, fl_det, dl_det); break;
        case 4: matrix_determinants<4>(src, nn, fx_det, fl_det, dl_det); break;
        case 5: matrix_determinants<5>(src, nn, fx_det, fl_det, dl_det); break;
        case 6: matrix_determinants<6>(src, nn, fx_det, fl_det, dl_det); break;
        default: return false;
        }

        return true;
    }

    /*********************************************************************************************/
    __ffi__ bool flmatrix_lu_decomposite(double* src, size_t size, MatrixTop** destL, MatrixTop** destU) {
        Matrix<4, 4, double> A(src, size);
        auto L = new Matrix<4, 4, double>();
        auto U = new Matrix<4, 4, double>();
        bool okay = A.LU_decomposite(L, U);

        // let the caller delete L and U even when the decomposition failed

        (*destL) = L;
        (*destU) = U;

        return okay;
    }

    __ffi__ bool flmatrix_lup_decomposite(double* src, size_t size, MatrixTop** destL, MatrixTop** destU, MatrixTop** destP) {
        Matrix<4, 4, double> A(src, size);
        auto L = new Matrix<4, 4, double>();
        auto U = new Matrix<4, 4, double>();
        auto P = new Matrix<4, 4, double>();
        bool okay = A.LUP_decomposite(L, U, P);

        // let the caller delete L and U even when the decomposition failed

        (*destL) = L;
        (*destU) = U;
        (*destP) = P;

        return okay;
    }

    /*********************************************************************************************/
    __ffi__ bool flmatrix_equal(FlMatrix4x4* m1, FlMatrix4x4* m2, double epsilon) {
        if (epsilon <= 0.0) {
            return m1->operator==(*m2);
        } else {
            return m1->flequal(m2, epsilon);
        }
    }
}
