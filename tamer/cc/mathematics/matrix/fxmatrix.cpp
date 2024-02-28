#include <vector>

#include "../../../../digitama/gydm/datum/array.hpp"
#include "../../../../digitama/gydm/physics/algebra/matrix.hpp"

using namespace GYDM;

/*************************************************************************************************/
namespace GYDM {
    template<typename T>
    std::vector<std::vector<T>> make_vector2d(size_t R, size_t C, T datum) {
        std::vector<std::vector<T>> v2d;

        for (size_t idx = 0; idx < R; idx ++) {
            v2d.push_back(std::vector<T>(C, datum));
        }

        return v2d;
    }
}

extern "C" {
    __ffi__ FxMatrix4x4* make_null_square_fxmatrix() {
        return new FxMatrix4x4();
    }

    __ffi__ FxMatrix4x4* make_square_fxmatrix(int* src, size_t N) {
        return new FxMatrix4x4(src, N);
    }

    __ffi__ FxMatrix4x4* make_square_fxmatrix_via_vector(int* src2d, size_t order) {
        auto fxm = new FxMatrix4x4();
        auto v2d = make_vector2d(order, order, 1);
        
        array2d_fill_from_array1d(v2d, order, order, src2d, order, order);

        switch (order) {
        case 1:  fxm->fill(FxSquareMatrix<1>(v2d, order, order)); break;
        case 2:  fxm->fill(FxSquareMatrix<2>(v2d, order, order)); break;
        case 3:  fxm->fill(FxSquareMatrix<3>(v2d, order, order)); break;
        default: fxm->fill(FxMatrix4x4(v2d, order, order));
        }

        return fxm;
    }

    __ffi__ FxMatrix3x4* make_rectangular_fxmatrix(int* src2D, size_t R, size_t C) {
        return new FxMatrix<3, 4>(src2D, R * C);
    }

    __ffi__ FxMatrix4x4* make_diagonal_fxmatrix(int scalar) {
        return new FxMatrix4x4(scalar);
    }

    __ffi__ FxMatrix4x4* make_square_fxmatrix_with_diagonal(int* src, size_t N) {
        auto mtx = new FxMatrix4x4();
        
        mtx->fill_diagonal(src, N);

        return mtx;
    }

    /*********************************************************************************************/
    __ffi__ size_t fxmatrix_data_row_by_row(FxMatrix4x4* self, int* dest, size_t order) {
        size_t stride = self->column_size();
        size_t total = 0;
        int* dest0 = dest;
        
        for (size_t r = 0; r < self->row_size(); r ++) {
            total += self->extract_row(r, dest0, stride);
            dest0 += stride;
        }

        return total;
    }
    
    __ffi__ size_t fxmatrix_data2d_via_vector(FxMatrix4x4* self, int* dest2d, size_t order) {
        auto v2d = make_vector2d(order, order, 0.0F);

        self->extract(v2d, order, order);

        return array1d_fill_from_array2d(dest2d, order, order, v2d, order, order);
    }

    __ffi__ size_t fxmatrix_rectangular_data2d(FxMatrix3x4* self, int* dest2d, size_t R, size_t C) {
        auto shadow = self->diagonal();

        self->lower_triangle(&shadow);
        self->upper_triangle(&shadow);

        return shadow.extract(dest2d, R * C);
    }

    /*********************************************************************************************/
    __ffi__ FxMatrix3x4* fxmatrix_add_subtract(FxMatrix3x4* lhs, FxMatrix3x4* rhs, bool forward) {
        FxMatrix3x4 self(lhs);
        
        if (forward) {
            self += (*rhs);
        } else {
            self -= (*rhs);
        }

        return new FxMatrix3x4(self);
    }

    __ffi__ FxMatrix3x4* fxmatrix_scale(FxMatrix3x4* lhs, int rhs, bool forward) {
        FxMatrix3x4 self(lhs);

        if (forward) {
            self *= rhs;
        } else {
            self /= rhs;
        }

        return new FxMatrix3x4(self);
    }

    __ffi__ FxMatrix3x4* fxmatrix_multiply(int* lhs2D, int* rhs2D, size_t M, size_t N, size_t P) {
        FxMatrix<3, 2> lhs(lhs2D, M * N);
        FxMatrix<2, 4> rhs(rhs2D, N * P);

        return new FxMatrix3x4(lhs * rhs);
    }

    /*********************************************************************************************/
    __ffi__ bool fxmatrix_equal(FxMatrix4x4* m1, FxMatrix4x4* m2) {
        return m1->operator==(*m2);
    }
}
