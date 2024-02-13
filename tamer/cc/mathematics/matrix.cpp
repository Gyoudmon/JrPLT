#include <vector>

#include "../../../digitama/gydm/datum/array.hpp"

#define OVERRIDE override
#include "../../../digitama/gydm/physics/algebra/matrix.hpp"

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

    __ffi__ FlMatrix4x4* make_square_flmatrix_via_vector(int* src2D, size_t order) {
        auto flmtx = new FlMatrix4x4();
        auto v2d = make_vector2d(order, order, 1);
        
        array2d_fill_from_array1d(v2d, order, order, src2D, order, order);

        switch (order) {
        case 1: flmtx->fill(FxSquareMatrix<1>(v2d, order, order)); break;
        case 2: flmtx->fill(FxSquareMatrix<2>(v2d, order, order)); break;
        case 3: flmtx->fill(FxSquareMatrix<3>(v2d, order, order)); break;
        default: flmtx->fill(FxMatrix4x4(v2d, order, order));
        }

        return flmtx;
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
    __ffi__ size_t matrix_shape(MatrixTop* self, size_t *row, size_t* col) {
        (*row) = self->row_size();
        (*col) = self->column_size();

        return (*row) * (*col);
    }

    __ffi__ size_t fxmatrix_data(FxMatrix4x4* self, int* dest, size_t size) {
        return self->extract(dest, size);
    }

    __ffi__ size_t flmatrix_data(FlMatrix4x4* self, float* dest, size_t size) {
        return self->extract(dest, size);
    }

    __ffi__ size_t fxmatrix_data2d_row_by_row(FxMatrix4x4* self, float* dest2d, size_t order) {
        size_t stride = self->column_size();
        size_t total = 0;
        float* dest0 = dest2d;
        
        for (size_t r = 0; r < self->row_size(); r ++) {
            total += self->extract_row(r, dest0, stride);
            dest0 += stride;
        }

        return total;
    }
    
    __ffi__ size_t flmatrix_data2d_via_vector(FxMatrix4x4* self, float* dest2D, size_t order) {
        auto v2d = make_vector2d(order, order, 0.0F);

        self->extract(v2d, order, order);

        return array1d_fill_from_array2d(dest2D, order, order, v2d, order, order);
    }

    __ffi__ int fxmatrix_trace(int* src, size_t order) {
        size_t nn = order * order;

        switch (order) {
        case 1: { FxSquareMatrix<1> m1x1(src, nn); return m1x1.trace(); }
        case 2: { FxSquareMatrix<2> m2x2(src, nn); return m2x2.trace(); }
        case 3: { FxSquareMatrix<3> m3x3(src, nn); return m3x3.trace(); }
        case 4: { FxSquareMatrix<4> m4x4(src, nn); return m4x4.trace(); }
        case 5: { FxSquareMatrix<5> m5x5(src, nn); return m5x5.trace(); }
        default: return -1;
        }
    }

    __ffi__ int fxmatrix_determinant(int* src, size_t order) {
        size_t nn = order * order;

        switch (order) {
        case 1: { FxSquareMatrix<1> m1x1(src, nn); return m1x1.determinant(); }
        case 2: { FxSquareMatrix<2> m2x2(src, nn); return m2x2.determinant(); }
        case 3: { FxSquareMatrix<3> m3x3(src, nn); return m3x3.determinant(); }
        case 4: { FxSquareMatrix<4> m4x4(src, nn); return m4x4.determinant(); }
        case 5: { FxSquareMatrix<5> m5x5(src, nn); return m5x5.determinant(); }
        default: return -1;
        }
    }

    __ffi__ float flmatrix_trace(float* src, size_t order) {
        size_t nn = order * order;

        switch (order) {
        case 1: { FlSquareMatrix<1> m1x1(src, nn); return m1x1.trace(); }
        case 2: { FlSquareMatrix<2> m2x2(src, nn); return m2x2.trace(); }
        case 3: { FlSquareMatrix<3> m3x3(src, nn); return m3x3.trace(); }
        case 4: { FlSquareMatrix<4> m4x4(src, nn); return m4x4.trace(); }
        case 5: { FlSquareMatrix<5> m5x5(src, nn); return m5x5.trace(); }
        default: return -1;
        }
    }

    __ffi__ float flmatrix_determinant(float* src, size_t order) {
        size_t nn = order * order;

        switch (order) {
        case 1: { FlSquareMatrix<1> m1x1(src, nn); return m1x1.determinant(); }
        case 2: { FlSquareMatrix<2> m2x2(src, nn); return m2x2.determinant(); }
        case 3: { FlSquareMatrix<3> m3x3(src, nn); return m3x3.determinant(); }
        case 4: { FlSquareMatrix<4> m4x4(src, nn); return m4x4.determinant(); }
        case 5: { FlSquareMatrix<5> m5x5(src, nn); return m5x5.determinant(); }
        default: return -1;
        }
    }

    /*********************************************************************************************/
    __ffi__ bool matrix_is_fixnum(MatrixTop* self) {
        return self->is_fixnum_matrix();
    }

    __ffi__ bool matrix_is_flonum(MatrixTop* self) {
        return self->is_flonum_matrix();
    }

    __ffi__ bool matrix_is_zero(MatrixTop* self) {
        return self->is_zero_matrix();
    }

    __ffi__ bool matrix_is_diagonal(MatrixTop* self) {
        return self->is_diagonal_matrix();
    }

    __ffi__ bool matrix_is_scalar(MatrixTop* self) {
        return self->is_scalar_matrix();
    }

    __ffi__ bool matrix_is_identity(MatrixTop* self) {
        return self->is_identity_matrix();
    }

    __ffi__ bool matrix_is_lower_triangular(MatrixTop* self) {
        return self->is_lower_triangular_matrix();
    }

    __ffi__ bool matrix_is_upper_triangular(MatrixTop* self) {
        return self->is_upper_triangular_matrix();
    }

    __ffi__ bool matrix_is_symmetric(MatrixTop* self) {
        return self->is_symmetric_matrix();
    }

    __ffi__ bool matrix_is_skew_symmetric(MatrixTop* self) {
        return self->is_skew_symmetric_matrix();
    }

    __ffi__ bool fxmatrix_equal(FxMatrix4x4* m1, FxMatrix4x4* m2) {
        return m1->operator==(*m2);
    }

    __ffi__ bool flmatrix_equal(FlMatrix4x4* m1, FlMatrix4x4* m2) {
        return m1->operator==(*m2);
    }

    /*********************************************************************************************/
    __ffi__ const char* matrix_desc(MatrixTop* self) {
        static std::string desc;
        
        desc = self->desc(true);

        return desc.c_str();
    }

    __ffi__ void destroy_matrix(MatrixTop* self) {
        delete self;
    }
}
