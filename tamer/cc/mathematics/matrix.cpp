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

    template<size_t N>
    void matrix_overflow_determinants(int* src, size_t nn, int* fx, float* fl) {
        (*fx) = SquareMatrix<N, int, int>(src, nn).determinant();
        (*fl) = SquareMatrix<N, float, float>(src, nn).determinant();
    }
}

extern "C" {
    __ffi__ FxMatrix4x4* make_null_square_fxmatrix() {
        return new FxMatrix4x4();
    }

    __ffi__ FxMatrix4x4* make_square_fxmatrix(int* src, size_t N) {
        return new FxMatrix4x4(src, N);
    }

    __ffi__ FxMatrix<4, 3>* make_rectangular_fxmatrix(int* src2D, size_t R, size_t C) {
        return new FxMatrix<4, 3>(src2D, R * C);
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

    __ffi__ size_t fxmatrix_data2d_via_triangle(FxMatrix4x3* self, float* dest2d, size_t R, size_t C) {
        auto shadow = self->diagonal();

        self->lower_triangle(&shadow);
        self->upper_triangle(&shadow);

        return shadow.extract(dest2d, R * C);
    }
    
    __ffi__ size_t flmatrix_data2d_via_vector(FlMatrix4x4* self, float* dest2D, size_t order) {
        auto v2d = make_vector2d(order, order, 0.0F);

        self->extract(v2d, order, order);

        return array1d_fill_from_array2d(dest2D, order, order, v2d, order, order);
    }

    /*********************************************************************************************/
    __ffi__ FxMatrix4x3* matrix_add_subtract(FxMatrix4x3* lhs, FxMatrix4x3* rhs, bool forward) {
        FxMatrix4x3 self(lhs);

        if (forward) {
            self += (*rhs);
        } else {
            self -= (*rhs);
        }

        return new FxMatrix4x3(self);
    }

    __ffi__ FlMatrix4x3* matrix_scale(FxMatrix4x3* lhs, float rhs, bool forward) {
        FlMatrix4x3 self(lhs);

        if (forward) {
            self *= rhs;
        } else {
            self /= rhs;
        }

        return new FlMatrix4x3(self);
    }

    __ffi__ bool matrix_trace(int* src, size_t order, long long* fx_tr, double* fl_tr, double* dl_tr) {
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

    __ffi__ bool matrix_determinant(int* src, size_t order, long long* fx_det, double* fl_det, double* dl_det) {
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

    __ffi__ bool matrix_overflow_determinant(int* src, size_t order, int* fx_det, float* fl_det) {
        size_t nn = order * order;

        switch (order) {
        case 5: matrix_overflow_determinants<5>(src, nn, fx_det, fl_det); break;
        case 6: matrix_overflow_determinants<6>(src, nn, fx_det, fl_det); break;
        default: return false;
        }

        return true;
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

    __ffi__ bool matrix_is_square(MatrixTop* self) {
        return self->is_square_matrix();
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
