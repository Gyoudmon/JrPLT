#include "../../../../digitama/gydm/physics/algebra/matrix.hpp"

using namespace GYDM;

/*************************************************************************************************/
extern "C" {
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

    __ffi__ bool matrix_is_row_echelon_form(MatrixTop* self) {
        return self->is_row_echelon_form();
    }

    __ffi__ bool matrix_is_row_canonical_form(MatrixTop* self) {
        return self->is_row_canonical_form();
    }

    /*********************************************************************************************/
    __ffi__ size_t matrix_shape(MatrixTop* self, size_t *row, size_t* col) {
        (*row) = self->row_size();
        (*col) = self->column_size();

        return (*row) * (*col);
    }

    __ffi__ const char* matrix_desc(MatrixTop* self, bool one_line) {
        static std::string desc;
        
        desc = self->desc(one_line);

        return desc.c_str();
    }

    __ffi__ void matrix_destroy(MatrixTop* self) {
        delete self;
    }
}
