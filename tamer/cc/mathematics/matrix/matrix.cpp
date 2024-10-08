#include "../../../../digitama/plteen/physics/algebra/matrix.hpp"

using namespace Plteen;

/*************************************************************************************************/
extern "C" {
    __ffi__ bool matrix_is_fixnum(Matrix* self) {
        return self->is_fixnum_matrix();
    }

    __ffi__ bool matrix_is_flonum(Matrix* self) {
        return self->is_flonum_matrix();
    }

    __ffi__ bool matrix_is_zero(Matrix* self) {
        return self->is_zero_matrix();
    }

    __ffi__ bool matrix_is_square(Matrix* self) {
        return self->is_square_matrix();
    }

    __ffi__ bool matrix_is_diagonal(Matrix* self) {
        return self->is_diagonal_matrix();
    }

    __ffi__ bool matrix_is_scalar(Matrix* self) {
        return self->is_scalar_matrix();
    }

    __ffi__ bool matrix_is_identity(Matrix* self) {
        return self->is_identity_matrix();
    }

    __ffi__ bool matrix_is_lower_triangular(Matrix* self) {
        return self->is_lower_triangular_matrix();
    }

    __ffi__ bool matrix_is_upper_triangular(Matrix* self) {
        return self->is_upper_triangular_matrix();
    }

    __ffi__ bool matrix_is_symmetric(Matrix* self) {
        return self->is_symmetric_matrix();
    }

    __ffi__ bool matrix_is_skew_symmetric(Matrix* self) {
        return self->is_skew_symmetric_matrix();
    }

    __ffi__ bool matrix_is_row_echelon_form(Matrix* self) {
        return self->is_row_echelon_form();
    }

    __ffi__ bool matrix_is_row_canonical_form(Matrix* self) {
        return self->is_row_canonical_form();
    }

    /*********************************************************************************************/
    __ffi__ size_t matrix_shape(Matrix* self, size_t *row, size_t* col) {
        (*row) = self->row_size();
        (*col) = self->column_size();

        return (*row) * (*col);
    }

    __ffi__ const char* matrix_desc(Matrix* self, bool one_line) {
        static std::string desc;
        
        desc = self->desc(one_line);

        return desc.c_str();
    }

    __ffi__ void matrix_destroy(Matrix* self) {
        delete self;
    }
}
