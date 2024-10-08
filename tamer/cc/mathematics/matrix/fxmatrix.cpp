#include <vector>

#include "../../../../digitama/plteen/datum/array.hpp"
#include "../../../../digitama/plteen/physics/algebra/matrix.hpp"

using namespace Plteen;

/*************************************************************************************************/
namespace Plteen {
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
    __ffi__ Matrix* make_null_square_fxmatrix() {
        return new Matrix(fxmatrix_4x4());
    }

    __ffi__ Matrix* make_square_fxmatrix(int* src, size_t N) {
        return new Matrix(fxmatrix_4x4(src, N));
    }

    __ffi__ Matrix* make_square_fxmatrix_via_vector(int* src2d, size_t order) {
        fxmatrix_4x4 fxm;
        auto v2d = make_vector2d(order, order, 1);
        
        array2d_fill_from_array1d(v2d, order, order, src2d, order, order);

        switch (order) {
        case 1:  fxm.fill(fxsmatrix<1>(v2d, order, order)); break;
        case 2:  fxm.fill(fxmatrix_2x2(v2d, order, order)); break;
        case 3:  fxm.fill(fxmatrix_3x3(v2d, order, order)); break;
        default: fxm.fill(fxmatrix_4x4(v2d, order, order));
        }

        return new Matrix(fxm);
    }

    __ffi__ Matrix* make_rectangular_fxmatrix(int* src2D, size_t R, size_t C) {
        return new Matrix(fxmatrix_3x4(src2D, R * C));
    }

    __ffi__ Matrix* make_diagonal_fxmatrix(int scalar) {
        return new Matrix(fxmatrix_4x4(scalar));
    }

    __ffi__ Matrix* make_square_fxmatrix_with_diagonal(int* src, size_t N) {
        fxmatrix_4x4 mtx;
        
        mtx.fill_diagonal(src, N);

        return new Matrix(mtx);
    }

    /*********************************************************************************************/
    __ffi__ size_t fxmatrix_data_row_by_row(fxmatrix_4x4* self, int* dest, size_t order) {
        size_t stride = self->column_size();
        size_t total = 0;
        int* dest0 = dest;
        
        for (size_t r = 0; r < self->row_size(); r ++) {
            total += self->extract_row(r, dest0, stride);
            dest0 += stride;
        }

        return total;
    }
    
    __ffi__ size_t fxmatrix_data2d_via_vector(Matrix* self, int* dest2d, size_t order) {
        auto v2d = make_vector2d(order, order, 0.0F);

        self->extract(v2d, order, order);

        return array1d_fill_from_array2d(dest2d, order, order, v2d, order, order);
    }

    __ffi__ size_t fxmatrix_rectangular_data2d(fxmatrix_3x4* self, int* dest2d, size_t R, size_t C) {
        auto shadow = self->diagonal();

        self->lower_triangle(&shadow);
        self->upper_triangle(&shadow);

        return shadow.extract(dest2d, R * C);
    }

    /*********************************************************************************************/
    __ffi__ Matrix* fxmatrix_add_subtract(Matrix* lhs, Matrix* rhs, bool forward) {
        fxmatrix_3x4 self;
        fxmatrix_3x4 mrhs;

        lhs->extract(self);
        rhs->extract(mrhs);
        
        if (forward) {
            self += mrhs;
        } else {
            self -= mrhs;
        }

        return new Matrix(self);
    }

    __ffi__ Matrix* fxmatrix_scale(Matrix* lhs, int rhs, bool forward) {
        fxmatrix_3x4 self;

        lhs->extract_diagonal(self);
        // lhs->extract_lower_triangle(self);
        // lhs->extract_upper_triangle(self);

        if (forward) {
            self *= rhs;
        } else {
            self /= rhs;
        }

        return new Matrix(self);
    }

    __ffi__ Matrix* fxmatrix_multiply(int* lhs2D, int* rhs2D, size_t M, size_t N, size_t P) {
        fxmatrix<3, 2> lhs(lhs2D, M * N);
        fxmatrix<2, 4> rhs(rhs2D, N * P);

        return new Matrix(lhs * rhs);
    }

    /*********************************************************************************************/
    __ffi__ bool fxmatrix_equal(Matrix* m1, Matrix* m2) {
        return m1->operator==(*m2);
    }
}
