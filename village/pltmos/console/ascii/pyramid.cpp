#include "pyramid.hpp"

#include <plteen/datum/string.hpp>

using namespace Plteen;
using namespace WarGrey::PLT;

/*************************************************************************************************/
WarGrey::PLT::CharPyramid::CharPyramid(int term_row, int term_col)
    : ASCIIArt(term_row - 1, term_col) {}

int WarGrey::PLT::CharPyramid::pos(int l) const noexcept {
    return this->row - l + 1;
}

int WarGrey::PLT::CharPyramid::span(int l) const noexcept {
    return 2 * l;
}

/*************************************************************************************************/
std::string WarGrey::PLT::CharPyramid::model_desc() const noexcept {
    return "pos(L) = N - L + 1;\nspan(L) = 2L;";
}

std::string WarGrey::PLT::CharPyramid::line_desc(int l) const noexcept {
    return make_nstring("  pos(%d) = %d - %d + 1 = %d;\nspan(%d) = 2 * %d = %d;",
                        l, this->row, l, this->pos(l), l, l, this->span(l));
}

std::string WarGrey::PLT::CharPyramid::shape_line(int l) const noexcept {
    std::string line(this->pos(l) - 1, ' ');
    int span = this->span(l);

    if (span >= 2) {
        std::string body(span, '_');

        body[0] = '/';
        body[span - 1] = '\\';
        line += body;
    }

    return line;
}
