#include "rectangle.hpp"

#include <plteen/datum/string.hpp>

using namespace Plteen;
using namespace WarGrey::PLT;

/*************************************************************************************************/
WarGrey::PLT::CharRectangle::CharRectangle(int term_row, int term_col)
    : ASCIIArt(term_row - 1, term_col / 2) {}

/*************************************************************************************************/
std::string WarGrey::PLT::CharRectangle::model_desc() const noexcept {
    return "pos(L) = 1;\nspan(L) = N;";
}

std::string WarGrey::PLT::CharRectangle::line_desc(int l) const noexcept {
    return make_nstring("  pos(%d) = 1;\nspan(%d) = %d;", l, l, this->span(l));
}

std::string WarGrey::PLT::CharRectangle::shape_line(int l) const noexcept {
    return std::string(this->span(l), '#');
}
