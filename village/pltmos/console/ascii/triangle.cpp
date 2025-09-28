#include "triangle.hpp"

#include <plteen/datum/string.hpp>

using namespace Plteen;
using namespace WarGrey::PLT;

/*************************************************************************************************/
WarGrey::PLT::CharRegularTriangle::CharRegularTriangle(int term_row, int term_col)
    : ASCIIArt(term_row - 1, term_col) {}

int WarGrey::PLT::CharRegularTriangle::pos(int l) const noexcept {
    return this->row - l + 1;
}

int WarGrey::PLT::CharRegularTriangle::span(int l) const noexcept {
    return 2 * l - 1;
}

/*************************************************************************************************/
std::string WarGrey::PLT::CharRegularTriangle::model_desc() const noexcept {
    return "pos(L) = N - L + 1;\nspan(L) = 2L - 1;";
}

std::string WarGrey::PLT::CharRegularTriangle::line_desc(int l) const noexcept {
    return make_nstring("  pos(%d) = %d - %d + 1 = %d;\nspan(%d) = 2 * %d - 1 = %d;",
                        l, this->row, l, this->pos(l), l, l, this->span(l));
}

std::string WarGrey::PLT::CharRegularTriangle::shape_line(int l) const noexcept {
    return std::string(this->pos(l) - 1, ' ') + std::string(this->span(l), '^');
}

/*************************************************************************************************/
WarGrey::PLT::CharRightTriangle::CharRightTriangle(int term_row, int term_col)
    : ASCIIArt(term_row - 1, term_col) {}

int WarGrey::PLT::CharRightTriangle::pos(int l) const noexcept {
    return 1;
}

int WarGrey::PLT::CharRightTriangle::span(int l) const noexcept {
    return l;
}

/*************************************************************************************************/
std::string WarGrey::PLT::CharRightTriangle::model_desc() const noexcept {
    return "pos(L) = 1;\nspan(L) = L;";
}

std::string WarGrey::PLT::CharRightTriangle::line_desc(int l) const noexcept {
    return make_nstring("  pos(%d) = %d;\nspan(%d) = %d;", l, this->pos(l), l, this->span(l));
}

std::string WarGrey::PLT::CharRightTriangle::shape_line(int l) const noexcept {
    return std::string(this->span(l), 'L');
}
