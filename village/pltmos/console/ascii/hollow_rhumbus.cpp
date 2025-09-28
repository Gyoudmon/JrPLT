#include "hollow_rhumbus.hpp"

#include <plteen/datum/string.hpp>

using namespace Plteen;
using namespace WarGrey::PLT;

/*************************************************************************************************/
WarGrey::PLT::CharHollowRhumbus::CharHollowRhumbus(int term_row, int term_col)
    : ASCIIArt(term_row / 2, term_row / 2) {}

int WarGrey::PLT::CharHollowRhumbus::pos(int l) const noexcept {
    return this->row - l + 1;
}

int WarGrey::PLT::CharHollowRhumbus::span(int l) const noexcept {
    return 2 * l - 1;
}

/*************************************************************************************************/
std::string WarGrey::PLT::CharHollowRhumbus::model_desc() const noexcept {
    return "pos(L) = N - L + 1;\nspan(L) = 2L - 1;";
}

std::string WarGrey::PLT::CharHollowRhumbus::line_desc(int l) const noexcept {
    return make_nstring("  pos(%d) = %d - %d + 1 = %d;\nspan(%d) = 2 * %d - 1 = %d;",
                        l, this->row, l, this->pos(l), l, l, this->span(l));
}

std::string WarGrey::PLT::CharHollowRhumbus::shape_line(int l) const noexcept {
    if (this->span(l) == 1) {
        return make_nstring("%*c", this->pos(l), '*');
    } else {
        return make_nstring("%*c%*c", this->pos(l), '*', this->span(l) - 1, '*');
    }
}
