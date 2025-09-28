#pragma once

#include "art.hpp"

namespace WarGrey::PLT {
    class __lambda__ CharRectangle : virtual public WarGrey::PLT::ASCIIArt {
    public:
        CharRectangle(int term_row, int term_col);
        virtual ~CharRectangle() noexcept {};
        
    public:
        std::string model_desc() const noexcept override;
        std::string line_desc(int l) const noexcept override;
        std::string shape_line(int l) const noexcept override;

    public:
        int pos(int l) const noexcept override { return 1; }
        int span(int l) const noexcept override { return this->col; }
    };
}
