#pragma once

#include "art.hpp"

namespace WarGrey::PLT {
    class __lambda__ CharPyramid : virtual public WarGrey::PLT::ASCIIArt {
    public:
        CharPyramid(int term_row, int term_col);
        virtual ~CharPyramid() noexcept {};
        
    public:
        std::string model_desc() const noexcept override;
        std::string line_desc(int l) const noexcept override;
        std::string shape_line(int l) const noexcept override;

    public:
        int pos(int l) const noexcept override;
        int span(int l) const noexcept override;
    };
}
