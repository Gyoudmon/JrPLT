#pragma once

#include "art.hpp"

namespace WarGrey::PLT {
    class __lambda__ CharRegularTriangle : virtual public WarGrey::PLT::ASCIIArt {
    public:
        CharRegularTriangle(int term_row, int term_col);
        virtual ~CharRegularTriangle() noexcept {};
        
    public:
        std::string model_desc() const noexcept override;
        std::string line_desc(int l) const noexcept override;
        std::string shape_line(int l) const noexcept override;

    public:
        int pos(int l) const noexcept override;
        int span(int l) const noexcept override;
    };

    class __lambda__ CharRightTriangle : virtual public WarGrey::PLT::ASCIIArt {
    public:
        CharRightTriangle(int term_row, int term_col);
        virtual ~CharRightTriangle() noexcept {};
        
    public:
        std::string model_desc() const noexcept override;
        std::string line_desc(int l) const noexcept override;
        std::string shape_line(int l) const noexcept override;

    public:
        int pos(int l) const noexcept override;
        int span(int l) const noexcept override;
    };
}
