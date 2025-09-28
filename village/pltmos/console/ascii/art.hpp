#pragma once

#include <string>

namespace WarGrey::PLT {
    class __lambda__ ASCIIArt {
    public:
        ASCIIArt(int r, int c) : row(r), col(c) {} 
        virtual ~ASCIIArt() noexcept {}

    public:
        int height() const noexcept { return this->row; }

    public:
        virtual std::string model_desc() const noexcept = 0;
        virtual std::string line_desc(int l) const noexcept = 0;
        virtual std::string shape_line(int l) const noexcept = 0;

    public:
        virtual int pos(int l) const noexcept = 0;
        virtual int span(int l) const noexcept = 0;

    protected:
        int row;
        int col;
    };
}
