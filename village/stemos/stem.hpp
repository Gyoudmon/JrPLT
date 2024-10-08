#pragma once

#include <plteen/game.hpp>
#include <plteen/bang.hpp>

namespace WarGrey::STEM {
    class __lambda__ TheSTEMPlane : public Plteen::TheBigBang {
    public:
        TheSTEMPlane(const char* name, uint32_t title_color = 0U);
        virtual ~TheSTEMPlane() {}
    };
}
