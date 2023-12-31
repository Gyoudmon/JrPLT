#pragma once

#include <gydm/game.hpp>
#include <gydm/bang.hpp>

namespace WarGrey::STEM {
    class __lambda__ TheSTEMPlane : public GYDM::TheBigBang {
    public:
        TheSTEMPlane(const char* name, uint32_t title_color = 0U);
        virtual ~TheSTEMPlane() {}
    };
}
