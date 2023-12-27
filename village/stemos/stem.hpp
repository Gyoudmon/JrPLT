#pragma once

#include <gydm/game.hpp>
#include <gydm/bang.hpp>

namespace WarGrey::STEM {
    class __lambda__ TheSCSMPlane : public GYDM::TheBigBang {
    public:
        TheSCSMPlane(const char* name, uint32_t title_color = 0U);
        virtual ~TheSCSMPlane() {}
    };
}
