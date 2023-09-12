#pragma once

#include <gydm_stem/game.hpp>
#include <gydm_stem/bang.hpp>

namespace WarGrey::SCSM {
    class __lambda__ TheSCSMPlane : public WarGrey::STEM::TheBigBang {
    public:
        TheSCSMPlane(const char* name);
        virtual ~TheSCSMPlane() {}
    };
}
