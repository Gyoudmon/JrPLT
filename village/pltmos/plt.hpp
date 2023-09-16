#pragma once

#include <gydm_stem/game.hpp>
#include <gydm_stem/bang.hpp>

namespace WarGrey::PLT {
    class __lambda__ ThePLTPlane : public WarGrey::STEM::TheBigBang {
    public:
        ThePLTPlane(const char* name, uint32_t title_color = 0U);
        virtual ~ThePLTPlane() {}

    protected:
        bool update_atlas_position_for_tooltip(WarGrey::STEM::IMatter* m, float x, float y);
    };
}
