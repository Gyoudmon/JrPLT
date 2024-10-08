#pragma once

#include <plteen/game.hpp>
#include <plteen/bang.hpp>

namespace WarGrey::PLT {
    class __lambda__ ThePLTPlane : public Plteen::TheBigBang {
    public:
        ThePLTPlane(const char* name, uint32_t title_color = 0U);
        virtual ~ThePLTPlane() {}

    protected:
        bool update_atlas_position_for_tooltip(Plteen::IMatter* m, float x, float y);
    };
}
