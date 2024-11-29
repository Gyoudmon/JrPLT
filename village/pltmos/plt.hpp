#pragma once

#include <plteen/game.hpp>
#include <plteen/bang.hpp>

namespace WarGrey::PLT {
    class __lambda__ ThePLTPlane : public virtual Plteen::TheBigBang {
    protected:
        const char* the_title_prefix() const override { return "[λ]程序语言理论"; }

    protected:
        bool update_atlas_position_for_tooltip(Plteen::IMatter* m, float x, float y);
    };
}
