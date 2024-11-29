#pragma once

#include <plteen/game.hpp>
#include <plteen/bang.hpp>

namespace WarGrey::STEM {
    class __lambda__ TheSTEMPlane : public virtual Plteen::TheBigBang {
    protected:
        const char* the_title_prefix() const override {
            // Επιστήμη(Science) in greek
            return "[Σ]交互式科学";
        }
    };
}
