#pragma once

#include "../../digitama/plteen/game.hpp"
#include "../../digitama/plteen/bang.hpp"

namespace Plteen {
    class __lambda__ TheTamerBang : public virtual Plteen::TheBigBang {
    protected:
        const char* the_title_prefix() const override { return "Tamer"; }
    };
}
