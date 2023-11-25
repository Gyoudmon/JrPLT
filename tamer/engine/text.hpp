#pragma once

#include "../../digitama/gydm_stem/game.hpp"
#include "../../digitama/gydm_stem/bang.hpp"

#include <vector>

/*************************************************************************************************/
namespace WarGrey::STEM {
    class TextPlane : public TheBigBang {
    public:
        TextPlane() : TheBigBang("Text Metrics") { }

    public:  // 覆盖游戏基本方法
        void construct(float width, float height) override;
        void load(float width, float height) override;
        void reflow(float width, float height) override;

    public:
        bool can_select(IMatter* m) override;

    protected:
        void on_mission_start(float width, float height) override;
        void after_select(IMatter *m, bool yes) override;

    private:
        void move_texts_at_random();
        
    private:
        std::vector<Labellet*> texts;
        std::vector<Dimensionlet*> metrics;

    private:
        DimensionStyle style;
    };
}
