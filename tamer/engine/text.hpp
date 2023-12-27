#pragma once

#include "../../digitama/gydm/bang.hpp"

#include <vector>

/*************************************************************************************************/
namespace GYDM {
    class TextPlane : public GYDM::TheBigBang {
    public:
        TextPlane() : TheBigBang("Text Metrics") { this->the_name("Tamer"); }

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
