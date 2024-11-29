#pragma once

#include "tamer.hpp"

#include <vector>

/*************************************************************************************************/
namespace Plteen {
    class LayerPlane : public Plteen::TheTamerBang {
    public:
        LayerPlane() : TheBigBang("Layer Order") {}

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override;
        
    public:
        bool can_select(IMatter* m) override;

    protected:
        void on_mission_start(float width, float height) override;
        void after_select(IMatter* m, bool yes) override;
        void on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) override;

    private:
        void move_shapes_at_random();
        
    private:
        std::vector<RegularPolygonlet*> shapes;
    };
}
