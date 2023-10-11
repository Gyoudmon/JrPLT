#include "../../digitama/gydm_stem/game.hpp"
#include "../../digitama/gydm_stem/bang.hpp"

#include <vector>

/*************************************************************************************************/
namespace WarGrey::STEM {
    class LayerPlane : public TheBigBang {
    public:
        LayerPlane() : TheBigBang("Layer Order") { this->the_name("Tamer"); }

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override;

    public:
        void on_enter(IPlane* from) override;

    public:
        bool can_select(IMatter* m) override;

    protected:
        void after_select(IMatter* m, bool yes) override;
        void on_tap_selected(IMatter* m, float x, float y) override;
        void on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) override;

    private:
        void move_shapes_at_random();
        
    private:
        std::vector<RegularPolygonlet*> shapes;
    };
}
