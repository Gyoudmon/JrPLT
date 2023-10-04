#include "../digitama/gydm_stem/game.hpp"
#include "../digitama/gydm_stem/bang.hpp"

#include <vector>

using namespace WarGrey::STEM;

/*************************************************************************************************/
namespace {
    static float radius = 80.0F;
    static double gliding_duration = 1.0;

    class LayerPlane : public TheBigBang {
    public:
        LayerPlane(Cosmos* master) : TheBigBang("Layer Order") {
            this->the_name("Tamer");
        }

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override {
            TheBigBang::load(width, height);

            for (int n = 3; n < 13; n++) {
                this->shapes.push_back(this->insert(new RegularPolygonlet(n, radius, -90.0F, random_uniform(0x333333U, 0xDDDDDDU))));
            }
        }

        void on_enter(IPlane* from) override {
            this->agent->play("Greeting", 1);
            this->move_shapes_at_random();
        }

    public:
        bool can_select(IMatter* m) override {
            return isinstance(m, IShapelet) || (this->agent == m);
        }

    protected:
        void after_select(IMatter* m, bool yes) override {
            if (!yes) {
                this->glide_to_mouse(gliding_duration, m, MatterAnchor::CC);
            }
        }

        void on_tap_selected(IMatter* m, float x, float y) override {
            if (is_shift_pressed()) {
                this->bring_forward(m);
            } else {
                this->send_backward(m);
            }
        }

        void on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) override {
            if (pressed) {
                switch (key) {
                case 'f': {
                    IMatter* which = this->find_next_selected_matter();

                    if (which != nullptr) {
                        this->bring_to_front(which);
                    }
                }; break;
                case 'b': {
                    IMatter* which = this->find_next_selected_matter();

                    if (which != nullptr) {
                        this->send_to_back(which);
                    }
                }; break;
                case 'r': this->move_shapes_at_random(); break;
                }
            }
        }

    private:
        void move_shapes_at_random() {
            for (auto shape : this->shapes) {
                this->glide_to_random_location(gliding_duration, shape);
            }
        }
        
    private:
        std::vector<IShapelet*> shapes;
    };

    /*********************************************************************************************/
    class LayerCosmos : public Cosmos {
    public:
        LayerCosmos(const char* process_path) : Cosmos(60) {
            enter_digimon_zone(process_path);
            imgdb_setup(digimon_zonedir().append("stone"));

#ifdef __windows__
            digimon_appdata_setup("C:\\opt\\GYDMstem\\");
            digimon_mascot_setup("C:\\opt\\GYDMstem\\stone\\mascot");
#else
            digimon_appdata_setup("/opt/GYDMstem/");
            digimon_mascot_setup("/opt/GYDMstem/stone/mascot");
#endif
        }

        virtual ~LayerCosmos() {
            imgdb_teardown();
        }

    public:  // 覆盖游戏基本方法
        void construct(int argc, char* argv[]) override {
            this->set_window_size(1200, 900);
            GameFont::fontsize(21);

            this->push_plane(new LayerPlane(this));
        }
    };
}

/*************************************************************************************************/
int main(int argc, char* args[]) {
    LayerCosmos universe(args[argc]);

    universe.construct(argc, args);
    universe.big_bang();

    return 0;
}
