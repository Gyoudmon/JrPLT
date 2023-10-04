#include "../digitama/gydm_stem/game.hpp"
#include "../digitama/gydm_stem/bang.hpp"

#include <vector>

using namespace WarGrey::STEM;

/*************************************************************************************************/
namespace {
    static double gliding_duration = 1.0;

    class TrackPlane : public TheBigBang {
    public:
        TrackPlane(Cosmos *master) : TheBigBang("Track") {
            this->the_name("Tamer");
        }

    public: // 覆盖游戏基本方法
        void load(float width, float height) override {
            TheBigBang::load(width, height);

            this->track = this->insert(new Tracklet(width, height));
            
            this->bracers.push_back(this->insert(new Estelle()));
            this->bracers.push_back(this->insert(new Joshua()));
            this->bracers.push_back(this->insert(new Scherazard()));
            this->bracers.push_back(this->insert(new Olivier()));
            this->bracers.push_back(this->insert(new Agate()));
            this->bracers.push_back(this->insert(new Klose()));
            this->bracers.push_back(this->insert(new Tita()));
            this->bracers.push_back(this->insert(new Zin()));
        }

        void update(uint64_t interval, uint32_t count, uint64_t uptime) override {
            if (is_shift_pressed()) {
                for (auto bracer : this->bracers) {
                    bracer->try_switch_mode(BracerMode::Run);
                }
            } else {
                for (auto bracer : this->bracers) {
                    bracer->try_switch_mode(BracerMode::Walk);
                }
            }
        }

        void on_enter(IPlane *from) override {
            this->agent->play("Greeting", 1);
            this->run_bracers_at_random();
        }

    public:
        bool can_select(IMatter *m) override {
            return isinstance(m, Citizen) || (this->agent == m);
        }

    protected:
        void after_select(IMatter *m, bool yes) override {
            if (!yes) {
                this->glide_to_mouse(gliding_duration, m, MatterAnchor::CC);
            }
        }

        void on_tap_selected(IMatter *m, float x, float y) override {
        }

        void on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) override {
            if (pressed) {
                switch (key) {
                case 'r': this->run_bracers_at_random(); break;
                case '8': this->run_bracers_in_8_ways(); break;
                case ' ': this->run_bracers_in_direction(); break;
                }
            }
        }

    protected:
        bool update_tooltip(IMatter *m, float lx, float ly, float gx, float gy) override {
            bool updated = false;

            if (this->can_select(m)) {
                this->tooltip->set_text("heading: %.02lf˚", m->get_heading(false));
                updated = true;
            }

            return updated;
        }

    private:
        void run_bracers_at_random() {
            for (auto bracer : this->bracers) {
                this->glide_to_random_location(gliding_duration, bracer);
            }
        }

        void run_bracers_in_direction() {
            float length = 128.0F;

            for (auto bracer : this->bracers) {
                this->glide(gliding_duration, bracer, length);
            }
        }

        void run_bracers_in_8_ways() {
            IMatter* selected = this->find_next_selected_matter();
            
            if (selected != nullptr) {
                this->run_bracer_in_8_ways(selected);
            } else {
                for (auto bracer : this->bracers) {
                    this->run_bracer_in_8_ways(bracer);
                }
            }
        }

        void run_bracer_in_8_ways(IMatter* bracer) {
            float length = 128.0F;
            float x, y;

            this->glide(gliding_duration, bracer, +length, 0.0F);
            this->glide(gliding_duration, bracer, 0.0F,    +length);
            this->glide(gliding_duration, bracer, -length, 0.0F);
            this->glide(gliding_duration, bracer, 0.0F,    -length);
            this->glide(gliding_duration, bracer, +length, +length);
            this->glide(gliding_duration, bracer, +length, -length);
            this->glide(gliding_duration, bracer, -length, -length);
            this->glide(gliding_duration, bracer, -length, +length);

            this->track->pen_down();
            this->track->set_pen_color(random_uniform(0.0, 360.0));
            this->feed_matter_location(bracer, &x, &y, MatterAnchor::CC);
            this->track->add_line(x, y, x + random_uniform(0.0F, length), y + random_uniform(0.0F, length));
            this->track->pen_up();
        }

    private:
        std::vector<Bracer*> bracers;
        Tracklet* track;
    };

    /*********************************************************************************************/
    class TrackCosmos : public Cosmos {
    public:
        TrackCosmos(const char *process_path) : Cosmos(60) {
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

        virtual ~TrackCosmos() {
            imgdb_teardown();
        }

    public: // 覆盖游戏基本方法
        void construct(int argc, char *argv[]) override {
            this->set_window_size(1200, 900);
            GameFont::fontsize(21);

            this->push_plane(new TrackPlane(this));
        }
    };
}

/*************************************************************************************************/
int main(int argc, char *args[]) {
    TrackCosmos universe(args[argc]);

    universe.construct(argc, args);
    universe.big_bang();

    return 0;
}
