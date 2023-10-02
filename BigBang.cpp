#include <gydm_stem/game.hpp>

#include "village/pltmos/stream.hpp"
#include "village/scsmos/schematics/optics/pinhole.hpp"

#include <vector>
#include <filesystem>

using namespace WarGrey::STEM;
using namespace WarGrey::PLT;
using namespace WarGrey::SCSM;

/*************************************************************************************************/
namespace {
    static const char* unknown_task_name = "冒险越来越深入了";
    static const char* task_name_fmt = "%02d %s";

    static const int advent_days = 25;

    /*********************************************************************************************/
    class LayerPlane : public Plane {
    public:
        LayerPlane(Cosmos* master) : Plane("青少计算机科学"), master(master) {}

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override {
            this->title = this->insert(new Labellet(GameFont::Title(), BLACK, this->name()));
            
            this->agent = this->insert(new Linkmon());
            this->agent->scale(-1.0F, 1.0F);
            this->set_sentry_sprite(this->agent);
            
            for (int idx = 0; idx < advent_days; idx ++) {
                const char* task_name = this->master->plane_name(idx + 1);
                
                if (task_name == nullptr) {
                    std::string vname = make_nstring(task_name_fmt, idx + 1, unknown_task_name);
            
                    this->coins.push_back(this->insert(new Coinlet(vname, idx + 1)));
                    this->coins.back()->stop();
                } else {
                    std::string vname = make_nstring(task_name_fmt, idx + 1, task_name);

                    this->coins.push_back(this->insert(new Coinlet(vname, idx + 1)));
                }
            }

            this->tux = this->insert(new Tuxmon());
            this->tux->wear("santa_hat");

            this->tooltip = this->insert(make_label_for_tooltip(GameFont::Tooltip()));
            this->set_tooltip_matter(this->tooltip);
        }
        
        void reflow(float width, float height) override {
            this->move_to(this->title, this->agent, MatterAnchor::RB, MatterAnchor::LB);
            
            for (int idx = 0; idx < this->coins.size(); idx ++) {
                if (idx == 0) {
                    this->move_to(this->coins[idx], this->agent, MatterAnchor::LB, MatterAnchor::LT);
                } else {
                    this->move_to(this->coins[idx], this->coins[idx - 1], MatterAnchor::RC, MatterAnchor::LC);
                }
            }

            if (this->coins.size() == 0) {
                this->move_to(this->tux, this->agent, MatterAnchor::LB, MatterAnchor::LT);
            } else {
                this->move_to(this->tux, this->coins[0], MatterAnchor::LB, MatterAnchor::LT);
            }
        }

        void update(uint64_t count, uint32_t interval, uint64_t uptime) override {
            if (this->coins.size() > 0) {
                float tux_lx, tux_rx, stars_rx;

                this->feed_matter_location(this->tux, &tux_rx, nullptr, MatterAnchor::RB);
                this->feed_matter_location(this->coins.back(), &stars_rx, nullptr, MatterAnchor::RB);

                if (tux_rx >= stars_rx) {
                    this->feed_matter_location(this->tux, &tux_lx, nullptr, MatterAnchor::CB);
            
                    if (tux_lx < stars_rx) {
                        this->tux->play("skid");
                    } else {
                        this->move(this->tux, - tux_rx, 0.0F);
                        this->tux->play("walk");
                    }
                }
            }

            if (this->target_plane > 0) {
                if (!this->agent->in_playing()) {
                    this->master->transfer_to_plane(this->target_plane);
                    this->target_plane = 0;
                }
            }
        }

        void on_enter(IPlane* from) override {
            this->agent->play("Greeting", 1);

            this->tux->set_border_strategy(BorderStrategy::IGNORE);
            this->tux->set_velocity(2.0F, 0.0F);            
        }

    public:
        bool can_select(IMatter* m) override {
            return isinstance(m, Coinlet)
                    || (m == this->tux)
                    || (m == this->agent);
        }

        void on_tap(IMatter* m, float x, float y) override {
            auto coin = dynamic_cast<Coinlet*>(m);

            if (m == this->tux) {
                if (this->tux->is_wearing()) {
                    this->tux->take_off();
                } else {
                    this->tux->wear("santa_hat");
                }
            } else if (coin != nullptr) {
                if (coin->in_playing()) {
                    this->target_plane = coin->idx;
                    this->agent->play("Hide", 1);
                }
            }
        }

    protected:
        bool update_tooltip(IMatter* m, float local_x, float local_y, float global_x, float global_y) override {
            auto coin = dynamic_cast<Coinlet*>(m);
            bool updated = false;            
            
            if (coin != nullptr) {
                this->tooltip->set_text(" %s ", coin->name.c_str());
                this->tooltip->set_text_color(coin->in_playing() ? BLACK : GREY);
                updated = true;
            }

            return updated;
        }

    private:
        Linkmon* agent;
        Labellet* title;
        Labellet* tooltip;
        std::vector<Sprite*> coins;
        std::vector<Labellet*> names;
        Tuxmon* tux;
        
    private:
        Cosmos* master;
        int target_plane = 0;
    };

    /*************************************************************************************************/
    enum class CmdlineOps { TopCount, GroupSize, _ };

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
            this->parse_cmdline_options(argc, argv);
            this->set_window_size(1200, 0);
            GameFont::fontsize(21);

            this->splash = this->push_plane(new LayerPlane(this));

            this->push_plane(new StreamPlane());
            this->push_plane(new PinholePlane());
        }

    protected:
        void update(uint64_t count, uint32_t interval, uint64_t uptime) override {
            if (this->has_current_mission_completed()) {
                this->transfer_to_plane(0);
            }
        }

        bool can_exit() override {
            return this->splash->has_mission_completed();
        }

    private:
        void parse_cmdline_options(int argc, char* argv[]) {}

    private:
        IPlane* splash;
    };
}

/*************************************************************************************************/
int main(int argc, char* args[]) {
    LayerCosmos universe(args[argc]);

    universe.construct(argc, args);
    universe.big_bang();

    return 0;
}
