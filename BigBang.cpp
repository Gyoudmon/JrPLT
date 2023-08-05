#include <gydm_stem/game.hpp>
#include <pltmos/stream.hpp>

#include <vector>
#include <filesystem>

using namespace WarGrey::STEM;

/*************************************************************************************************/
namespace {
#ifndef __windows__
    static const char* unknown_task_name = "冒\n险\n越\n来\n越\n深\n入\n了";
    static const char* task_name_fmt = "%02d\n%s";
#else
    static const char* unknown_task_name = "冒险越来越深入了";
    static const char* task_name_fmt = "%02d %s";
#endif

    static const int advent_days = 25;

    /*********************************************************************************************/
    class TheBigBangPlane : public Plane {
    public:
        TheBigBangPlane(Cosmos* master) : Plane("The Big Bang!"), master(master) {}

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override {
            this->title = this->insert(new Labellet(GameFont::Title(), BLACK, "青少计算机科学：宇宙大爆炸"));
            
            this->agent = this->insert(new Linkmon());
            this->agent->scale(-1.0F, 1.0F);
            
            for (int idx = 0; idx < advent_days; idx ++) {
                const char* task_name = this->master->plane_name(idx + 1);
                
                if (task_name == nullptr) {
                    std::string vname = make_nstring(task_name_fmt, idx + 1, unknown_task_name);
            
#ifndef __windows__
                    this->names.push_back(this->insert(new Labellet(GameFont::fangsong(), GAINSBORO, "%s", vname.c_str())));
                    this->coins.push_back(this->insert(new StarFruitlet(idx + 1)));
#else
                    this->coins.push_back(this->insert(new Coinlet(vname, idx + 1)));
#endif

                    this->coins.back()->stop();
                } else {
#ifndef __windows__
                    std::string vname = make_nstring(task_name_fmt, idx + 1, string_add_between(task_name).c_str());

                    this->names.push_back(this->insert(new Labellet(GameFont::fangsong(), ROYALBLUE, "%s", vname.c_str())));
                    this->stars.push_back(this->insert(new StarFruitlet(idx + 1)));
#else
                    std::string vname = make_nstring(task_name_fmt, idx + 1, task_name);

                    this->coins.push_back(this->insert(new Coinlet(vname, idx + 1)));
#endif
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

#ifndef __windows__
                this->move_to(this->names[idx], this->stars[idx], MatterAnchor::CB, MatterAnchor::CT);
#endif
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
            return true;
        }

        void on_tap(IMatter* m, float x, float y) override {
            if (m == this->tux) {
                if (this->tux->is_wearing()) {
                    this->tux->take_off();
                } else {
                    this->tux->wear("santa_hat");
                }
            }
        }

    protected:
        bool update_tooltip(IMatter* m, float local_x, float local_y, float global_x, float global_y) override {
            bool updated = false;
            
#ifdef __windows__
            auto coin = dynamic_cast<Coinlet*>(m);

            if (coin != nullptr) {
                this->tooltip->set_text(" %s ", coin->name.c_str());
                updated = true;
            }
#endif

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

    class TheBigBangCosmos : public Cosmos {
    public:
        TheBigBangCosmos(const char* process_path) : Cosmos(60) {
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

        virtual ~TheBigBangCosmos() {
            imgdb_teardown();
        }

    public:  // 覆盖游戏基本方法
        void construct(int argc, char* argv[]) override {
            this->parse_cmdline_options(argc, argv);
            this->set_window_size(1200, 0);
            GameFont::fontsize(21);

            this->push_plane(new TheBigBangPlane(this));
        }

    protected:
        void update(uint64_t count, uint32_t interval, uint64_t uptime) override {
            if (this->has_current_mission_completed()) {
                this->transfer_to_plane(0);
            }
        }

    private:
        void parse_cmdline_options(int argc, char* argv[]) {
            CmdlineOps opt = CmdlineOps::_;
            std::string datin;

            // this->set_cmdwin_height(32);

            for (int idx = 1; idx < argc; idx ++) {
                switch (opt) {
                    case CmdlineOps::TopCount: {
                        this->top_count = std::stoi(argv[idx]);
                        opt = CmdlineOps::_;
                    }; break;
                    default: {
                        if (strncmp("--tc", argv[idx], 5) == 0) {
                            opt = CmdlineOps::TopCount;
                        } else {
                            datin = std::string(argv[idx]);
                        }
                    }
                }
            }
        }

    private:
        int top_count = 0;
    };
}

/*************************************************************************************************/
int main(int argc, char* args[]) {
    TheBigBangCosmos universe(args[argc]);

    universe.construct(argc, args);
    universe.big_bang();

    return 0;
}
