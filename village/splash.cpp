#include "splash.hpp"

using namespace GYDM;

/*************************************************************************************************/
static const char* unknown_task_name = "冒险越来越深入了";
static const char* task_name_fmt = "%02d %s";

static const int advent_days = 25;

/*************************************************************************************************/
namespace {
    static int xtile_count = 15;
    static int ytile_count = 15;

    class StageAtlas : public PlanetCuteAtlas {
    public:
        StageAtlas(int row, int col) : PlanetCuteAtlas(row, col, GroundBlockType::Stone) {}

        virtual ~StageAtlas() noexcept {}

    public:
        int preferred_local_fps() override { return 4; }

    public:
        void reset() {
            for (int r = 0; r < this->map_row; r ++) {
                for (int c = 0; c < this->map_col; c ++) {
                    if ((r == 0) || (c == 0)
                            || (r == this->map_row - 1)
                            || (c == this->map_col - 1)) {
                        this->set_tile_type(r, c, GroundBlockType::Grass);
                    }
                }
            }
        }

    protected:
        void on_tilemap_load(shared_texture_t atlas) override {
            Margin margin = this->get_map_overlay();
            Dot tile_rb;

            PlanetCuteAtlas::on_tilemap_load(atlas);
            this->reset();

            tile_rb = this->get_map_tile_location(0, MatterPort::RB);
            this->create_logic_grid(xtile_count, ytile_count, Margin(tile_rb.y - margin.top, tile_rb.x));
        }
    };
}

/*********************************************************************************************/
void GYDM::JrPlane::load(float width, float height) {
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

    this->load_for_demo(width, height);
    this->load_for_plot(width, height);
}

void GYDM::JrPlane::load_for_demo(float width, float height) {
    // this->conveyer = this->insert(new ConveyerBeltlet(32.0F, 128.0F, 120.0));
}

void GYDM::JrPlane::load_for_plot(float width, float height) {
    this->stage = this->insert(new StageAtlas(xtile_count + 2, ytile_count + 2));
    this->host = this->insert(new Joshua("邹忌"));
    this->wife = this->insert(new Estelle("妻"));
    this->concubine = this->insert(new Klose("妾"));
    this->handsome = this->insert(new Olivier("徐公"));

    this->stage->scale(0.75F);
    this->set_bubble_margin({ 4.0F, 8.0F });
}
        
void GYDM::JrPlane::reflow(float width, float height) {
    this->move_to(this->title, Position(this->agent, MatterPort::RB), MatterPort::LB);
            
    for (int idx = 0; idx < this->coins.size(); idx ++) {
        if (idx == 0) {
            this->move_to(this->coins[idx], Position(this->agent, MatterPort::LB), MatterPort::LT);
        } else {
            this->move_to(this->coins[idx], Position(this->coins[idx - 1], MatterPort::RC), MatterPort::LC);
        }
    }

    if (this->coins.size() == 0) {
        this->move_to(this->tux, Position(this->agent, MatterPort::LB), MatterPort::LT);
    } else {
        this->move_to(this->tux, Position(this->coins[0], MatterPort::LB), MatterPort::LT);
    }

    this->reflow_demo(width, height);
    this->reflow_plot(width, height);
}

void GYDM::JrPlane::reflow_demo(float width, float height) {
}

void GYDM::JrPlane::reflow_plot(float width, float height) {
    this->move_to(this->stage, Position(width * 0.5, height), MatterPort::CB);
    this->stage->move_to_logic_tile(this->host, xtile_count / 2, ytile_count / 2, MatterPort::CB, MatterPort::CB);
    this->stage->move_to_logic_tile(this->wife, 0, -3, MatterPort::CB, MatterPort::CB);
    this->stage->move_to_logic_tile(this->concubine, 0, -1, MatterPort::CB, MatterPort::CB);
    this->stage->move_to_logic_tile(this->handsome, -1, 0, MatterPort::CB, MatterPort::CB);
}

void GYDM::JrPlane::update(uint64_t count, uint32_t interval, uint64_t uptime) {
    if (this->coins.size() > 0) {
        Dot tux_rb = this->get_matter_location(this->tux, MatterPort::RB);
        Dot star_rb = this->get_matter_location(this->coins.back(), MatterPort::RB);

        if (tux_rb.x >= star_rb.x) {
            Dot tux_cb = this->get_matter_location(this->tux, MatterPort::CB);
            
            if (tux_cb.x < star_rb.x) {
                this->tux->play("skid");
            } else {
                this->move(this->tux, Vector(- tux_rb.x, 0.0F));
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

void GYDM::JrPlane::on_mission_start(float width, float height) {
    this->agent->play_greeting(1);

    this->tux->set_border_strategy(BorderStrategy::IGNORE);
    this->tux->set_velocity(2.0, 0.0);
}

bool GYDM::JrPlane::can_select(IMatter* m) {
    return isinstance(m, Coinlet)
            || isinstance(m, Citizen)
            || (m == this->tux)
            || (m == this->agent);
}

void GYDM::JrPlane::on_tap(IMatter* m, float x, float y) {
    auto coin = dynamic_cast<Coinlet*>(m);
    auto citizen = dynamic_cast<Citizen*>(m);

    if (m == this->tux) {
        if (this->tux->is_wearing()) {
            this->tux->take_off();
        } else {
            this->tux->wear("santa_hat");
        }
    } else if (coin != nullptr) {
        if (coin->in_playing()) {
            this->target_plane = coin->get_index();
            this->agent->play_hide(1);
        }
    } else if (citizen != nullptr) {
        if (citizen->in_speech()) {
            citizen->shh();
        } else {
            citizen->say(2.0, citizen->nickname(),
                (citizen->gender() == CreatureGender::Male) ? GREEN : CRIMSON);
        }
    }
}

bool GYDM::JrPlane::update_tooltip(IMatter* m, float local_x, float local_y, float global_x, float global_y) {
    auto coin = dynamic_cast<Coinlet*>(m);
    bool updated = false;            
            
    if (coin != nullptr) {
        this->tooltip->set_text(" %s ", coin->name());
        this->tooltip->set_text_color(coin->in_playing() ? BLACK : GREY);
        updated = true;
    }

    return updated;
}
