#include "splash.hpp"

using namespace Plteen;

/*************************************************************************************************/
static const char* unknown_task_name = "冒险越来越深入了";
static const char* task_name_fmt = "%02d %s";

static const int advent_days = 25;

/*************************************************************************************************/
namespace {
    static const int Tile_Count = 15;
    static const float Tile_Size = 48.0F;
}

/*********************************************************************************************/
Plteen::JrPlane::JrPlane(Cosmos* master) : Plane("青少计算机科学"), master(master) {
    this->set_background(BLACK);
    this->set_grid_color(AZURE);
}

/*********************************************************************************************/
void Plteen::JrPlane::load(float width, float height) {
    this->title = this->insert(new Labellet(GameFont::Title(), GHOSTWHITE, this->name()));
            
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

void Plteen::JrPlane::load_for_demo(float width, float height) {
    // this->conveyer = this->insert(new ConveyerBeltlet(32.0F, 128.0F, 120.0));
}

void Plteen::JrPlane::load_for_plot(float width, float height) {
    this->host = this->insert(new Joshua("邹忌"));
    this->wife = this->insert(new Estelle("妻"));
    this->concubine = this->insert(new Klose("妾"));
    this->handsome = this->insert(new Olivier("徐公"));

    this->set_bubble_margin({ 4.0F, 8.0F });
}
        
void Plteen::JrPlane::reflow(float width, float height) {
    this->create_centered_grid(::Tile_Count, ::Tile_Size, { 0.0F, float(this->title->display_height()) * 1.618F });

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

void Plteen::JrPlane::reflow_demo(float width, float height) {
}

void Plteen::JrPlane::reflow_plot(float width, float height) {
    this->move_to_grid(this->host, Tile_Count / 2, Tile_Count / 2, MatterPort::CB);
    this->move_to_grid(this->wife, 0, Tile_Count - 3, MatterPort::CB);
    this->move_to_grid(this->concubine, 0, Tile_Count - 1, MatterPort::CB);
    this->move_to_grid(this->handsome, Tile_Count - 1, 0, MatterPort::CB);
}

void Plteen::JrPlane::update(uint64_t count, uint32_t interval, uint64_t uptime) {
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

void Plteen::JrPlane::on_mission_start(float width, float height) {
    this->agent->play_greeting(1);

    this->tux->set_border_strategy(BorderStrategy::IGNORE);
    this->tux->set_velocity(2.0, 0.0);
}

bool Plteen::JrPlane::can_select(IMatter* m) {
    return isinstance(m, Coinlet)
            || isinstance(m, Citizen)
            || (m == this->tux)
            || (m == this->agent);
}

void Plteen::JrPlane::on_tap(IMatter* m, float x, float y) {
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

bool Plteen::JrPlane::update_tooltip(IMatter* m, float local_x, float local_y, float global_x, float global_y) {
    auto coin = dynamic_cast<Coinlet*>(m);
    bool updated = false;            
            
    if (coin != nullptr) {
        this->tooltip->set_text(" %s ", coin->name());
        this->tooltip->set_text_color(coin->in_playing() ? BLACK : GREY);
        updated = true;
    }

    return updated;
}
