#include "pinhole.hpp"

#include <gydm/physics/color/rgba.hpp>

#include <filesystem>
#include <vector>

using namespace WarGrey::STEM;
using namespace GYDM;

/*************************************************************************************************/
static const int default_frame_rate = 24;

static const float light_dot_radius = 2.0F;
static const float light_delta_length = 8.0F;
static const float moving_distance = 2.0F;

const uint32_t pinhole_color = BURLYWOOD;

/*************************************************************************************************/
static const char ILLU_KEY = 'i';
static const char STOP_KEY = 's';
static const char EDIT_KEY = 'e';
static const char PACE_KEY = 'p';
static const char CLER_KEY = 'c';
static const char RSET_KEY = 'r';

static const char ordered_keys[] = { ILLU_KEY, STOP_KEY, EDIT_KEY, PACE_KEY, CLER_KEY, RSET_KEY };
static const uint32_t colors_for_auto[] = { GRAY,  GREEN, GRAY,  GRAY,  GRAY,  GRAY };
static const uint32_t colors_for_stop[] = { GREEN, GRAY,  GREEN, GREEN, GRAY,  GRAY };
static const uint32_t colors_for_edit[] = { GREEN, GRAY,  GRAY,  GREEN, GREEN, GREEN };


/*************************************************************************************************/
class LightSource {
public:
    LightSource(float x, float y, const RGBA& rgb)
        : x(x), y(y), length(0.0F)
        , color(rgb), dirty(true) {}

public:
    float x;
    float y;
    float length;
    RGBA color;

public:
    bool dirty;
    shared_texture_t texture;
};

/*************************************************************************************************/
class WarGrey::STEM::PinholePlane::Pinholet : public GYDM::IGraphlet {
public:
    Pinholet(float width, float height) : width(width), height(height) {}
    virtual ~Pinholet() {}

public:
    Box get_bounding_box() override {
        return { this->width + 1.0F, this->height + 1.0F };
    }

    void draw(dc_t* dc, float x, float y, float Width, float Height) override {
        dc->fill_rect(x, y, Width, Height, this->background_color);
        
        for (auto light : this->lights) {
            float cx = light.x - 1.0F;
            float cy = light.y - 1.0F;

            if (light.texture.use_count() == 0) {
                light.texture = std::make_shared<Texture>(dc->create_blank_image(this->width, this->height));
            }

            if (light.texture->okay()) {
                if (light.dirty) {
                    SDL_Texture* origin = dc->get_target();

                    dc->set_target(light.texture->self());
                    dc->fill_rect(0.0F, 0.0F, this->width, this->height, transparent);

                    if (light.length > 0.0F) {
                        RGBA alt_color = RGBA(light.color, 0.1618);

                        dc->set_draw_color(alt_color);
                        this->draw_light_area(dc, cx, cy, light.length);
                    }

                    dc->set_target(origin);
                    light.dirty = false;
                }

                dc->stamp(light.texture->self(), x, y, Width, Height);
            }

            dc->fill_circle(x + cx, y + cy, light_dot_radius, RGBA(light.color, 0.8));
        }

        /* draw pinhole and screen */ {
            float px = x + this->pinhole_x;
            float py = y + this->pinhole_y;
            float pte = py + (this->pinhole_height - this->pinhole_size) * 0.5F;
            float pm  = py + this->pinhole_height * 0.5F;
            float pbs = pte + this->pinhole_size;
            float pb = py + this->pinhole_height;
            float sx = x + this->screen_x;
            float sy = y + this->screen_y;
            float sb = sy + this->screen_height;
            
            dc->draw_line(px, py, px, pte, pinhole_color);
            dc->draw_line(px, pbs, px, pb, pinhole_color);
            dc->draw_line(sx, sy, sx, sb, pinhole_color);

            for (auto light : this->lights) {
                if (light.x < this->pinhole_x) {
                    float cx = x + light.x - 1.0F;
                    float cy = y + light.y - 1.0F;
                    float pty, pby, pmy;

                    lines_intersect(cx, cy, px, pte, sx, sy, sx, sb, flnull_f, &pty, flnull_f);
                    lines_intersect(cx, cy, px, pm,  sx, sy, sx, sb, flnull_f, &pmy, flnull_f);
                    lines_intersect(cx, cy, px, pbs, sx, sy, sx, sb, flnull_f, &pby, flnull_f);

                    dc->draw_line(cx, cy, sx, pty, light.color);
                    dc->draw_line(cx, cy, sx, pmy, light.color);
                    dc->draw_line(cx, cy, sx, pby, light.color);
                }
            }
        }
    }

public:
    void set_background_color(uint32_t hex) {
        if (this->background_color != hex) {
            this->background_color = hex;
            this->notify_updated();
        }
    }

    void set_pinhole(float fx, float length, float size = 3.0F) {
        this->pinhole_size = size;
        this->pinhole_height = length;
        this->pinhole_x = this->width * fx;
        this->pinhole_y = (this->height - length) * 0.5F;
        
        this->notify_updated();
    }

    void set_screen(float fx, float length) {
        this->screen_x = this->width * fx;
        this->screen_y = (this->height - length) * 0.5F;
        this->screen_height = length;

        this->notify_updated();
    }

    void move_pinhole(float dx) {
        this->pinhole_x += dx;

        if (flin(1.0F, this->pinhole_x, this->screen_x - 1.0F)) {
            this->dirty_all();
            this->notify_updated();
        } else {
            this->pinhole_x -= dx;
        }
    }

    void move_screen(float dx) {
        this->screen_x += dx;
        
        if (flin(this->pinhole_x + 1.0F, this->screen_x, this->width)) {
            this->dirty_all();
            this->notify_updated();
        } else {
            this->screen_x -= dx;
        }
    }

    void move_pinhole_screen(float dx) {
        this->pinhole_x += dx;
        this->screen_x += dx;

        if ((this->pinhole_x > 0.0F) && (this->screen_x <= this->width)) {
            this->dirty_all();
            this->notify_updated();
        } else {
            this->pinhole_x -= dx;
            this->screen_x -= dx;
        }
    }

    void resize_hole(float d) {
        float dsize = d * 2.0F;

        this->pinhole_size += dsize;

        if ((this->pinhole_size > 0.0F) && (this->pinhole_size < this->pinhole_height)) {
            this->notify_updated();
        } else {
            this->pinhole_size -= dsize;
        }
    }
    
public:
    void add_light_source(float x, float y) {
        if (x < this->pinhole_x) {
            float precision = light_dot_radius * 0.618F;

            for (int i = 0; i < this->lights.size(); i ++) {
                float dx = flabs(this->lights[i].x - x);
                float dy = flabs(this->lights[i].y - y);

                if ((dx <= precision) && (dy <= precision)) {
                    this->lights.erase(this->lights.begin() + i);
                    this->notify_updated();

                    return;
                }
            }

            this->lights.push_back(LightSource(x, y, random_uniform(0x030303U, 0xDDDDDDU)));
            this->notify_updated();
        }
    }

    bool pace_forward() {
        bool stepped = false;

        for (int i = 0; i < this->lights.size(); i ++) {
            float mx = flmax(this->lights[i].x, this->width - this->lights[i].x);
            float my = flmax(this->lights[i].x, this->height - this->lights[i].y);
                
            if (this->lights[i].length < flmax(mx, my)) {
                stepped = true;
            } else {
                float mlength = point_distance(0.0F, 0.0F, mx, my);

                if (this->lights[i].length < mlength) {
                    stepped = true;
                }
            }

            if (stepped) {
                this->lights[i].length += light_delta_length;
                this->lights[i].dirty = true;
            }
        }

        if (stepped) {
            this->notify_updated();
        }

        return stepped;
    }

    void clear() {
        if (!this->lights.empty()) {
            for (int i = 0; i < this->lights.size(); i ++) {
                this->lights[i].length = 0.0F;
                this->lights[i].dirty = true;
            }

            this->notify_updated();
        }
    }

    void reset() {
        if (!this->lights.empty()) {
            this->lights.clear();
        }
    }

    void dirty_all() {
        for (int i = 0; i < this->lights.size(); i ++) {
            this->lights[i].dirty = true;
        }
    }

private:
    void draw_light_area(dc_t* dc, int cx, int cy, int radius) {
        float flx = float(cx);
        float fly = float(cy);
        int err = 2 - 2 * radius;
        int x = -radius;
        int y = 0;
    
        do {
            float self_x = flx - x;
            
            SDL_RenderDrawLineF(dc->self(), flx + x, cy,     flx + x, fly - y); // Q II

            if (self_x < this->pinhole_x) {
                SDL_RenderDrawLineF(dc->self(),   flx + x, fly + y, self_x, fly + y); // Q III, Q IV
                SDL_RenderDrawLineF(dc->self(),   flx,     fly - y, self_x, fly - y); // Q I
            } else {
                this->draw_light_line(dc, flx,     fly - y, self_x, fly - y, flx, fly); // Q I
                this->draw_light_line(dc, flx + x, fly + y, self_x, fly + y, flx, fly); // Q III, Q IV
            }

            radius = err;
            if (radius <= y) {
                err += ++y * 2 + 1;
            }

            if ((radius > x) || (err > y)) {
                err += ++x * 2 + 1;
            }
        } while (x < 0);
    }

    void draw_light_line(dc_t* dc, float x0, float y0, float rx, float y, float ox, float oy) {
        float px = this->pinhole_x;
        float py = this->pinhole_y;
        float pte = py + (this->pinhole_height - this->pinhole_size) * 0.5F;
        float pbs = pte + this->pinhole_size;
        float pb = py + this->pinhole_height;
        float sx = this->screen_x;
        float sy = this->screen_y;
        float sb = sy + this->screen_height;
        float pix, six, ptt, ptb, st;
        std::vector<float> xs = { x0, width };

        // 与孔阑有交点
        lines_intersect(x0, y0, rx, y, px, py, px, pte, flnull_f, flnull_f, flnull_f, &ptt);
        lines_intersect(x0, y0, rx, y, px, pbs, px, pb, flnull_f, flnull_f, flnull_f, &ptb);
        if (flin(0.0F, ptt, 1.0F) || flin(0.0F, ptb, 1.0F)) this->insert_ordered_x(xs, px);

        if (flin(px, oy, py)) {
            lines_intersect(x0, y0, rx, y, ox, oy, px, py, &pix, flnull_f, flnull_f, &ptt);
            lines_intersect(ox, oy, sx, sy, px, py, px, pb, flnull_f, flnull_f, flnull_f, &st);
            if (flin(0.0F, st, 1.0F)) { // 屏被孔阑遮挡，忽略
                this->insert_ordered_x(xs, (ptt >= 1.0F) ? flmin(pix, rx) : rx);
            } else { // 屏未被孔阑遮挡，光线能抵达靠右的交点
                lines_intersect(x0, y0, rx, y, ox, oy, sx, sb, &six, flnull_f, flnull_f, &ptb);

                if (ptb > 0.0F) {
                    this->insert_ordered_x(xs, (ptb > 0.0F) ? flmin(flmax(pix, six), rx) : rx);
                } else {
                    this->insert_ordered_x(xs, (ptt >= 1.0F) ? flmin(pix, rx) : rx);
                }
            }
        } else {
            this->insert_ordered_x(xs, rx);
        }

        for (int idx = 0; idx < xs.size() - 1; idx += 2) {
            SDL_RenderDrawLineF(dc->self(), xs[idx], y, xs[idx + 1], y);
        }
    }

    void insert_ordered_x(std::vector<float>& xs, float x) {
        for (auto it = xs.begin(); it != xs.end(); ++ it) {
            if (x < (*it)) {
                xs.emplace(it, x);
                break;
            }
        }
    }

private:
    float width;
    float height;

private:
    std::vector<LightSource> lights;
    float pinhole_x;
    float pinhole_y;
    float pinhole_height;
    float pinhole_size;
    float screen_x;
    float screen_y;
    float screen_height;

private:
    uint32_t background_color = BLACK;
};

/*************************************************************************************************/
void WarGrey::STEM::PinholePlane::load(float width, float height) {
    TheSTEMPlane::load(width, height);

    this->load_labview(width, height);
    this->load_instructions(width, height);

    this->set_local_fps(default_frame_rate);
}

void WarGrey::STEM::PinholePlane::load_labview(float width, float height) {
    float board_height = height - this->get_titlebar_height() * 2.0F;
    float board_width = width - this->get_titlebar_height();

    this->labview = this->insert(new PinholePlane::Pinholet(board_width, board_height));
    this->labview->set_pinhole(0.5F, 128.0F);
    this->labview->set_screen(0.7F, 128.0F);
}

void WarGrey::STEM::PinholePlane::load_instructions(float width, float height) {
    this->instructions[ILLU_KEY] = this->insert(new Labellet(GameFont::monospace(), "%c. 正常发光", ILLU_KEY));
    this->instructions[STOP_KEY] = this->insert(new Labellet(GameFont::monospace(), "%c. 停止发光", STOP_KEY));
    this->instructions[EDIT_KEY] = this->insert(new Labellet(GameFont::monospace(), "%c. 编辑光源", EDIT_KEY));
    this->instructions[CLER_KEY] = this->insert(new Labellet(GameFont::monospace(), "%c. 清除光线", CLER_KEY));
    this->instructions[RSET_KEY] = this->insert(new Labellet(GameFont::monospace(), "%c. 重制光源", RSET_KEY));
    this->instructions[PACE_KEY] = this->insert(new Labellet(GameFont::monospace(), "%c. 单步跟踪", PACE_KEY));
}

void WarGrey::STEM::PinholePlane::reflow(float width, float height) {
    TheSTEMPlane::reflow(width, height);

    this->move_to(this->labview, Position(width * 0.5F, (height + this->get_titlebar_height()) * 0.5F), MatterPort::CC);
    
    this->move_to(this->instructions[ordered_keys[0]], Position(0.0F, height), MatterPort::LB);
    for (int idx = 1; idx < sizeof(ordered_keys) / sizeof(char); idx ++) {
        this->move_to(this->instructions[ordered_keys[idx]],
                        Position(this->instructions[ordered_keys[idx - 1]], MatterPort::RB),
                        MatterPort::LB, { 16.0F, 0.0F } );
    }
}

void WarGrey::STEM::PinholePlane::on_mission_start(float width, float height) {
    this->switch_game_state(GameState::Edit);
}

void WarGrey::STEM::PinholePlane::update(uint64_t count, uint32_t interval, uint64_t uptime) {
    if (this->state == GameState::Auto) {
        this->pace_forward();
    }
}

/*************************************************************************************************/
bool WarGrey::STEM::PinholePlane::can_select(IMatter* m) {
    Labellet* menu = dynamic_cast<Labellet*>(m);

    return m == this->agent
        || ((this->state == GameState::Edit)
            && (m == this->labview))
        || ((menu != nullptr)
            && (menu->get_foreground_color() == GREEN));
}

void WarGrey::STEM::PinholePlane::on_tap(IMatter* matter, float x, float y) {
    if (matter == this->labview) {
        if (this->state == GameState::Edit) {
            this->labview->add_light_source(x, y);
        }
    } else if (isinstance(matter, Labellet)) {
        for (size_t idx = 0; idx < sizeof(ordered_keys) / sizeof(char);  idx ++) {
            if (this->instructions[ordered_keys[idx]] == matter) {
                this->on_char(ordered_keys[idx], 0, 1, false);
                this->no_selected();
                break;
            }
        }
    }
}

void WarGrey::STEM::PinholePlane::on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) {
    if (!pressed) {
        if (this->instructions.find(key) != this->instructions.end()) {
            if (this->instructions[key]->get_foreground_color() == GREEN) {
                switch(key) {
                case ILLU_KEY: this->switch_game_state(GameState::Auto); break;
                case STOP_KEY: this->switch_game_state(GameState::Stop); break;
                case EDIT_KEY: this->switch_game_state(GameState::Edit); break;
                case PACE_KEY: this->agent->play_processing(1); this->pace_forward(); break;
                case CLER_KEY: this->agent->play_empty_trash(1); this->labview->clear(); break;
                case RSET_KEY: this->agent->play_empty_trash(1); this->labview->reset(); break;
                }
            } else {
                this->instructions[key]->set_text_color(CRIMSON);
            }
        } else {
            switch(key) {
            case 'a': this->labview->move_pinhole(- moving_distance); break;
            case 'd': this->labview->move_pinhole(+ moving_distance); break;
            case 'j': this->labview->move_screen(- moving_distance); break;
            case 'l': this->labview->move_screen(+ moving_distance); break;
            case 'v': this->labview->move_pinhole_screen(- moving_distance); break;
            case 'n': this->labview->move_pinhole_screen(+ moving_distance); break;
            case 'w': this->labview->resize_hole(+1.0F); break;
            case 'z': this->labview->resize_hole(-1.0F); break;
            }    
        }
    }
}

/*************************************************************************************************/
void WarGrey::STEM::PinholePlane::pace_forward() {
    if (!this->labview->pace_forward()) {
        this->switch_game_state(GameState::Stop);
    }
}

/*************************************************************************************************/
void WarGrey::STEM::PinholePlane::switch_game_state(GameState new_state) {
    if (this->state != new_state) {
        switch (new_state) {
        case GameState::Auto: {
            this->agent->play_thinking(8);
            this->update_instructions_state(colors_for_auto);
        }; break;
        case GameState::Stop: {
            this->agent->play_rest_pose(1);
            this->update_instructions_state(colors_for_stop);
        }; break;
        case GameState::Edit: {
            this->agent->play_writing(-1);
            this->update_instructions_state(colors_for_edit);
        }; break;
        default: /* 什么都不做 */; break;
        }

        this->state = new_state;
        this->notify_updated();
    }
}

void WarGrey::STEM::PinholePlane::update_instructions_state(const uint32_t* colors) {
    for (size_t idx = 0; idx < sizeof(ordered_keys) / sizeof(char);  idx ++) {
        this->instructions[ordered_keys[idx]]->set_text_color(colors[idx]);
    }
}
