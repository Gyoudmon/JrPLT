#include "pinhole.hpp"

#include <gydm_stem/graphics/text.hpp>

#include <filesystem>

using namespace WarGrey::SCSM;
using namespace WarGrey::STEM;
using namespace std::filesystem;

/*************************************************************************************************/
#define DEFAULT_CONWAY_DEMO digimon_path("demo/conway/typical", ".gof")

static const int default_frame_rate = 8;

/*************************************************************************************************/
static const char AUTO_KEY = 'a';
static const char STOP_KEY = 's';
static const char PACE_KEY = 'p';
static const char RSET_KEY = 'z';

static const char ordered_keys[] = { AUTO_KEY, STOP_KEY, PACE_KEY, RSET_KEY };
static const uint32_t colors_for_auto[] = { GRAY, GREEN, GRAY, GRAY, GRAY, GRAY, GRAY, GRAY };
static const uint32_t colors_for_stop[]  = { GREEN, GRAY, GREEN, GREEN, GRAY, GRAY, GRAY, GRAY };
static const uint32_t colors_for_edit[] = { GREEN, GRAY, GREEN, GRAY, GREEN, GREEN, GREEN, GREEN };

/*************************************************************************************************/
class WarGrey::SCSM::PinholePlane::Pinholet : public WarGrey::STEM::IGraphlet {
public:
    Pinholet(float width, float height) : width(width), height(height) {}
    virtual ~Pinholet() {}

public:
    void feed_extent(float x, float y, float* width = nullptr, float* height = nullptr) override {
        SET_BOX(width, this->width + 1.0F);
        SET_BOX(height, this->height + 1.0F);
    }
    void draw(SDL_Renderer* renderer, float x, float y, float Width, float Height) override {
        RGB_SetRenderDrawColor(renderer, this->color);
        game_draw_rect(renderer, x, y, Width, Height, this->color);
    }

public:
    void set_color(uint32_t hex) {
        if (this->color != hex) {
            this->color = hex;
            this->notify_updated();
        }
    }
    
public:
    bool pace_forward() {
        return false;
    }

    void reset() {
    }

private:
    float width;
    float height;

private:
    uint32_t color = BLACK;
};

/*************************************************************************************************/
void WarGrey::SCSM::PinholePlane::load(float width, float height) {
    TheSCSMPlane::load(width, height);

    this->load_gameboard(width, height);
    this->load_instructions(width, height);

    this->set_local_fps(default_frame_rate);
}

void WarGrey::SCSM::PinholePlane::load_gameboard(float width, float height) {
    float board_height = height - this->get_titlebar_height() * 2.0F;
    float board_width = width - this->get_titlebar_height();

    this->gameboard = this->insert(new PinholePlane::Pinholet(board_width, board_height));
}

void WarGrey::SCSM::PinholePlane::load_instructions(float width, float height) {
    this->instructions[AUTO_KEY] = this->insert(new Labellet(GameFont::monospace(), "%c. 自行演化", AUTO_KEY));
    this->instructions[STOP_KEY] = this->insert(new Labellet(GameFont::monospace(), "%c. 停止演化", STOP_KEY));
    this->instructions[RSET_KEY] = this->insert(new Labellet(GameFont::monospace(), "%c. 世界归零", RSET_KEY));
    this->instructions[PACE_KEY] = this->insert(new Labellet(GameFont::monospace(), "%c. 单步跟踪", PACE_KEY));
}

void WarGrey::SCSM::PinholePlane::reflow(float width, float height) {
    TheSCSMPlane::reflow(width, height);

    this->move_to(this->gameboard, width * 0.5F, (height + this->get_titlebar_height()) * 0.5F, MatterAnchor::CC);
    
    this->move_to(this->instructions[ordered_keys[0]], 0.0F, height, MatterAnchor::LB);
    for (int idx = 1; idx < sizeof(ordered_keys) / sizeof(char); idx ++) {
        this->move_to(this->instructions[ordered_keys[idx]],
                    this->instructions[ordered_keys[idx - 1]], MatterAnchor::RB,
                    MatterAnchor::LB, 16.0F);
    }
}

void WarGrey::SCSM::PinholePlane::on_mission_start(float width, float height) {
    this->switch_game_state(GameState::Stop);
}

void WarGrey::SCSM::PinholePlane::update(uint64_t count, uint32_t interval, uint64_t uptime) {
    if (this->state == GameState::Auto) {
        this->pace_forward();
    }
}

/*************************************************************************************************/
bool WarGrey::SCSM::PinholePlane::can_select(IMatter* m) {
    Labellet* menu = dynamic_cast<Labellet*>(m);

    return m == this->agent
        || ((this->state == GameState::Edit)
            && (m == this->gameboard))
        || ((menu != nullptr)
            && (menu->get_text_color() == GREEN));
}

void WarGrey::SCSM::PinholePlane::on_tap(IMatter* matter, float x, float y) {
    if (isinstance(matter, Labellet)) {
        for (size_t idx = 0; idx < sizeof(ordered_keys) / sizeof(char);  idx ++) {
            if (this->instructions[ordered_keys[idx]] == matter) {
                this->on_char(ordered_keys[idx], 0, 1, false);
                this->no_selected();
                break;
            }
        }
    }
}

void WarGrey::SCSM::PinholePlane::on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) {
    if (!pressed) {
        if (this->instructions.find(key) != this->instructions.end()) {
            if (this->instructions[key]->get_text_color() == GREEN) {
                switch(key) {
                case AUTO_KEY: this->switch_game_state(GameState::Auto); break;
                case STOP_KEY: this->switch_game_state(GameState::Stop); break;
                case RSET_KEY: this->agent->play_empty_trash(1); this->gameboard->reset(); break;
                case PACE_KEY: this->agent->play_processing(1); this->pace_forward(); break;
                }

                this->notify_updated();
            } else {
                this->instructions[key]->set_text_color(CRIMSON);
            }
        }
    }
}

/*************************************************************************************************/
void WarGrey::SCSM::PinholePlane::pace_forward() {
    this->gameboard->pace_forward();
}

/*************************************************************************************************/
void WarGrey::SCSM::PinholePlane::switch_game_state(GameState new_state) {
    if (this->state != new_state) {
        switch (new_state) {
        case GameState::Auto: {
            this->gameboard->set_color(LIGHTSKYBLUE);
            this->agent->play_thinking(8);
            this->update_instructions_state(colors_for_auto);
        }; break;
        case GameState::Stop: {
            this->gameboard->set_color(DIMGRAY);
            this->agent->play_rest_pose(1);
            this->update_instructions_state(colors_for_stop);
        }; break;
        case GameState::Edit: {
            this->gameboard->set_color(ROYALBLUE);
            this->agent->play_writing(-1);
            this->update_instructions_state(colors_for_edit);
        }; break;
        default: /* 什么都不做 */; break;
        }

        this->state = new_state;
        this->notify_updated();
    }
}

void WarGrey::SCSM::PinholePlane::update_instructions_state(const uint32_t* colors) {
    for (size_t idx = 0; idx < sizeof(ordered_keys) / sizeof(char);  idx ++) {
        this->instructions[ordered_keys[idx]]->set_text_color(colors[idx]);
    }
}
