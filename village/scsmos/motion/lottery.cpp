#include "lottery.hpp"

#include <gydm_stem/graphics/text.hpp>
#include <gydm_stem/graphics/brush.hpp>
#include <gydm_stem/graphics/color.hpp>

#include <filesystem>
#include <vector>

using namespace WarGrey::SCSM;
using namespace WarGrey::STEM;
using namespace std::filesystem;

/*************************************************************************************************/
static const int default_frame_rate = 24;

static const float light_dot_radius = 2.0F;
static const float light_delta_length = 8.0F;
static const float moving_distance = 2.0F;

const uint32_t pinhole_color = BURLYWOOD;

/*************************************************************************************************/
static const char STOP_KEY = 's';
static const char PLAY_KEY = 'p';
static const char RSET_KEY = 'r';

static const char ordered_keys[] = { PLAY_KEY, STOP_KEY, RSET_KEY };
static const uint32_t colors_for_play[] = { GRAY, GREEN,  GRAY };
static const uint32_t colors_for_stop[] = { GREEN, GRAY,  GREEN };
static const uint32_t colors_for_rest[] = { GREEN, GRAY,  GRAY };

/*************************************************************************************************/
void WarGrey::SCSM::LotteryPlane::load(float width, float height) {
    TheSCSMPlane::load(width, height);

    this->load_instructions(width, height);

    this->set_local_fps(default_frame_rate);
}

void WarGrey::SCSM::LotteryPlane::load_instructions(float width, float height) {
    this->instructions[PLAY_KEY] = this->insert(new Labellet(GameFont::monospace(), "%c. 摇号", PLAY_KEY));
    this->instructions[STOP_KEY] = this->insert(new Labellet(GameFont::monospace(), "%c. 暂停", STOP_KEY));
    this->instructions[RSET_KEY] = this->insert(new Labellet(GameFont::monospace(), "%c. 重制", RSET_KEY));
}

void WarGrey::SCSM::LotteryPlane::reflow(float width, float height) {
    TheSCSMPlane::reflow(width, height);

    this->move_to(this->instructions[ordered_keys[0]], 0.0F, height, MatterAnchor::LB);
    for (int idx = 1; idx < sizeof(ordered_keys) / sizeof(char); idx ++) {
        this->move_to(this->instructions[ordered_keys[idx]],
                    this->instructions[ordered_keys[idx - 1]], MatterAnchor::RB,
                    MatterAnchor::LB, 16.0F);
    }
}

void WarGrey::SCSM::LotteryPlane::on_mission_start(float width, float height) {
    this->switch_game_state(LMState::Reset);
}

void WarGrey::SCSM::LotteryPlane::update(uint64_t count, uint32_t interval, uint64_t uptime) {
    if (this->state == LMState::Play) {
        this->pace_forward();
    }
}

/*************************************************************************************************/
bool WarGrey::SCSM::LotteryPlane::can_select(IMatter* m) {
    Labellet* menu = dynamic_cast<Labellet*>(m);

    return m == this->agent
        || ((menu != nullptr)
            && (menu->get_foreground_color() == GREEN));
}

void WarGrey::SCSM::LotteryPlane::on_tap(IMatter* matter, float x, float y) {
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

void WarGrey::SCSM::LotteryPlane::on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) {
    if (!pressed) {
        if (this->instructions.find(key) != this->instructions.end()) {
            if (this->instructions[key]->get_foreground_color() == GREEN) {
                switch(key) {
                case PLAY_KEY: this->switch_game_state(LMState::Play); break;
                case STOP_KEY: this->switch_game_state(LMState::Stop); break;
                case RSET_KEY: this->agent->play_empty_trash(1); break;
                }
            } else {
                this->instructions[key]->set_text_color(CRIMSON);
            }
        } else {
            switch(key) {
            }    
        }
    }
}

/*************************************************************************************************/
void WarGrey::SCSM::LotteryPlane::pace_forward() {
    // if (!this->labview->pace_forward()) {
    //    this->switch_game_state(LMState::Stop);
    // }
}

/*************************************************************************************************/
void WarGrey::SCSM::LotteryPlane::switch_game_state(LMState new_state) {
    if (this->state != new_state) {
        switch (new_state) {
        case LMState::Play: {
            this->agent->play_get_artsy(8);
            this->update_instructions_state(colors_for_play);
        }; break;
        case LMState::Stop: {
            this->agent->play_rest_pose(1);
            this->update_instructions_state(colors_for_stop);
        }; break;
        case LMState::Reset: {
            this->agent->play_writing(-1);
            this->update_instructions_state(colors_for_rest);
        }; break;
        default: /* 什么都不做 */; break;
        }

        this->state = new_state;
        this->notify_updated();
    }
}

void WarGrey::SCSM::LotteryPlane::update_instructions_state(const uint32_t* colors) {
    for (size_t idx = 0; idx < sizeof(ordered_keys) / sizeof(char);  idx ++) {
        this->instructions[ordered_keys[idx]]->set_text_color(colors[idx]);
    }
}
