#include "lottery.hpp"

#include <gydm_stem/graphics/text.hpp>
#include <gydm_stem/graphics/brush.hpp>
#include <gydm_stem/graphics/color.hpp>
#include <gydm_stem/datum/time.hpp>

using namespace WarGrey::SCSM;
using namespace WarGrey::STEM;

/*************************************************************************************************/
static const float winning_font_size = 96.0F;

static const float ball_radius = 24.0F;
static const float machine_radius = 256.0F;
static const float winning_slot_height = 8.0F;

static const double gravity_accelaration = 0.618;
static const double fan_delta_speed = -0.382;
static const double gliding_duration = 1.0;

/*************************************************************************************************/
static const char PLAY_KEY = 'p';
static const char RSET_KEY = 'r';

/*************************************************************************************************/
class WarGrey::SCSM::TwoColorLotteryPlane::Ballet : public Ellipselet {
public:
    Ballet(size_t number, TCLMColor type)
        : Ellipselet(ball_radius, RGBA::HSV(random_uniform(0.0, 360.0)))
        , No(number), ball_type(type) {}

    virtual ~Ballet() noexcept {}

public:
    size_t number() const { return this->No; }
    TCLMColor type() const { return this->ball_type; }

protected:
    void fill_shape(SDL_Renderer* renderer, int width, int height, uint8_t r, uint8_t g, uint8_t b, uint8_t a) override {
        shared_font_t font = GameFont::monospace(ball_radius);
        std::string label = make_nstring("%02u", No);
        int lbl_width, lbl_height;

        Ellipselet::fill_shape(renderer, width, height, r, g, b, a);
        font->feed_text_extent(label.c_str(), &lbl_width, &lbl_height);
        Pen::draw_blended_text(font, renderer, BLACK,
                                (float(width) - float(lbl_width)) * 0.5F,
                                (float(height) - float(lbl_height)) * 0.618F,
                                label);
    }

private:
    size_t No;
    TCLMColor ball_type;
};

/*************************************************************************************************/
void WarGrey::SCSM::TwoColorLotteryPlane::load(float width, float height) {
    TheSCSMPlane::load(width, height);

    this->machine = this->insert(new RegularPolygonlet(6, machine_radius, GOLDENROD));
    this->window = this->insert(new Circlet(machine_radius * flcos(pi_f / 6.0F) * 0.85F, SNOW));
    this->inlet = this->insert(new Ellipselet(ball_radius, 8.0F, FORESTGREEN));
    this->outlet = this->insert(new Circlet(ball_radius + 2.0F, CHOCOLATE));
    this->winning_slot = this->insert(new Rectanglet(machine_radius * 2.618F, winning_slot_height, BURLYWOOD));

    this->load_balls(width, height);
    this->load_winning_numbers(width, height);
}

void WarGrey::SCSM::TwoColorLotteryPlane::reflow(float width, float height) {
    TheSCSMPlane::reflow(width, height);
    
    this->move_to(this->machine, width * 0.5F, height * 0.618F, MatterAnchor::CC);
    this->move_to(this->window, this->machine, MatterAnchor::CC, MatterAnchor::CC);
    this->move_to(this->inlet, this->window, MatterAnchor::CT, MatterAnchor::CB);
    this->move_to(this->outlet, this->machine, MatterAnchor::RB, MatterAnchor::RB);
    this->move_to(this->winning_slot, this->machine, MatterAnchor::CB, MatterAnchor::CT, 0.0F, ball_radius * 3.0F);

    this->reflow_winning_numbers(width, height);

    this->move_to(this->tips[0], width, 0.0F, MatterAnchor::RT);
    for (size_t idx = 1; idx < this->tips.size(); idx ++) {
        this->move_to(this->tips[idx], this->tips[idx - 1], MatterAnchor::RB, MatterAnchor::RT, 0.0F, 2.0F);
    }
}

void WarGrey::SCSM::TwoColorLotteryPlane::load_balls(float width, float height) {
    this->load_tip("剩余红球", this->red_top, CRIMSON);
    this->load_tip("剩余蓝球", this->blue_top, ROYALBLUE);
    this->load_tip("已出号码", this->current_winning_slot, FORESTGREEN);
    
    for (size_t idx = 1; idx <= this->red_top; idx ++) {
        this->red_balls[idx] = this->insert(new TwoColorLotteryPlane::Ballet(idx, TCLMColor::Red));
    }

    for (size_t idx = 1; idx <= this->blue_top; idx ++) {
        this->blue_balls[idx] = this->insert(new TwoColorLotteryPlane::Ballet(idx, TCLMColor::Blue));
    }
}

void WarGrey::SCSM::TwoColorLotteryPlane::load_winning_numbers(float width, float height) {
    for (size_t idx = 0; idx < this->red_count; idx ++) {
        Labellet* red_number = this->insert(new Labellet(GameFont::monospace(winning_font_size), GHOSTWHITE, "00"));

        red_number->set_background_color(CRIMSON);
        this->winning_numbers.push_back(red_number);
    }

    for (size_t idx = 0; idx < this->blue_count; idx ++) {
        Labellet* blue_number = this->insert(new Labellet(GameFont::monospace(winning_font_size), GHOSTWHITE, "00"));

        blue_number->set_background_color(ROYALBLUE);
        this->winning_numbers.push_back(blue_number);
    }
}

void WarGrey::SCSM::TwoColorLotteryPlane::load_tip(const char* label, size_t value, uint32_t label_bgc) {
    DimensionStyle style = make_highlight_dimension_style(generic_font_size(FontSize::x_large), 3, 0);

    style.label_background_color = label_bgc;
    this->tips.push_back(this->insert(new Dimensionlet(style, "个", label)));
    this->tips.back()->set_value(double(value));
}

void WarGrey::SCSM::TwoColorLotteryPlane::reflow_winning_numbers(float width, float height) {
    this->move_to(this->winning_numbers[0], this->agent, MatterAnchor::LB, MatterAnchor::LT);
    for (size_t idx = 1; idx < this->winning_numbers.size(); idx ++) {
        this->move_to(this->winning_numbers[idx], this->winning_numbers[idx - 1], MatterAnchor::RC, MatterAnchor::LC);        
    }
}

void WarGrey::SCSM::TwoColorLotteryPlane::on_mission_start(float width, float height) {
    this->switch_game_state(TCLMState::Reset);
}

void WarGrey::SCSM::TwoColorLotteryPlane::update(uint64_t count, uint32_t interval, uint64_t uptime) {
    float cx, cy, window_size;

    this->feed_matter_location(this->window, &cx, &cy, MatterAnchor::CC);
    this->window->feed_extent(0.0F, 0.0F, &window_size);
        
    if (this->state == TCLMState::Play) {
        double distance = window_size * 0.5F - ball_radius;
        std::vector<TwoColorLotteryPlane::Ballet*> lucky_balls;

        switch (this->substate) {
        case TCLMSubState::PlayRed: {
            this->update_balls(this->red_balls, lucky_balls,
                cx, cy, distance,
                this->current_winning_slot >= this->red_count);
        }; break;
        case TCLMSubState::PlayBlue: {
            this->update_balls(this->blue_balls, lucky_balls,
                cx, cy, distance,
                this->current_winning_slot >= this->winning_numbers.size());
        }; break;
        default: /* do nothing */;
        }
        
        if (this->select(lucky_balls)) {
            this->picking_timestamp = current_inexact_milliseconds();
        }
    }
}

void WarGrey::SCSM::TwoColorLotteryPlane::prepare(const std::map<size_t, TwoColorLotteryPlane::Ballet*>& balls) {
    float cx, cy, apothem, window_size;

    this->feed_matter_location(this->window, &cx, &cy, MatterAnchor::CC);
    this->window->feed_extent(0.0F, 0.0F, &window_size);
    apothem = (window_size * 0.5F - ball_radius) * flsqrt(2.0F) * 0.5F;

    for (auto ball : balls) {
        ball.second->show(true);
        this->spot_ball(ball.second, cx, cy, apothem);
    }
}

void WarGrey::SCSM::TwoColorLotteryPlane::sally(const std::map<size_t, TwoColorLotteryPlane::Ballet*>& balls) {
    for (auto ball : balls) {
        ball.second->set_speed(0.0, 0.0);
        ball.second->set_delta_speed(0.0, gravity_accelaration);
    }
}

void WarGrey::SCSM::TwoColorLotteryPlane::hide(const std::map<size_t, TwoColorLotteryPlane::Ballet*>& balls) {
    for (auto ball : balls) {
        ball.second->show(false);
        ball.second->motion_stop();
    }
}

void WarGrey::SCSM::TwoColorLotteryPlane::play() {
    if (this->current_winning_slot < this->winning_numbers.size()) {
        this->picking_timestamp = current_inexact_milliseconds();

        if (this->current_winning_slot < this->red_count) {
            if (this->current_winning_slot > 0) {
                this->prepare(this->red_balls);
            }

            this->sally(this->red_balls);
            this->substate = TCLMSubState::PlayRed;
        } else  {
            if (this->current_winning_slot > 0) {
                this->prepare(this->blue_balls);
            }

            this->hide(this->red_balls);
            this->sally(this->blue_balls);
            this->substate = TCLMSubState::PlayBlue;
        }
    }
}

void WarGrey::SCSM::TwoColorLotteryPlane::reset() {
    this->red_balls.merge(this->win_red_balls);
    this->blue_balls.merge(this->win_blue_balls);

    this->tips[0]->set_value(double(this->red_balls.size()));
    this->tips[1]->set_value(double(this->blue_balls.size()));
    this->tips[2]->set_value(0.0);

    this->current_winning_slot = 0;
    for (auto number : this->winning_numbers) {
        number->set_text_alpha(0.0);
    }

    if (this->red_count > 0) {
        this->prepare(this->red_balls);
        this->hide(this->blue_balls);
    } else {
        this->hide(this->red_balls);
        this->prepare(this->blue_balls);
    }
}

void WarGrey::SCSM::TwoColorLotteryPlane::update_balls(const std::map<size_t, Ballet*>& balls, std::vector<Ballet*>& lucky_balls, float cx, float cy, float distance, bool motion_only) {
    for (auto ball : balls) {
        if (ball.second->visible()) {
            this->apply_forces(ball.second, cx, cy, distance, motion_only);

            if (!motion_only) {
                if (this->is_colliding(ball.second, this->inlet)) {
                    lucky_balls.push_back(ball.second);
                }
            }
        }
    }
}

bool WarGrey::SCSM::TwoColorLotteryPlane::select(const std::vector<TwoColorLotteryPlane::Ballet*>& balls) {
    size_t top = balls.size();
    bool okay = false;

    if (top > 0) {
        size_t target = random_uniform(0, top - 1);

        if (target < top) {
            okay = this->pick(balls[target]);
        }
    }

    return okay;
}

bool WarGrey::SCSM::TwoColorLotteryPlane::pick(TwoColorLotteryPlane::Ballet* ball) {
    bool okay = false;

    if (ball != nullptr) {
        size_t No = ball->number();

        switch (ball->type()) {
        case TCLMColor::Red: {
            this->red_balls.erase(No);
            this->win_red_balls[No] = ball;
            this->tips[0]->set_value(double(this->red_balls.size()));
            okay = true;
        }; break;
        case TCLMColor::Blue: {
            this->blue_balls.erase(No);
            this->win_blue_balls[No] = ball;
            this->tips[1]->set_value(double(this->blue_balls.size()));
            okay = true;
        }; break;
        default: /* do nothing */;
        }

        this->move_to(ball, this->outlet, MatterAnchor::CC, MatterAnchor::CC);
        this->winning_numbers[this->current_winning_slot]->set_text("%02u", ball->number());
        this->winning_numbers[this->current_winning_slot]->set_foreground_color(GHOSTWHITE);
        this->current_winning_slot += 1;
        this->tips[2]->set_value(double(this->current_winning_slot));

        /* moving the winning ball */ {
            float slot_width = ball_radius * 2.0F;
            float y0, sx, ey, ex;

            this->feed_matter_location(this->outlet, &sx, &y0, MatterAnchor::LB);
            this->feed_matter_location(this->winning_slot, &ex, &ey, MatterAnchor::LB);
            this->glide(gliding_duration, ball, 0.0F, ey - y0);
            this->glide(gliding_duration, ball, ex - sx + slot_width * float(this->current_winning_slot), 0.0F);
        }
    }

    return okay;
}

/*************************************************************************************************/
void WarGrey::SCSM::TwoColorLotteryPlane::spot_ball(TwoColorLotteryPlane::Ballet* ball, float cx, float cy, float apothem) {
    float bx = random_uniform(cx - apothem, cx + apothem);
    float by = random_uniform(cy - apothem, cy);

    this->move_to(ball, bx, by, MatterAnchor::CC);
    ball->motion_stop();
}

void WarGrey::SCSM::TwoColorLotteryPlane::apply_forces(TwoColorLotteryPlane::Ballet* ball, float cx, float cy, float radius, bool no_fan) {
    float bx, by, distance;
    double dx, dy;

    this->feed_matter_location(ball, &bx, &by, MatterAnchor::CC);
    distance = point_distance(bx, by, cx, cy);

    if (distance > radius) {
        double theta = vector_direction(double(cx - bx), double(cy - by));

        orthogonal_decomposition(double(distance - radius), theta, &dx, &dy);
        
        // this->move(ball, -dx, -dy); // this makes the ball eventually do circular motion
        ball->add_speed(dx, dy);
    }

    if (!no_fan) {
        if ((current_inexact_milliseconds() - this->picking_timestamp) >= this->fan_frequency) {
            ball->add_speed(0.0, fan_delta_speed);
        }
    }
}

/*************************************************************************************************/
bool WarGrey::SCSM::TwoColorLotteryPlane::can_select(IMatter* m) {
    auto button = dynamic_cast<Ellipselet*>(m);
    auto ball = dynamic_cast<Ballet*>(m);

    return (m == this->agent)
            || ((button != nullptr) && (ball == nullptr));
}

void WarGrey::SCSM::TwoColorLotteryPlane::on_tap(IMatter* matter, float x, float y) {
    if (matter == this->inlet) {
        this->on_char(PLAY_KEY, 0, 1, false);
    } else if (matter == this->outlet) {
        this->on_char(RSET_KEY, 0, 1, false);
    }
}

void WarGrey::SCSM::TwoColorLotteryPlane::on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) {
    if (!pressed) {
        switch(key) {
        case PLAY_KEY: this->switch_game_state(TCLMState::Play); break;
        case RSET_KEY: this->switch_game_state(TCLMState::Reset); break;
        default: /* do nothing */;
        }
    }
}

/*************************************************************************************************/
void WarGrey::SCSM::TwoColorLotteryPlane::switch_game_state(TCLMState new_state) {
    switch (new_state) {
    case TCLMState::Play: {
        this->agent->play_get_artsy(8);
        this->play();
    }; break;
    case TCLMState::Reset: {
        this->agent->play_writing(-1);
        this->reset();
    }; break;
    default: {
        this->agent->stop();
    }; break;
    }

    this->state = new_state;
    this->notify_updated();
}
