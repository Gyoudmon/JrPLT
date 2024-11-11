#include "lottery.hpp"

#include <plteen/datum/time.hpp>

#include <plteen/physics/color/rgba.hpp>

using namespace Plteen;
using namespace WarGrey::STEM;

/*************************************************************************************************/
static const float winning_font_size = 96.0F;

static const float ball_radius = 24.0F;
static const float machine_radius = 300.0F;
static const float winning_slot_height = 8.0F;

static const double gravity_accelaration = 0.618;
static const double terminal_speed = 24.0;
static const double fan_delta_range = 1.0;
static const double gliding_duration = 1.0;

/*************************************************************************************************/
static const char PLAY_KEY = 's';
static const char RSET_KEY = 'r';

/*************************************************************************************************/
class WarGrey::STEM::LotteryPlane::Ballet : public Ellipselet {
public:
    Ballet(size_t number) : Ellipselet(ball_radius, RGBA::HSV(random_uniform(0.0, 360.0))), No(number) {}
    virtual ~Ballet() noexcept {}

public:
    size_t number() const { return this->No; }

protected:
    void fill_shape(dc_t* dc, int width, int height, uint8_t r, uint8_t g, uint8_t b, uint8_t a) override {
        shared_font_t font = GameFont::monospace(ball_radius);
        std::string label = make_nstring("%02u", No);
        int lbl_width, lbl_height;

        Ellipselet::fill_shape(dc, width, height, r, g, b, a);
        font->feed_text_extent(label.c_str(), &lbl_width, &lbl_height);
        dc->draw_blended_text(label, font, 
            (float(width) - float(lbl_width)) * 0.5F, (float(height) - float(lbl_height)) * 0.618F,
            BLACK);
    }

private:
    size_t No;
};

/*************************************************************************************************/
WarGrey::STEM::LotteryPlane::LotteryPlane(const std::vector<uint8_t>& numbers
    , const std::vector<BallGroup>& groups, double fan_frequency)
    : TheSTEMPlane("摇奖机"), fan_frequency(fan_frequency), ball_numbers(numbers), ball_groups(groups) {
        this->ball_count = 0;
        for (auto group : groups) {
            this->ball_count += group.count;
        }
}

void WarGrey::STEM::LotteryPlane::load(float width, float height) {
    TheSTEMPlane::load(width, height);

    this->machine = this->insert(new RegularPolygonlet(6, machine_radius, GOLDENROD));
    this->window = this->insert(new Circlet(machine_radius * flcos(pi_f / 6.0F) * 0.85F, SNOW));
    this->inlet = this->insert(new Ellipselet(ball_radius, 8.0F, FORESTGREEN));
    this->outlet = this->insert(new Circlet(ball_radius + 2.0F, CHOCOLATE));
    this->winning_slot = this->insert(new Rectanglet(machine_radius * 2.618F, winning_slot_height, BURLYWOOD));

    this->load_balls(width, height);
    this->load_winning_numbers(width, height);
}

void WarGrey::STEM::LotteryPlane::reflow(float width, float height) {
    TheBigBang::reflow(width, height);
    
    this->move_to(this->machine, Position(width * 0.5F, height * 0.618F), MatterPort::CC);
    this->move_to(this->window, Position(this->machine, MatterPort::CC), MatterPort::CC);
    this->move_to(this->inlet, Position(this->window, MatterPort::CT), MatterPort::CB);
    this->move_to(this->outlet, Position(this->machine, MatterPort::RB), MatterPort::RB);
    this->move_to(this->winning_slot, Position(this->machine, MatterPort::CB), MatterPort::CT, { 0.0F, ball_radius * 3.0F });

    this->reflow_winning_numbers(width, height);
}

void WarGrey::STEM::LotteryPlane::load_balls(float width, float height) {
    for (size_t No : this->ball_numbers) {
        this->all_balls[No] = this->insert(new LotteryPlane::Ballet(No));
    }
}

void WarGrey::STEM::LotteryPlane::load_winning_numbers(float width, float height) {
    for (auto group : this->ball_groups) {
        for (size_t i = 0; i < group.count; i ++) {
            Labellet* ball_number = this->insert(new Labellet(GameFont::monospace(winning_font_size), GHOSTWHITE, "00"));
            Labellet* ball_label = this->insert(new Labellet(GameFont::Tooltip(FontSize::large), LIGHTGRAY, "%d 等奖", group.id));

            ball_number->set_background_color(group.color);
            this->winning_numbers.push_back(ball_number);
            this->winning_labels.push_back(ball_label);
        }
    }
}

void WarGrey::STEM::LotteryPlane::reflow_winning_numbers(float width, float height) {
    this->move_to(this->winning_numbers[0], Position(this->agent, MatterPort::LB), MatterPort::LT);
    for (size_t idx = 1; idx < this->winning_numbers.size(); idx ++) {
        this->move_to(this->winning_numbers[idx],
                        Position(this->winning_numbers[idx - 1], MatterPort::RC),
                        MatterPort::LC, { 2.0F, 0.0F });
    }

    for (size_t idx = 0; idx < this->winning_labels.size(); idx ++) {
        this->move_to(this->winning_labels[idx],
                        Position(this->winning_numbers[idx], MatterPort::CB),
                        MatterPort::CB);
    }
}

void WarGrey::STEM::LotteryPlane::on_mission_start(float width, float height) {
    this->switch_game_state(TCLMState::Reset);
}

void WarGrey::STEM::LotteryPlane::update(uint64_t count, uint32_t interval, uint64_t uptime) {
    Box box = this->window->get_bounding_box();
    cPoint O = this->get_matter_location(this->window, MatterPort::CC);
    
    if (this->state == TCLMState::Play) {
        double distance = box.width() * 0.5F - ball_radius;
        std::vector<LotteryPlane::Ballet*> lucky_balls;

        this->update_balls(this->all_balls, lucky_balls, O, distance,
            this->current_winning_slot >= this->winning_numbers.size());
        
        if (this->select(lucky_balls)) {
            this->picking_timestamp = current_inexact_milliseconds();
            this->prepare(this->all_balls);
        }
    }
}

void WarGrey::STEM::LotteryPlane::prepare(const std::map<size_t, LotteryPlane::Ballet*>& balls) {
    cPoint dot = this->get_matter_location(this->window, MatterPort::CC);
    Box box = this->window->get_bounding_box();
    float apothem = (box.width() * 0.5F - ball_radius) * flsqrt(2.0F) * 0.5F;

    for (auto ball : balls) {
        ball.second->show(true);
        this->spot_ball(ball.second, dot, apothem);
    }
}

void WarGrey::STEM::LotteryPlane::sally(const std::map<size_t, LotteryPlane::Ballet*>& balls) {
    for (auto ball : balls) {
        ball.second->set_terminal_speed(terminal_speed, terminal_speed);
        ball.second->set_speed(0.0, 0.0);
        ball.second->set_delta_speed(0.0, gravity_accelaration);
    }
}

void WarGrey::STEM::LotteryPlane::play() {
    if (this->current_winning_slot < this->winning_numbers.size()) {
        this->picking_timestamp = current_inexact_milliseconds();
        this->sally(this->all_balls);
    }
}

void WarGrey::STEM::LotteryPlane::reset() {
    this->all_balls.merge(this->win_balls);
    
    this->current_winning_slot = 0;
    for (auto number : this->winning_numbers) {
        number->set_text_alpha(0.0);
    }

    if (this->ball_count > 0) {
        this->prepare(this->all_balls);
    }
}

void WarGrey::STEM::LotteryPlane::update_balls(const std::map<size_t, Ballet*>& balls, std::vector<Ballet*>& lucky_balls, const cPoint& O, float distance, bool motion_only) {
    for (auto ball : balls) {
        this->apply_forces(ball.second, O, distance, motion_only);

        if (!motion_only) {
            if (this->is_colliding(ball.second, this->inlet)) {
                lucky_balls.push_back(ball.second);
            }
        }
    }
}

bool WarGrey::STEM::LotteryPlane::select(const std::vector<LotteryPlane::Ballet*>& balls) {
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

bool WarGrey::STEM::LotteryPlane::pick(LotteryPlane::Ballet* ball) {
    bool okay = false;

    if (ball != nullptr) {
        size_t No = ball->number();

        if (this->all_balls.find(No) != this->all_balls.end()) {
            this->all_balls.erase(No);
            this->win_balls[No] = ball;
            okay = true;

            this->move_to(ball, Position(this->outlet, MatterPort::CC), MatterPort::CC);
            this->winning_numbers[this->current_winning_slot]->set_text("%02u", ball->number());
            this->winning_numbers[this->current_winning_slot]->set_foreground_color(GHOSTWHITE);
            this->current_winning_slot += 1;
        
            /* moving the winning ball */ {
                float slot_width = ball_radius * 2.0F;
                cPoint out = this->get_matter_location(this->outlet, MatterPort::LB);
                cPoint end = this->get_matter_location(this->winning_slot, MatterPort::LB);
                cVector v = out - end;

                this->glide(gliding_duration, ball, cVector(0.0F, _Y(v)));
                this->glide(gliding_duration, ball, cVector(_X(v) + slot_width * float(this->current_winning_slot), 0.0F));
            }
        }
    }

    return okay;
}

/*************************************************************************************************/
void WarGrey::STEM::LotteryPlane::spot_ball(LotteryPlane::Ballet* ball, const cPoint& O, float apothem) {
    // 号码球的初始位置随机出现在摇奖机上部
    float bx = random_uniform(_X(O) - apothem, _X(O) + apothem);
    float by = random_uniform(_Y(O) - apothem, _Y(O));

    this->move_to(ball, Position(bx, by), MatterPort::CC);
    ball->motion_stop();
}

void WarGrey::STEM::LotteryPlane::apply_forces(LotteryPlane::Ballet* ball, const cPoint& O, float radius, bool no_fan) {
    double fan_dy = 0.0;
    cPoint B = this->get_matter_location(ball, MatterPort::CC);
    float distance = std::abs(B - O);
    
    if (!no_fan) {
        double now = current_inexact_milliseconds();

        if ((now - this->picking_timestamp) >= this->fan_frequency) {
            fan_dy = -random_uniform(0.1, fan_delta_range);
            this->picking_timestamp = now;
        }
    }

    if (distance > radius) {
        // 奖球反弹，运动轨迹由弹力、重力和支持力共同决定，摩擦损耗随机
        double theta = std::arg(O - B);
        double elasticity = double(distance - radius);
        double friction_loss = random_uniform(0.8, 1.0);
        double dx, dy;
        
        orthogonal_decompose(elasticity * friction_loss, theta, &dx, &dy);
        ball->add_speed(dx, dy + fan_dy);
    }
}

/*************************************************************************************************/
bool WarGrey::STEM::LotteryPlane::can_select(IMatter* m) {
    auto button = dynamic_cast<Ellipselet*>(m);
    auto ball = dynamic_cast<Ballet*>(m);

    return (m == this->agent)
            || ((button != nullptr) && (ball == nullptr));
}

void WarGrey::STEM::LotteryPlane::on_tap(IMatter* matter, float x, float y) {
    if (matter == this->inlet) {
        this->on_char(PLAY_KEY, 0, 1, false);
    } else if (matter == this->outlet) {
        this->on_char(RSET_KEY, 0, 1, false);
    }
}

void WarGrey::STEM::LotteryPlane::on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) {
    if (!pressed) {
        switch(key) {
        case PLAY_KEY: this->switch_game_state(TCLMState::Play); break;
        case RSET_KEY: this->switch_game_state(TCLMState::Reset); break;
        default: /* do nothing */;
        }
    }
}

/*************************************************************************************************/
void WarGrey::STEM::LotteryPlane::switch_game_state(TCLMState new_state) {
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
