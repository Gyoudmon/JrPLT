#include "carry.hpp"

#include <plteen/bang.hpp>
#include <plteen/physics/random.hpp>

using namespace Plteen;
using namespace WarGrey::PLT;

/*************************************************************************************************/
static const float CELL_SIZE = 48.0F;
static const float LABEL_GAPSIZE = 16.0F;

static const char DEC_KEY = 'd';
static const char BIN_KEY = 'b';
static const char OCT_KEY = 'o';
static const char HEX_KEY = 'x';
static const char UI8_KEY = 'u';

static const facility_item_t ordered_keys[] = {
    { DEC_KEY, "十进制" },
    { BIN_KEY, "二进制" },
    { OCT_KEY, "八进制" },
    { HEX_KEY, "十六进制" },
    { UI8_KEY, "256进制" }
};

/*************************************************************************************************/
WarGrey::PLT::CarrySystemPlane::CarrySystemPlane(size_t num)
        : TheBigBang("位值制与进制"), TextFacilityPlane(GameFont::monospace(FontSize::large), ordered_keys, true) {
    if (num > 0) {
        this->animals = std::vector<Animal*>(num, nullptr);
    } else {
        this->animals = std::vector<Animal*>(random_uniform(16, 32), nullptr);
    }
}

/*************************************************************************************************/
void WarGrey::PLT::CarrySystemPlane::load(float width, float height) {
    auto label_font = GameFont::monospace();
    auto digit_font = GameFont::fantasy(FontSize::xx_large);
    auto system_font = GameFont::Default(FontSize::large);

    TextFacilityPlane::load(width, height);
    this->set_background(LIGHTSKYBLUE);
    this->set_grid_color(FORESTGREEN);

    for (size_t idx = 0; idx < SLOTS; idx ++) {
        this->digits[idx] = this->insert(new Labellet(digit_font, BLACK, "0"));
        this->slots[idx] = this->insert(new RoundedRectanglet(CELL_SIZE, 4.0F, 2.0F, DODGERBLUE));
    }
    
    for (size_t idx = 0; idx < this->animals.size(); idx ++) {
        this->animals[idx] = this->insert(new Rooster());
        this->animals[idx]->set_border_strategy(BorderStrategy::BOUNCE);
    }
}

void WarGrey::PLT::CarrySystemPlane::reflow(float width, float height) {
    TextFacilityPlane::reflow(width, height);
    
    float grid_x = (width  - CELL_SIZE * GCOLS) * 0.5F;
    float grid_y = (height - CELL_SIZE * GROWS) * 0.618F;

    this->create_grid(CELL_SIZE, CELL_SIZE, grid_x, grid_y, GROWS, GCOLS);
    
    for (size_t idx = SLOTS; idx > 0; idx --) {
        if (idx == SLOTS) {
            this->move_to(this->slots[idx - 1], { grid_x + CELL_SIZE * GCOLS, grid_y }, MatterPort::RB, { 0.0F, -LABEL_GAPSIZE });
        } else {
            this->move_to(this->slots[idx - 1], { this->slots[idx], MatterPort::LB }, MatterPort::RB);
        }

        this->move_to(this->digits[idx - 1], { this->slots[idx - 1], MatterPort::CT }, MatterPort::CB);
    }

    for (size_t idx = 0; idx < this->animals.size(); idx ++) {
        this->move_to(this->animals[idx], { random_uniform(0.0F, width), random_uniform(0.0F, height) } );
        this->animals[idx]->set_heading(random_uniform(0.0F, 360.0F));
        this->animals[idx]->set_velocity(1.0);
    }
}

void WarGrey::PLT::CarrySystemPlane::update(uint64_t count, uint32_t interval, uint64_t uptime) {
}

bool WarGrey::PLT::CarrySystemPlane::can_select(IMatter* m) {
    return ThePLTPlane::can_select(m);
}

bool WarGrey::PLT::CarrySystemPlane::update_tooltip(IMatter* m, float x, float y, float gx, float gy) {
    bool updated = false;

    return updated;
}

/*************************************************************************************************/
void WarGrey::PLT::CarrySystemPlane::on_facility_command(char key) {
    this->mode = key;
    this->notify_updated();
}
