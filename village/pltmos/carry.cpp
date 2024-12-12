#include "carry.hpp"

#include <plteen/bang.hpp>
#include <plteen/physics/random.hpp>

using namespace Plteen;
using namespace WarGrey::PLT;

/*************************************************************************************************/
static const double COUNTING_DURATION = 0.2;

static const char DEC_KEY = 'd';
static const char BIN_KEY = 'b';
static const char OCT_KEY = 'o';
static const char HEX_KEY = 'x';

static const facility_item_t ordered_keys[] = {
    { DEC_KEY, "十进制" },
    { BIN_KEY, "二进制" },
    { OCT_KEY, "八进制" },
    { HEX_KEY, "十六进制" }
};

static const bool key_states[][sizeof(ordered_keys)/sizeof(facility_item_t)] = {
    { false, true, true, true },
    { true, false, true, true },
    { true, true, false, true },
    { true, true, true, false },
};

/*************************************************************************************************/
namespace {
    class RadixAtlas : public PlanetCuteAtlas {
    public:
        RadixAtlas(int row, int col) : PlanetCuteAtlas(row, col, GroundBlockType::Grass), row(row) {}
        virtual ~RadixAtlas() noexcept {}

    public:
        int preferred_local_fps() override { return 4; }

    public:
        void set_available_rows(size_t row) {
            this->row = row;
            this->reset_map_tiles();
        }

    protected:
        void alter_map_tile(size_t r, size_t c) override {
            if (r == 0) {
                this->set_tile_type(r, c, GroundBlockType::Plain);
            } else if (r > this->row + 1) {
                this->set_tile_type(r, c, GroundBlockType::Water);
            } else if (r > this->row) {
                this->set_tile_type(r, c, GroundBlockType::Stone);
            }
        }

    private:
        size_t row;
    };
}

WarGrey::PLT::DotAndCarryOnePlane::DotAndCarryOnePlane(size_t num)
        : TheBigBang("位值制与进制系统"), TextFacilityPlane(GameFont::monospace(FontSize::large), ordered_keys, key_states) {
    if (num > 0) {
        this->animals = std::vector<Animal*>(num, nullptr);
    } else {
        this->animals = std::vector<Animal*>(random_uniform(128, 256), nullptr);
    }
}

/*************************************************************************************************/
void WarGrey::PLT::DotAndCarryOnePlane::load(float width, float height) {
    auto label_font = GameFont::monospace(FontSize::xx_large);
    auto digit_font = GameFont::fantasy(FontSize::x_large);
    auto base_font = GameFont::Default(FontSize::medium);
    auto expt_font = GameFont::Default(FontSize::xx_small);
    
    TextFacilityPlane::load(width, height);
    this->set_background(LIGHTSKYBLUE);

    this->counter = this->insert(new Labellet(label_font, BLACK, "??? ="));

    for (size_t idx = 0; idx < SLOTS; idx ++) {
        this->digits[idx] = this->insert(new Labellet(digit_font, BLACK, "0"));
        this->bases[idx] = this->insert(new Labellet(base_font, GRAY, "×10"));
        this->expts[idx] = this->insert(new Labellet(expt_font, ROYALBLUE, "%d", idx));
    }

    this->stage = this->insert(new RadixAtlas(SLOTS + 1, SLOTS));
    for (size_t idx = 0; idx < this->animals.size(); idx ++) {
        this->animals[idx] = this->insert(new Rooster());
        this->animals[idx]->set_border_strategy(BorderStrategy::BOUNCE);
    }
}

void WarGrey::PLT::DotAndCarryOnePlane::reflow(float width, float height) {
    TextFacilityPlane::reflow(width, height);

    this->move_to(this->stage, { width * 0.5F, height *  0.5F }, MatterPort::CC, { 0.0F, 10.0F });
    this->move_to(this->counter, { this->stage, MatterPort::RT }, MatterPort::RC);
    
    for (size_t idx = 0; idx < SLOTS; idx ++) {
        this->stage->move_to_map_tile(this->digits[idx], 0, SLOTS - idx - 1, MatterPort::LC, MatterPort::LC, { 2.0F, 0.0F });
        this->move_to(this->bases[idx], { this->digits[idx], MatterPort::RC }, MatterPort::LC, { 0.0F, 4.0F });
        this->move_to(this->expts[idx], { this->bases[idx], MatterPort::RT }, MatterPort::LT);
    }
}

void WarGrey::PLT::DotAndCarryOnePlane::graze(float width, float height) {
    for (size_t idx = 0; idx < this->animals.size(); idx ++) {
        this->glide_to_random_location(0.0, this->animals[idx]);
        this->animals[idx]->set_velocity(1.0, random_uniform(0.0, 360.0));
    }

    if (this->radix > 0) {
        static_cast<RadixAtlas*>(this->stage)->set_available_rows(this->radix - 1);
        this->bring_to_front(this->stage);
    } else {
        static_cast<RadixAtlas*>(this->stage)->set_available_rows(SLOTS + 1);
        this->send_to_back(this->stage);
    }

    for (size_t idx = 0; idx < SLOTS; idx ++) {
        if (idx < this->slots) {
            this->bring_to_front(this->digits[idx], this->stage);
            this->bring_to_front(this->bases[idx], this->stage);
            this->bring_to_front(this->expts[idx], this->stage);
            this->digits[idx]->set_text(MatterPort::LC, "%d", 0);
            this->bases[idx]->set_text(MatterPort::LC, (this->radix < 10) ? " ×%d" : "×%d", this->radix);
        } else {
            this->send_to_back(this->digits[idx], this->stage);
            this->send_to_back(this->bases[idx], this->stage);
            this->send_to_back(this->expts[idx], this->stage);
        }
    }
}

bool WarGrey::PLT::DotAndCarryOnePlane::can_select(IMatter* m) {
    return ThePLTPlane::can_select(m);
}

/*************************************************************************************************/
void WarGrey::PLT::DotAndCarryOnePlane::on_mission_start(float width, float height) {
    this->graze(width, height);
}

void WarGrey::PLT::DotAndCarryOnePlane::on_mission_complete() {
    this->radix = 0;
    this->slots = 0;
    this->seq = 0;
    this->counter->set_text(MatterPort::RC, "??? =");
}

void WarGrey::PLT::DotAndCarryOnePlane::on_new_counting_round(char key, float width, float height) {
    if (this->animals.size()) {
        switch(key) {
        case 'b': this->radix = 2;  this->slots = binary_digit(this->animals.size());      break;
        case 'o': this->radix = 8;  this->slots = octal_digit(this->animals.size());       break;
        case 'd': this->radix = 10; this->slots = decimal_digit(this->animals.size());     break;
        default:  this->radix = 16; this->slots = hexadecimal_digit(this->animals.size()); break;
        }

        this->seq = 1;
        this->graze(width, height);
        this->counting(0, 1, -1, COUNTING_DURATION);
    }
}

void WarGrey::PLT::DotAndCarryOnePlane::on_motion_complete(Plteen::IMatter* m, float x, float y, double xspd, double yspd) {
    if (this->animals[this->seq - 1] == m) {
        if (this->seq <= this->animals.size()) {
            int ones = int(this->seq % this->radix);
            this->digits[0]->set_text("%X", ones);
            this->bring_to_front(this->digits[0]);
            this->counter->set_text(MatterPort::RB, "%d =", this->seq);
            this->counting(this->seq, ones + 1, -1, COUNTING_DURATION);

            if (ones == 0) {
                size_t head = this->seq / this->radix;
                size_t digit = head % this->radix;
                size_t radix_size = this->radix;
                size_t slots_idx = 1;
                int col = -2;

                while (digit == 0) {
                    this->digits[slots_idx]->set_text("0");

                    head /= this->radix;
                    digit = head % this->radix;
                    radix_size *= this->radix;
                    slots_idx ++;
                    col --;
                };

                this->digits[slots_idx]->set_text("%X", digit);
                for (size_t idx = 1; idx <= radix_size; idx++) {
                    this->stage->glide_to_logic_tile(COUNTING_DURATION,
                            this->animals[this->seq - idx], digit, col,
                            MatterPort::CC, MatterPort::CC);
                }
            }

            this->seq ++;
        }
    }
}

void WarGrey::PLT::DotAndCarryOnePlane::counting(size_t idx, int row, int col, double duration) {
    if (idx < this->animals.size()) {
        bool is_carrying = (row == this->radix);
        double speech_duration = duration * (is_carrying ? 5.0 : 2.0);
        double motion_duration = duration * (is_carrying ? 3.0 : 1.0);
        std::string speech = (is_carrying ? make_nstring("逢 %d 进 1", row) : make_nstring("%X", row));

        this->bring_to_front(this->animals[idx]);
        this->animals[idx]->say(speech_duration, " " + speech + " ");
        this->stage->glide_to_logic_tile(motion_duration, this->animals[idx], row, col, MatterPort::CC, MatterPort::CC);
    }
}

void WarGrey::PLT::DotAndCarryOnePlane::on_facility_command(size_t idx, char key, float width, float height) {
    this->on_new_counting_round(key, width, height);
    this->notify_updated();
}
