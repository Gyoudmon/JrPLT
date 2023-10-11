#include "track.hpp"

using namespace WarGrey::STEM;

/*************************************************************************************************/
static double gliding_duration = 0.2;

/*************************************************************************************************/
void WarGrey::STEM::TrackPlane::load(float width, float height) {
    this->track = this->insert(new Tracklet(width, height));

    TheBigBang::load(width, height);

    this->bracers.push_back(this->insert(new Estelle()));
    this->bracers.push_back(this->insert(new Joshua()));
    this->bracers.push_back(this->insert(new Scherazard()));
    this->bracers.push_back(this->insert(new Olivier()));
    this->bracers.push_back(this->insert(new Agate()));
    this->bracers.push_back(this->insert(new Klose()));
    this->bracers.push_back(this->insert(new Tita()));
    this->bracers.push_back(this->insert(new Zin()));

    for (auto bracer : this->bracers) {
        this->bind_canvas(bracer, this->track, 0.5F, 0.9F, true);
    }
}

void WarGrey::STEM::TrackPlane::update(uint64_t interval, uint32_t count, uint64_t uptime) {
    if (is_shift_pressed()) {
        for (auto bracer : this->bracers) {
            bracer->try_switch_mode(BracerMode::Run);
        }
    } else {
        for (auto bracer : this->bracers) {
            bracer->try_switch_mode(BracerMode::Walk);
        }
    }
}

void WarGrey::STEM::TrackPlane::on_enter(IPlane *from) {
    this->agent->play("Greeting", 1);
    this->run_bracers_at_random(false);
}

bool WarGrey::STEM::TrackPlane::can_select(IMatter *m) {
    return isinstance(m, Citizen) || (this->agent == m);
}

void WarGrey::STEM::TrackPlane::after_select(IMatter *m, bool yes) {
    if (!yes) {
        this->glide_to_mouse(gliding_duration, m, MatterAnchor::CC);
    }
}

void WarGrey::STEM::TrackPlane::on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) {
    if (pressed) {
        switch (key) {
        case 'r': this->run_bracers_at_random(true); break;
        case '8': this->run_bracers_in_8_ways(); break;
        case ' ': this->run_bracers_in_direction(); break;
        case 'c': this->track->erase(); break;
        }
    }
}

bool WarGrey::STEM::TrackPlane::update_tooltip(IMatter *m, float lx, float ly, float gx, float gy) {
    bool updated = false;

    if (this->can_select(m)) {
        this->tooltip->set_text("heading: %.02lf˚", m->get_heading(false));
        updated = true;
    }

    return updated;
}

void WarGrey::STEM::TrackPlane::run_bracers_at_random(bool drawing) {
    for (auto bracer : this->bracers) {
        this->glide_to_random_location(gliding_duration, bracer);
    }
}

void WarGrey::STEM::TrackPlane::run_bracers_in_direction() {
    float length = 72.0F;

    for (auto bracer : this->bracers) {
        this->glide(gliding_duration, bracer, length);
    }
}

void WarGrey::STEM::TrackPlane::run_bracers_in_8_ways() {
    IMatter* selected = this->find_next_selected_matter();

    if (selected != nullptr) {
        this->run_bracer_in_8_ways(selected, 6, 6, 16.0);
    } else {
        for (size_t i = 0; i < this->bracers.size(); i ++) {
            this->run_bracer_in_8_ways(this->bracers[i], 3 + i, 3 + i, 16.0);
        }
    }
}

void WarGrey::STEM::TrackPlane::run_bracer_in_8_ways(IMatter* bracer, int sides, int rounds, double gapsize) {
    double meridian = double(rounds * gapsize);
    double rad = degrees_to_radians(360.0 / sides);
    double factor = 2.0 - 2.0 * flcos(rad); 
    double direction = bracer->get_heading();
    float x, y;

    this->feed_matter_location(bracer, &x, &y, MatterAnchor::LT);
            
    this->set_pen_color(bracer, random_uniform(0.0, 360.0));

    for (int s = 0; s < sides; s ++) {
        this->pen_up(bracer);
        this->move_to(bracer, x, y, MatterAnchor::LT); // moving doesn't change the heading
        this->pen_down(bracer);
        this->glide(gliding_duration, bracer, meridian);
        this->turn(bracer, rad, true);
    }

    while (meridian > 4.0) {
        double parallel = flsqrt(meridian * meridian * factor); 

        this->pen_up(bracer);
        this->set_pen_color(bracer, random_uniform(0.0, 360.0));
        this->move_to(bracer, x, y, MatterAnchor::LT); // moving doesn't change the heading
        this->set_heading(bracer, direction, true);
        this->move(bracer, meridian);
        this->pen_down(bracer);
        this->turn(bracer, (pi - rad) / 2.0, true);

        for (int s = 0; s < sides; s ++) {
            this->turn(bracer, rad, true);
            this->glide(gliding_duration, bracer, parallel);
        }

        meridian -= gapsize;
    }

    this->move_to(bracer, x, y, MatterAnchor::LT);
    this->stamp(bracer);
    this->pen_up(bracer);
}
