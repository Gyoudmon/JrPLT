#include "track.hpp"

using namespace Plteen;

/*************************************************************************************************/
static double gliding_duration = 0.2;

/*************************************************************************************************/
void Plteen::TrackPlane::construct(float width, float height) {
    this->the_name("Tamer");
    this->style = make_highlight_dimension_style(24U, 8U, 2);
}

void Plteen::TrackPlane::load(float width, float height) {
    this->track = this->insert(new Tracklet(width, height));

    TheBigBang::load(width, height);

    this->variable = this->insert(new Dimensionlet(this->style, "deg", "方向"));
    this->variable->bind_value(this->heading);

    this->bracers.push_back(this->insert(new Estelle()));
    this->bracers.push_back(this->insert(new Joshua()));
    this->bracers.push_back(this->insert(new Scherazard()));
    this->bracers.push_back(this->insert(new Olivier()));
    this->bracers.push_back(this->insert(new Agate()));
    this->bracers.push_back(this->insert(new Klose()));
    this->bracers.push_back(this->insert(new Tita()));
    this->bracers.push_back(this->insert(new Zin()));

    for (auto bracer : this->bracers) {
        this->bind_canvas(bracer, this->track, { 0.5F, 0.9F }, true);
    }
}

void Plteen::TrackPlane::reflow(float width, float height) {
    TheBigBang::reflow(width, height);

    this->move_to(this->variable, { width, 0.0F }, MatterPort::RT, { -8.0F, 8.0F });
}

void Plteen::TrackPlane::update(uint64_t interval, uint32_t count, uint64_t uptime) {
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

void Plteen::TrackPlane::on_mission_start(float width, float height) {
    this->run_bracers_at_random(false);
}

bool Plteen::TrackPlane::can_select(IMatter *m) {
    return isinstance(m, Citizen) || (this->agent == m);
}

bool Plteen::TrackPlane::can_select_multiple() {
    return is_shift_pressed();
}

void Plteen::TrackPlane::after_select(IMatter *m, bool yes) {
    if (!yes) {
        if (isinstance(m, Citizen)) {
            if (!this->is_colliding_with_mouse(m)) {
                this->glide_to_mouse(gliding_duration, m, MatterPort::CC);
            }
        }
    }

    this->heading = m->get_heading(false);
}

void Plteen::TrackPlane::on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) {
    if (pressed) {
        switch (key) {
        case 'c': this->track->erase(); break;
        case 'r': this->run_bracers_at_random(true); break;
        case '8': this->run_bracers_in_8_ways(); break;
        case ' ': this->run_bracers_in_direction(); break;
        }
    }
}

bool Plteen::TrackPlane::update_tooltip(IMatter *m, float lx, float ly, float gx, float gy) {
    bool updated = false;

    if (isinstance(m, Citizen)) {
        this->tooltip->set_text("heading: %.02lf˚", m->get_heading(false));
        updated = true;
    }

    return updated;
}

void Plteen::TrackPlane::run_bracers_at_random(bool drawing) {
    IMatter* selected = this->find_next_selected_matter();

    if (selected == nullptr) {
        for (auto bracer : this->bracers) {
            this->glide_to_random_location(gliding_duration, bracer);
        }
    } else {
        do {
            if (isinstance(selected, Citizen)) {
                this->glide_to_random_location(gliding_duration, selected);
            }
            selected = this->find_next_selected_matter(selected);
        } while (selected != nullptr);
    }
}

void Plteen::TrackPlane::run_bracers_in_direction() {
    size_t selected = this->count_selected();
    float length = 72.0F;

    if (selected > 0) {
        this->glide(gliding_duration, nullptr, length);
    } else {
        for (auto bracer : this->bracers) {
            this->glide(gliding_duration, bracer, length);
        }
    }
}

void Plteen::TrackPlane::run_bracers_in_8_ways() {
    IMatter* selected = this->find_next_selected_matter();

    if (selected == nullptr) {
        for (size_t i = 0; i < this->bracers.size(); i ++) {
            this->run_bracer_in_8_ways(this->bracers[i], 3 + i, 3 + i, 16.0);
        }
    } else {
        do {
            if (isinstance(selected, Citizen)) {
                this->run_bracer_in_8_ways(selected, 6, 6, 16.0);
            }
            selected = this->find_next_selected_matter(selected);
        } while (selected != nullptr);
    }
}

void Plteen::TrackPlane::run_bracer_in_8_ways(IMatter* bracer, size_t sides, size_t rounds, double gapsize) {
    double meridian = double(rounds * gapsize);
    double rad = degrees_to_radians(360.0 / sides);
    double factor = 2.0 - 2.0 * flcos(rad); 
    double direction = bracer->get_heading();
    Position dot = this->get_matter_location(bracer, MatterPort::LT);
            
    this->set_pen_color(bracer, RGBA::HSV(random_uniform(0.0, 360.0)));

    for (size_t s = 0; s < sides; s ++) {
        this->pen_up(bracer);
        this->move_to(bracer, dot, MatterPort::LT); // moving doesn't change the heading
        this->pen_down(bracer);
        this->glide(gliding_duration, bracer, meridian);
        this->turn(bracer, rad, true);
    }

    while (meridian > 4.0) {
        double parallel = flsqrt(meridian * meridian * factor); 

        this->pen_up(bracer);
        this->set_pen_color(bracer, RGBA::HSV(random_uniform(0.0, 360.0)));
        this->move_to(bracer, dot, MatterPort::LT); // moving doesn't change the heading
        this->set_heading(bracer, direction, true);
        this->move(bracer, meridian);
        this->pen_down(bracer);
        this->turn(bracer, (pi - rad) * 0.5, true);

        for (size_t s = 0; s < sides; s ++) {
            this->turn(bracer, rad, true);
            this->glide(gliding_duration, bracer, parallel);
        }

        meridian -= gapsize;
    }

    this->move_to(bracer, dot, MatterPort::LT);
    this->stamp(bracer);
    this->pen_up(bracer);
}
