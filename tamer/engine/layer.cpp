#include "layer.hpp"

using namespace GYDM;

/*************************************************************************************************/
static float radius = 80.0F;
static double gliding_duration = 0.618;

/*************************************************************************************************/
void GYDM::LayerPlane::load(float width, float height) {
    TheBigBang::load(width, height);

    for (int n = 3; n < 13; n++) {
        this->shapes.push_back(this->insert(new RegularPolygonlet(n, radius, -90.0F, random_uniform(0x333333U, 0xDDDDDDU))));
    }
}

void GYDM::LayerPlane::on_mission_start(float width, float height) {
    this->move_shapes_at_random();
}

bool GYDM::LayerPlane::can_select(IMatter* m) {
    return isinstance(m, RegularPolygonlet) || (this->agent == m);
}

void GYDM::LayerPlane::after_select(IMatter* m, bool yes) {
    if (!yes) {
        if (isinstance(m, RegularPolygonlet)) {
            if (!this->is_colliding_with_mouse(m)) {
                this->glide_to_mouse(gliding_duration, m, MatterAnchor::CC);
            }
        }
    }
}

void GYDM::LayerPlane::on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) {
    if (pressed) {
        switch (key) {
        case 'f': {
            IMatter* which = this->find_next_selected_matter();

            if (which != nullptr) {
                this->bring_to_front(which);
            }
        }; break;
        case 'b': {
            IMatter* which = this->find_next_selected_matter();

            if (which != nullptr) {
                this->send_to_back(which);
            }
        }; break;
        case 'r': this->move_shapes_at_random(); break;
        }
    }
}

void GYDM::LayerPlane::move_shapes_at_random() {
    for (auto shape : this->shapes) {
        this->glide_to_random_location(gliding_duration, shape);
    }
}
