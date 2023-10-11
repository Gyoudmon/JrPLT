#include "layer.hpp"

using namespace WarGrey::STEM;

/*************************************************************************************************/
static float radius = 80.0F;
static double gliding_duration = 0.618;

/*************************************************************************************************/
void WarGrey::STEM::LayerPlane::construct(float width, float height) {
    this->style = make_highlight_dimension_style(24U, 4U, 0);
}

void WarGrey::STEM::LayerPlane::load(float width, float height) {
    TheBigBang::load(width, height);

    this->variable = this->insert(new Dimensionlet(this->style, "边形", "选中了"));
    this->variable->set_value(&this->n);

    for (int n = 3; n < 13; n++) {
        this->shapes.push_back(this->insert(new RegularPolygonlet(n, radius, -90.0F, random_uniform(0x333333U, 0xDDDDDDU))));
    }
}

void WarGrey::STEM::LayerPlane::reflow(float width, float height) {
    TheBigBang::reflow(width, height);

    this->move_to(this->variable, width, 0.0F, MatterAnchor::RT, -8.0F, 8.0F);
}

void WarGrey::STEM::LayerPlane::on_enter(IPlane* from) {
    this->agent->play("Greeting", 1);
    this->move_shapes_at_random();
}

bool WarGrey::STEM::LayerPlane::can_select(IMatter* m) {
    return isinstance(m, RegularPolygonlet) || (this->agent == m);
}

void WarGrey::STEM::LayerPlane::after_select(IMatter* m, bool yes) {
    RegularPolygonlet* polygon = dynamic_cast<RegularPolygonlet*>(m);

    if (polygon != nullptr) {
        if (!yes) {
            this->glide_to_mouse(gliding_duration, m, MatterAnchor::CC);
        } else {
            this->n = float(polygon->get_side_count());
        }
    }
}

void WarGrey::STEM::LayerPlane::on_tap_selected(IMatter* m, float x, float y) {
    if (is_shift_pressed()) {
        this->bring_forward(m);
    } else {
        this->send_backward(m);
    }
}

void WarGrey::STEM::LayerPlane::on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) {
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

void WarGrey::STEM::LayerPlane::move_shapes_at_random() {
    for (auto shape : this->shapes) {
        this->glide_to_random_location(gliding_duration, shape);
    }
}
