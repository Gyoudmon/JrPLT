#include "chromaticity.hpp"

using namespace WarGrey::STEM;
using namespace WarGrey::SCSM;

/*************************************************************************************************/
static const size_t hue_count = 36U;
static const float hue_radius = 16.0F;
static const float wheel_radius = 360.0F;

static const float primary_radius = 64.0F;
static const float chromaticity_size = 380.0F;

/*************************************************************************************************/
void WarGrey::SCSM::ChromaticityDiagramPlane::load(float width, float height) {
    float delta_deg = 360.0F / float(hue_count);

    this->set_background(0x000000U);

    for (float deg = 0.0F; deg < 360.0F; deg += delta_deg) {
        this->hues.push_back(this->insert(new Circlet(hue_radius, RGBA::HSV(deg))));
    }

    this->primaries.push_back(this->insert(new Ellipselet(primary_radius, 0xFF0000U)));
    this->primaries.push_back(this->insert(new Ellipselet(primary_radius, 0x00FF00U)));
    this->primaries.push_back(this->insert(new Ellipselet(primary_radius, 0x0000FFU)));
    this->chroma_dia = this->insert(new Chromalet(chromaticity_size));

    for (auto com : this->primaries) {
        com->set_color_mixture(ColorMixture::Add);
    }

    TheBigBang::load(width, height);
}

void WarGrey::SCSM::ChromaticityDiagramPlane::reflow(float width, float height) {
    float cx = width * 0.5F;
    float cy = height * 0.55F;
    float x, y;
    
    TheBigBang::reflow(width, height);
    
    for (auto c : this->hues) {
        circle_point(wheel_radius, float(c->get_brush_color().hue()) - 90.0F, &x, &y, false);
        this->move_to(c, cx + x, cy + y, MatterAnchor::CC);
    }

    circle_point(wheel_radius, -90.0F, &x, &y, false);
    this->reflow_primaries(cx + x, cy + y + (primary_radius + hue_radius) * 1.618F);
    this->move_to(this->chroma_dia, width * 0.5F, height * 0.618F, MatterAnchor::CC);
}

void WarGrey::SCSM::ChromaticityDiagramPlane::update(uint64_t interval, uint32_t count, uint64_t uptime) {
    if (is_shift_pressed()) {
        this->chroma_dia->set_pseudo_primary_triangle_alpha(0.32);
    } else {
        this->chroma_dia->set_pseudo_primary_triangle_alpha(0.00);
    }
}

bool WarGrey::SCSM::ChromaticityDiagramPlane::can_select(IMatter* m) {
    return (dynamic_cast<Circlet*>(m) != nullptr)
            || (m == this->agent)
            || (m == this->chroma_dia);
}

void WarGrey::SCSM::ChromaticityDiagramPlane::after_select(IMatter* m, bool yes) {
    if (yes) {
        auto com = dynamic_cast<Circlet*>(m);

        if (com != nullptr) {
            RGBA pcolor = com->get_brush_color();

            this->primaries[this->selection_seq]->set_brush_color(pcolor);
            this->chroma_dia->set_pseudo_primary_color(pcolor, this->selection_seq);
            this->selection_seq = (this->selection_seq + 1) % this->primaries.size();
        } else if (m == this->chroma_dia) {
            switch (this->chroma_dia->get_standard()) {
            case CIE_Standard::Primary: this->chroma_dia->set_standard(CIE_Standard::D65); break;
            case CIE_Standard::D65: this->chroma_dia->set_standard(CIE_Standard::Primary); break;
            }

            this->no_selected();
        }
    }
}

bool WarGrey::SCSM::ChromaticityDiagramPlane::update_tooltip(IMatter* m, float x, float y, float gx, float gy) {
    bool updated = false;
    auto com = dynamic_cast<Circlet*>(m);
    auto cc = dynamic_cast<Ellipselet*>(m);

    if (com != nullptr) {
        RGBA brush = com->get_brush_color();

        this->tooltip->set_text(" #%06X [Hue: %.2f] ", brush.rgb(), brush.hue());
        this->tooltip->set_background_color(GHOSTWHITE);

        this->no_selected();
        updated = true;
    } else if (cc != nullptr) {
        RGBA c;

        for (size_t idx = 0; idx < this->primaries.size(); idx ++) {
            float cx, cy;
            
            this->feed_matter_location(this->primaries[idx], &cx, &cy, MatterAnchor::CC);

            if (point_distance(gx, gy, cx, cy) <= primary_radius) {
                c = c + this->primaries[idx]->get_brush_color();
            }
        }

        this->tooltip->set_text(" #%06X ", c.rgb());
        this->tooltip->set_background_color(GHOSTWHITE);
        updated = true;
    } else if (m == this->chroma_dia) {
        RGBA c = this->chroma_dia->get_color_at(x, y, is_shift_pressed());

        if (!c.is_black()) {
            double co_x, co_y;

            this->chroma_dia->feed_color_location(c, nullptr, nullptr, &co_x, &co_y);
            
            switch (this->chroma_dia->get_standard()) {
                case CIE_Standard::Primary: {
                    this->tooltip->set_text(" CIE Primary: %06X (%.3lf, %.3lf, %.3lf) ",
                        c.rgb(), co_x, co_y, 1.0 - co_x - co_y);
                 }; break;
                case CIE_Standard::D65: {
                    this->tooltip->set_text(" CIE sRGB-D65: %06X (%.3lf, %.3lf, %.3lf) ",
                        c.rgb(), co_x, co_y, 1.0 - co_x - co_y);
                 }; break;
            }

            this->tooltip->set_background_color(c);
            updated = true;
        }
    }

    return updated;
}

void WarGrey::SCSM::ChromaticityDiagramPlane::reflow_primaries(float x, float y) {
    float cc_off = primary_radius * 0.5F;
    
    this->move_to(this->primaries[0], x, y, MatterAnchor::CB, 0.0F, cc_off);
    this->move_to(this->primaries[1], this->primaries[0], MatterAnchor::CB, MatterAnchor::RC, cc_off);
    this->move_to(this->primaries[2], this->primaries[1], MatterAnchor::CC, MatterAnchor::LC);
}
