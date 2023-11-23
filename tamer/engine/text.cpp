#include "text.hpp"

using namespace WarGrey::STEM;

/*************************************************************************************************/
static const double gliding_duration = 0.618;

static std::vector<std::string> predefined_texts = { "Sphinx", "x", "0", "O", "em", "ch" };

/*************************************************************************************************/
void WarGrey::STEM::TextPlane::construct(float width, float height) {
    this->the_name("Tamer");

    this->style = make_highlight_dimension_style(24U, 8U, 4U, 0);
    this->style.label_font = GameFont::monospace();
    this->style.number_font = this->style.label_font;
}

void WarGrey::STEM::TextPlane::load(float width, float height) {
    TheBigBang::load(width, height);

    for (auto t : predefined_texts) {
        this->texts.push_back(this->insert(new Labellet(GameFont::Tooltip(FontSize::xx_large),
                                            ROYALBLUE, "%s", t.c_str())));
    }

    this->metrics.push_back(this->insert(new Dimensionlet(this->style, "pt", "Width")));
    this->metrics.push_back(this->insert(new Dimensionlet(this->style, "pt", "Height")));
    this->metrics.push_back(this->insert(new Dimensionlet(this->style, "pt", "Ascent")));
    this->metrics.push_back(this->insert(new Dimensionlet(this->style, "pt", "Descent")));
    this->metrics.push_back(this->insert(new Dimensionlet(this->style, "pt", "TSpace")));
    this->metrics.push_back(this->insert(new Dimensionlet(this->style, "pt", "RSpace")));
    this->metrics.push_back(this->insert(new Dimensionlet(this->style, "pt", "BSpace")));
    this->metrics.push_back(this->insert(new Dimensionlet(this->style, "pt", "LSpace")));
}

void WarGrey::STEM::TextPlane::reflow(float width, float height) {
    TheBigBang::reflow(width, height);

    this->move_to(this->metrics[0], this->agent, MatterAnchor::LB, MatterAnchor::LT);
    for (int idx = 1; idx < this->metrics.size(); idx ++) {
        this->move_to(this->metrics[idx], this->metrics[idx - 1], MatterAnchor::LB, MatterAnchor::LT, 0.0F, 2.0F);
    }
}

void WarGrey::STEM::TextPlane::on_mission_start(float width, float height) {
    this->move_texts_at_random();
}

bool WarGrey::STEM::TextPlane::can_select(IMatter *m) {
    return isinstance(m, Labellet) || (this->agent == m);
}

void WarGrey::STEM::TextPlane::after_select(IMatter *m, bool yes) {
    if (yes) {
        auto lbl = dynamic_cast<Labellet*>(m);

        if (lbl != nullptr) {
            this->style.label_font->feed_text_extent(lbl->c_str(), &this->text_metrics);
            
            this->metrics[0]->set_value(this->text_metrics.width);
            this->metrics[1]->set_value(this->text_metrics.height);
            this->metrics[2]->set_value(this->text_metrics.ascent);
            this->metrics[3]->set_value(this->text_metrics.descent);
            this->metrics[4]->set_value(this->text_metrics.tspace);
            this->metrics[5]->set_value(this->text_metrics.rspace);
            this->metrics[6]->set_value(this->text_metrics.bspace);
            this->metrics[7]->set_value(this->text_metrics.lspace);
        }
    }
}

void WarGrey::STEM::TextPlane::move_texts_at_random() {
    for (auto t : this->texts) {
        this->glide_to_random_location(gliding_duration, t);
    }
}
