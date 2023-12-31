#include "text.hpp"

using namespace GYDM;

/*************************************************************************************************/
static const double gliding_duration = 0.618;

static std::vector<std::string> predefined_texts = { "Sphinx", "ex", "0", "O", "em", "ch" };

/*************************************************************************************************/
void GYDM::TextPlane::construct(float width, float height) {
    this->style = make_highlight_dimension_style(24U, 8U, 4U, 0);
    this->style.label_xfraction = 1.0F;
    this->style.label_font = GameFont::monospace();
    this->style.number_font = this->style.label_font;
}

void GYDM::TextPlane::load(float width, float height) {
    TheBigBang::load(width, height);

    for (auto t : predefined_texts) {
        this->texts.push_back(
            this->insert(new Labellet(GameFont::Tooltip(FontSize::xx_large),
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

void GYDM::TextPlane::reflow(float width, float height) {
    TheBigBang::reflow(width, height);

    this->move_to(this->metrics[0], { this->agent, MatterAnchor::LB }, MatterAnchor::LT);
    for (int idx = 1; idx < this->metrics.size(); idx ++) {
        this->move_to(this->metrics[idx], { this->metrics[idx - 1], MatterAnchor::LB }, MatterAnchor::LT, { 0.0F, 2.0F });
    }
}

void GYDM::TextPlane::on_mission_start(float width, float height) {
    this->move_texts_at_random();
}

bool GYDM::TextPlane::can_select(IMatter *m) {
    return isinstance(m, Labellet) || (this->agent == m);
}

void GYDM::TextPlane::after_select(IMatter *m, bool yes) {
    if (yes) {
        auto lbl = dynamic_cast<Labellet*>(m);

        if (lbl != nullptr) {
            TextMetrics text_metrics = this->style.label_font->get_text_metrics(lbl->c_str());
            
            this->metrics[0]->set_value(text_metrics.width);
            this->metrics[1]->set_value(text_metrics.height);
            this->metrics[2]->set_value(text_metrics.ascent);
            this->metrics[3]->set_value(text_metrics.descent);
            this->metrics[4]->set_value(text_metrics.tspace);
            this->metrics[5]->set_value(text_metrics.rspace);
            this->metrics[6]->set_value(text_metrics.bspace);
            this->metrics[7]->set_value(text_metrics.lspace);
        }
    }
}

void GYDM::TextPlane::move_texts_at_random() {
    for (auto t : this->texts) {
        this->glide_to_random_location(gliding_duration, t);
    }
}
