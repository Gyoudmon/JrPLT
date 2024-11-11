#include "computer_desklet.hpp"

using namespace Plteen;
using namespace WarGrey::CAE;

/*************************************************************************************************/
void WarGrey::CAE::ComputerDesklet::draw(dc_t* dc, float x, float y, float Width, float Height) {
    RGBA pen =  this->get_pen_color();
    shared_font_t font = GameFont::monospace();
    float seat_height = Height / float(this->seat_count());
    float Y  = y + Height;
    float dx = float(font->descent());
    float dy = float(font->height()); 
    
    Rectanglet::draw(dc, x, y, Width, Height);

    for (size_t idx = 0; idx < this->seat_count(); idx ++) {
        float yself = Y - seat_height * float(idx);

        if (idx > 0) {
            dc->draw_hline(x, yself, Width, pen);
        }

        dc->draw_solid_text(std::to_string(idx + this->idx0), font, x + dx, yself - dy, pen);
    }
}

int WarGrey::CAE::ComputerDesklet::get_seat_by(float local_x, float local_y) {
    float height = this->get_bounding_box().height();
    float seat_height = height / float(this->seat_count()); 
    int idx = int(flfloor((height - local_y) / seat_height));

    return idx + this->idx0;
}

void WarGrey::CAE::ComputerDesklet::sit(ISprite* stu, int idx, double duration) {
    int a_idx = idx - this->idx0;

    if (a_idx >= 0) {
        auto master = this->master();
    
        if (master != nullptr) {
            float height = this->get_bounding_box().height();
            float seat_height = height / float(this->seat_count());
            cPoint O = master->get_matter_location(this, (this->get_index() % 2 == 1) ? MatterPort::LB : MatterPort::RB);
            
            master->glide_to(duration, stu,
                cPoint(0.0F, seat_height * (float(- a_idx) - 0.5F)) + O,
                (this->get_index() % 2 == 1) ? MatterPort::RC : MatterPort::LC,
                cO);

            master->glide(0.1, stu, { 0.0F, 1.0F });
        }
    }
}
