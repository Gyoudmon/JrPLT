#include "plot.hpp"

using namespace GYDM;

/*************************************************************************************************/
void GYDM::PlotPlane::load(float width, float height) {
    TheBigBang::load(width, height);
    
    this->history = this->insert(new Historylet(400.0F, 300.0F, ROYALBLUE));
    this->set_local_fps(8);
}

void GYDM::PlotPlane::reflow(float width, float height) {
    TheBigBang::reflow(width, height);
    
    this->move_to(this->history, { width * 0.5F, height * 0.5F }, MatterAnchor::CC);
}

void GYDM::PlotPlane::update(uint64_t count, uint32_t interval, uint64_t uptime) {
    this->history_day ++;

    this->history->push_back_datum(double(this->history_day), random_uniform(1.0, double(history_day)));
}
