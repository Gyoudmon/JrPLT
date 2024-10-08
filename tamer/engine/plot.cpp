#include "plot.hpp"

using namespace Plteen;

/*************************************************************************************************/
void Plteen::PlotPlane::load(float width, float height) {
    TheBigBang::load(width, height);
    
    this->history = this->insert(new Historylet(400.0F, 300.0F, ROYALBLUE));
    this->set_local_fps(8);
}

void Plteen::PlotPlane::reflow(float width, float height) {
    TheBigBang::reflow(width, height);
    
    this->move_to(this->history, { width * 0.5F, height * 0.5F }, MatterPort::CC);
}

void Plteen::PlotPlane::update(uint64_t count, uint32_t interval, uint64_t uptime) {
    this->history_day ++;

    this->history->push_back_datum(float(this->history_day), random_uniform(1.0F, float(this->history_day)));
}
