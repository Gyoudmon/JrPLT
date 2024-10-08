#pragma once // 确保只被 include 一次

#include "../../digitama/plteen/bang.hpp"

#include <vector>

namespace Plteen {
    /*********************************************************************************************/
    class PlotPlane : public Plteen::TheBigBang {
    public:
        PlotPlane() : TheBigBang("Plot Diagram") { this->the_name("Tamer"); }
        virtual ~PlotPlane() {}
        
    public:
        void load(float width, float height) override;
        void reflow(float width, float height) override;
        void update(uint64_t count, uint32_t interval, uint64_t uptime) override;
        
    private: /* 本世界中的物体 */
        Plteen::Historylet* history;

    private:
        uint64_t history_day = 0U;
    };
}
