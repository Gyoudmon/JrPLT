#pragma once

#include "tamer.hpp"

#include <vector>

/*************************************************************************************************/
namespace Plteen {
    class TrackPlane : public Plteen::TheTamerBang {
    public:
        TrackPlane() : TheBigBang("Track") {}

    public: // 覆盖游戏基本方法
        void construct(float width, float height) override;
        void load(float width, float height) override;
        void reflow(float width, float height) override;
        void update(uint64_t interval, uint32_t count, uint64_t uptime) override;

    public:
        bool can_select(IMatter *m) override;
        bool can_select_multiple() override;

    protected:
        void on_mission_start(float width, float height) override;
        void after_select(IMatter *m, bool yes) override;
        void on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) override;

    protected:
        bool update_tooltip(IMatter *m, float lx, float ly, float gx, float gy) override;

    private:
        void run_bracers_at_random(bool drawing);
        void run_bracers_in_direction();
        void run_bracers_in_8_ways();
        void run_bracer_in_8_ways(IMatter* bracer, size_t sides, size_t rounds, double gapsize);

    private:
        std::vector<Bracer*> bracers;
        Dimensionlet* variable;
        Tracklet* track;

    private:
        DimensionStyle style;
        double heading;
    };
}
