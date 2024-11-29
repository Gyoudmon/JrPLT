#pragma once // 确保只被 include 一次

#include <plteen/game.hpp>
#include <vector>
#include <map>

#include "plt.hpp"

namespace WarGrey::PLT {
    static const size_t GROWS = 10;
    static const size_t GCOLS = 16;
    static const size_t SLOTS = 8;

    /*********************************************************************************************/
    class __lambda__ CarrySystemPlane : public WarGrey::PLT::ThePLTPlane, public Plteen::TextFacilityPlane {
    public:
        CarrySystemPlane(size_t num = 0);
        virtual ~CarrySystemPlane() noexcept {}

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override;
        void reflow(float width, float height) override;
        void update(uint64_t count, uint32_t interval, uint64_t uptime) override;

    public:
        bool can_select(Plteen::IMatter* m) override;

    public:
        bool update_tooltip(Plteen::IMatter* m, float x, float y, float gx, float gy) override;

    protected:
        void on_mission_start(float width, float height) override { }
        void on_facility_command(char cmd) override;

    private: // 本游戏世界中的物体
        Plteen::RoundedRectanglet* slots[SLOTS];
        Plteen::Labellet* digits[SLOTS];
        std::vector<Plteen::Animal*> animals;

    private:
        char mode = '\0';
    };
}
