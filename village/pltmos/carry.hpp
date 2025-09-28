#pragma once // 确保只被 include 一次

#include <plteen/game.hpp>
#include <vector>

#include "plt.hpp"

namespace WarGrey::PLT {
    static const size_t SLOTS = 16;

    /*********************************************************************************************/
    class __lambda__ DotAndCarryOnePlane : public WarGrey::PLT::ThePLTPlane, public Plteen::CmdletPlane {
    public:
        DotAndCarryOnePlane(size_t num = 0);
        virtual ~DotAndCarryOnePlane() noexcept {}

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override;
        void reflow(float width, float height) override;
        void on_mission_start(float width, float height) override;
        void on_mission_complete() override;

    protected:
        void on_cmdlet(size_t idx, char cmd, const std::string& name, float width, float height) override;
        void on_motion_complete(Plteen::IMatter* m, float x, float y, double xspd, double yspd) override;

    private:
        void on_new_counting_round(char key, float width, float height);
        void graze(float width, float height);
        void counting(size_t idx, int row, int col, double duration);

    private: // 本游戏世界中的物体
        Plteen::Labellet* digits[SLOTS];
        Plteen::Labellet* expts[SLOTS];
        Plteen::Labellet* bases[SLOTS];
        Plteen::Labellet* counter;
        std::vector<Plteen::Animal*> animals;
        Plteen::PlanetCuteAtlas* stage;

    private:
        size_t seq = 0;
        size_t slots = 0;
        uint16_t radix = 0;
    };
}
