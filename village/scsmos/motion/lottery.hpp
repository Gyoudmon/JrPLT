#pragma once // 确保只被 include 一次

#include <gydm_stem/game.hpp>

#include "../scsm.hpp"

#include <map>
#include <vector>

namespace WarGrey::SCSM {
    enum class TCLMState { Play, Reset };
    enum class TCLMSubState { PlayRed, PlayBlue };
    enum class TCLMColor { Red, Blue };

    /** 声明游戏宇宙 **/
    class __lambda__ TwoColorLotteryPlane : public WarGrey::SCSM::TheSCSMPlane {
    private:
        class Ballet;

    public:
        TwoColorLotteryPlane(uint8_t red_top = 16U, uint8_t blue_top = 8U, size_t red_count = 6, size_t blue_count = 2, double fan_frequency = 6000.0)
            : TheSCSMPlane("双色球摇奖机"), fan_frequency(fan_frequency)
            ,red_top(red_top), blue_top(blue_top), red_count(red_count), blue_count(blue_count) {}

        virtual ~TwoColorLotteryPlane() noexcept {}

    public:    // 覆盖游戏基本方法
        void load(float width, float height) override;
        void reflow(float width, float height) override;
        void update(uint64_t count, uint32_t interval, uint64_t uptime) override;
        void on_mission_start(float width, float height) override;

    public:
        bool can_select(WarGrey::STEM::IMatter* m) override;
            
    protected: // 覆盖输入事件处理方法
        void on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) override; // 处理键盘事件
        void on_tap(WarGrey::STEM::IMatter* m, float x, float y) override;                  // 处理鼠标事件

    private:
        void load_balls(float width, float height);
        void load_winning_numbers(float width, float height);
        void load_tip(const char* label, size_t value, uint32_t label_bgc);
        void reflow_winning_numbers(float width, float height);
        
    private:
        void prepare(const std::map<size_t, WarGrey::SCSM::TwoColorLotteryPlane::Ballet*>& balls);
        void sally(const std::map<size_t, WarGrey::SCSM::TwoColorLotteryPlane::Ballet*>& balls);
        void hide(const std::map<size_t, WarGrey::SCSM::TwoColorLotteryPlane::Ballet*>& balls);
        void play();
        void reset();
        
    private:
        void switch_game_state(WarGrey::SCSM::TCLMState new_state);
        void spot_ball(WarGrey::SCSM::TwoColorLotteryPlane::Ballet* ball, float cx, float cy, float apothem);
        void update_balls(const std::map<size_t, Ballet*>& balls, std::vector<Ballet*>& lucky_balls, float cx, float cy, float apothem, bool motion_only);
        bool select(const std::vector<WarGrey::SCSM::TwoColorLotteryPlane::Ballet*>& balls);
        bool pick(WarGrey::SCSM::TwoColorLotteryPlane::Ballet* ball);
        void apply_forces(WarGrey::SCSM::TwoColorLotteryPlane::Ballet* ball, float cx, float cy, float radius, bool no_fan);
        
    private: // 游戏操作
        std::vector<WarGrey::STEM::Labellet*> winning_numbers;
        std::vector<WarGrey::STEM::Dimensionlet*> tips;
        std::map<size_t, WarGrey::SCSM::TwoColorLotteryPlane::Ballet*> red_balls;
        std::map<size_t, WarGrey::SCSM::TwoColorLotteryPlane::Ballet*> blue_balls;
        std::map<size_t, WarGrey::SCSM::TwoColorLotteryPlane::Ballet*> win_red_balls;
        std::map<size_t, WarGrey::SCSM::TwoColorLotteryPlane::Ballet*> win_blue_balls;
        WarGrey::STEM::IShapelet* machine;
        WarGrey::STEM::IShapelet* window;
        WarGrey::STEM::IShapelet* inlet;
        WarGrey::STEM::IShapelet* outlet;
        WarGrey::STEM::IShapelet* winning_slot;
        size_t current_winning_slot = 0;

    private: // 游戏状态
        WarGrey::SCSM::TCLMState state = TCLMState::Reset;
        WarGrey::SCSM::TCLMSubState substate = TCLMSubState::PlayRed;

    private:
        double fan_frequency;
        double picking_timestamp;

    private:
        uint8_t red_top;
        uint8_t blue_top;
        size_t red_count;
        size_t blue_count;
    };
}
