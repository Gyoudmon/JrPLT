#pragma once // 确保只被 include 一次

#include "../stem.hpp"

#include <map>
#include <vector>

namespace WarGrey::STEM {
    enum class TCLMState { Play, Reset };

    struct BallGroup {
        size_t id;
        size_t count;
        uint32_t color;
    };

    // 所有参与抽奖的号码
    static std::vector<uint8_t> all_ball_numbers = {
        1, 2, 3,
        5, 6, 7, 8, 9, 10, 11, 12, 13,
        15, 16, 17, 18, 19, 20, 21, 22, 23,
        25, 26, 27, 28, 29, 30, 31, 32, 33,
        35, 36, 37,
        39, 40, 41, 42, 43 
    };

    static std::vector<WarGrey::STEM::BallGroup> all_ball_groups = {
        { 3, 5, 0xEE0000U },
        { 2, 3, 0x0000FFU },
        { 1, 1, 0x00AA00U }
    };

    /** 声明游戏宇宙 **/
    class __lambda__ LotteryPlane : public WarGrey::STEM::TheSTEMPlane {
    private:
        class Ballet;

    public:
        LotteryPlane(const std::vector<uint8_t>& numbers = all_ball_numbers,
                const std::vector<WarGrey::STEM::BallGroup>& groups = all_ball_groups,
                double fan_frequency = 4000.0);

        virtual ~LotteryPlane() noexcept {}

    public:    // 覆盖游戏基本方法
        void load(float width, float height) override;
        void reflow(float width, float height) override;
        void update(uint64_t count, uint32_t interval, uint64_t uptime) override;
        void on_mission_start(float width, float height) override;

    public:
        bool can_select(Plteen::IMatter* m) override;
            
    protected: // 覆盖输入事件处理方法
        void on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) override; // 处理键盘事件
        void on_tap(Plteen::IMatter* m, float x, float y) override;                  // 处理鼠标事件

    private:
        void load_balls(float width, float height);
        void load_winning_numbers(float width, float height);
        void reflow_winning_numbers(float width, float height);
        
    private:
        void prepare(const std::map<size_t, WarGrey::STEM::LotteryPlane::Ballet*>& balls);
        void sally(const std::map<size_t, WarGrey::STEM::LotteryPlane::Ballet*>& balls);
        void play();
        void reset();
        
    private:
        void switch_game_state(WarGrey::STEM::TCLMState new_state);
        void spot_ball(WarGrey::STEM::LotteryPlane::Ballet* ball, const Plteen::Dot& O, float apothem);
        void update_balls(const std::map<size_t, Ballet*>& balls, std::vector<Ballet*>& lucky_balls, const Plteen::Dot& O, float apothem, bool motion_only);
        bool select(const std::vector<WarGrey::STEM::LotteryPlane::Ballet*>& balls);
        bool pick(WarGrey::STEM::LotteryPlane::Ballet* ball);
        void apply_forces(WarGrey::STEM::LotteryPlane::Ballet* ball, const Plteen::Dot& O, float radius, bool no_fan);
        
    private: // 游戏操作
        std::vector<Plteen::Labellet*> winning_numbers;
        std::vector<Plteen::Labellet*> winning_labels;
        std::map<size_t, WarGrey::STEM::LotteryPlane::Ballet*> all_balls;
        std::map<size_t, WarGrey::STEM::LotteryPlane::Ballet*> win_balls;
        Plteen::IShapelet* machine;
        Plteen::IShapelet* window;
        Plteen::IShapelet* inlet;
        Plteen::IShapelet* outlet;
        Plteen::IShapelet* winning_slot;
        
    private: // 游戏状态
        WarGrey::STEM::TCLMState state = TCLMState::Reset;
        size_t current_winning_slot = 0;

    private:
        double fan_frequency;
        double picking_timestamp;
        
    private:
        std::vector<uint8_t> ball_numbers;
        std::vector<WarGrey::STEM::BallGroup> ball_groups;
        size_t ball_count;
    };
}
