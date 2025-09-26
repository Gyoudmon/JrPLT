#pragma once // 确保只被 include 一次

#include <plteen/game.hpp>
#include <vector>

#include "plt.hpp"

namespace WarGrey::PLT {
    /*********************************************************************************************/
    class __lambda__ TerminalPlane : public WarGrey::PLT::ThePLTPlane, public Plteen::TextFacilityPlane {
    public:
        TerminalPlane(int row = 8, int col = 32);
        virtual ~TerminalPlane() noexcept;

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override;
        void reflow(float width, float height) override;
        void update(uint64_t count, uint32_t interval, uint64_t uptime) override;
        void on_mission_start(float width, float height) override;
        
    public:
        bool can_select(Plteen::IMatter* m) override;

    protected:
        void on_facility_command(size_t idx, char cmd, float width, float height) override;
        void on_motion_complete(Plteen::IMatter* m, float x, float y, double xspd, double yspd) override;
        void on_bubble_expired(Plteen::IMatter* m, Plteen::SpeechBubble type) override;

    private:
        void clear_screen();
        void display(const std::string& message);
        void linefeed();
        void try_scroll_screen();
        void clear_term_cell(int r, int c, int count);

    private: // 本游戏世界中的物体
        Plteen::Rectanglet* stage = nullptr;
        Plteen::Bracer* assistant = nullptr;
        Plteen::Labellet* cursor = nullptr;
        Plteen::Labellet*** chars = nullptr;

    private:
        int term_row = 0;
        int term_col = 0;
        int term_ridx = 0;
        int term_cidx = 0;
    };
}
