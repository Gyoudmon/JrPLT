#pragma once // 确保只被 include 一次

#include <plteen/game.hpp>
#include <vector>

#include "plt.hpp"
#include "console/ascii/art.hpp"

namespace WarGrey::PLT {
    /*********************************************************************************************/
    class __lambda__ ASCIIArtPlane : public WarGrey::PLT::ThePLTPlane, public Plteen::CmdletPlane {
    public:
        ASCIIArtPlane(int row = 10, int col = 20);
        virtual ~ASCIIArtPlane() noexcept;

    public:
        void load(float width, float height) override;
        void reflow(float width, float height) override;
        void update(uint64_t count, uint32_t interval, uint64_t uptime) override;
        void on_mission_start(float width, float height) override;

    protected:
        void on_cmdlet(size_t idx, char cmd, const std::string& name, float width, float height) override;
        void on_motion_complete(Plteen::IMatter* m, float x, float y, double xspd, double yspd) override;
        void on_bubble_expired(Plteen::IMatter* m, Plteen::SpeechBubble type) override;
        void draw_background(Plteen::dc_t* dc, float X, float Y, float Width, float Height) override;

    public:
        void sync_cursor();
        void linefeed(bool sync = true);
        void display(const std::string& message, bool sync_cursor = true);
        void clear_screen();
        
    private:
        void try_scroll_screen();
        void clear_term_cell(int r, int c, int count);

    private:
        void display_shape(WarGrey::PLT::ASCIIArt* algo, const std::string& name);
        void scan_done();

    private:
        Plteen::Rectanglet* scanline = nullptr;
        Plteen::Bracer* assistant = nullptr;
        Plteen::Labellet* modinfo = nullptr;
        Plteen::Labellet* posinfo = nullptr;
        Plteen::Labellet* spaninfo = nullptr;
        Plteen::Labellet* cursor = nullptr;
        Plteen::Labellet*** chars = nullptr;

    private:
        bool scanline_extending = false;
        WarGrey::PLT::ASCIIArt* algorithm = nullptr;
        std::vector<SDL_FPoint> shape_dots;
		Plteen::shared_texture_t true_shape = nullptr;
        Plteen::Dot last_scanline_pos = { 0.0F, 0.0F };

    private:
        int term_row = 0;
        int term_col = 0;
        int term_ridx = 0;
        int term_cidx = 0;
    };
}
