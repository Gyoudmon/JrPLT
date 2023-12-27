#pragma once // 确保只被 include 一次

#include <gydm/game.hpp>

#include <fstream>
#include <deque>

#include "plt.hpp"

namespace WarGrey::PLT {
    /******************************************* 声明游戏物体 ******************************************/
    class __lambda__ StreamSprite : public GYDM::Sprite {
    public:
        StreamSprite(const char* action, float width = 0.0F, float ratio = 3.0F);

    public:
        void construct(SDL_Renderer* renderer) override;

    public:
        void close();
        void open();
        void flow();
        void pause();

    private:
        std::string action;
        float width;
        float ratio;
    };
    
    /******************************************* 声明游戏世界 ******************************************/
    class __lambda__ StreamPlane : public WarGrey::PLT::ThePLTPlane {
    public:
        StreamPlane(const char* spath = "");
        virtual ~StreamPlane() {}

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override;
        void reflow(float width, float height) override;
        void update(uint64_t count, uint32_t interval, uint64_t uptime) override;

    public:
        bool can_select(GYDM::IMatter* m) override;
        void after_select(GYDM::IMatter* m, bool yes) override;

    public:
        bool update_tooltip(GYDM::IMatter* m, float x, float y, float gx, float gy) override;

    protected:
        void on_mission_start(float width, float height) override;

    private:
        bool update_pipe_status(std::ifstream& in, GYDM::MarioVPipe* pipe, GYDM::MarioPipeColor closed_color);
        bool update_progress(std::ifstream& in, const char* message);
        void agent_rest();

    private: // 本游戏世界中的物体
        GYDM::MarioGroundAtlas* ground;
        GYDM::MarioGroundAtlas* underground;
        WarGrey::PLT::StreamSprite* char_port;
        WarGrey::PLT::StreamSprite* line_port;
        WarGrey::PLT::StreamSprite* char_fall;
        WarGrey::PLT::StreamSprite* line_fall;
        GYDM::Labellet* char_label;
        GYDM::Labellet* line_label;
        GYDM::Sprite* char_cloud;
        GYDM::Sprite* line_cloud;
        GYDM::Sprite* char_sign;
        GYDM::Sprite* line_sign;
        GYDM::Sprite* char_filter;
        GYDM::Sprite* line_filter;
        GYDM::MarioVPipe* char_pipe;
        GYDM::MarioVPipe* line_pipe;
        std::deque<GYDM::Labellet*> chars;
        std::deque<GYDM::Labellet*> line_chars;
        std::deque<GYDM::Labellet*> lines;

    private:
        std::string stream_source;
        float stream_source_size;
        std::string stream_buffer;
        std::ifstream charin;
        std::ifstream linein;
    };
}
