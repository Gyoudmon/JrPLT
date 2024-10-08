#pragma once // 确保只被 include 一次

#include <plteen/game.hpp>

#include <fstream>
#include <deque>

#include "plt.hpp"

namespace WarGrey::PLT {
    /******************************************* 声明游戏物体 ******************************************/
    class __lambda__ StreamSprite : public Plteen::Sprite {
    public:
        StreamSprite(const char* action, float width = 0.0F, float ratio = 3.0F);

    public:
        void construct(Plteen::dc_t* dc) override;

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
        bool can_select(Plteen::IMatter* m) override;
        void after_select(Plteen::IMatter* m, bool yes) override;

    public:
        bool update_tooltip(Plteen::IMatter* m, float x, float y, float gx, float gy) override;

    protected:
        void on_mission_start(float width, float height) override;

    private:
        bool update_pipe_status(std::ifstream& in, Plteen::MarioVPipe* pipe, Plteen::MarioPipeColor closed_color);
        bool update_progress(std::ifstream& in, const char* message);
        void agent_rest();

    private: // 本游戏世界中的物体
        Plteen::MarioGroundAtlas* ground;
        Plteen::MarioGroundAtlas* underground;
        WarGrey::PLT::StreamSprite* char_port;
        WarGrey::PLT::StreamSprite* line_port;
        WarGrey::PLT::StreamSprite* char_fall;
        WarGrey::PLT::StreamSprite* line_fall;
        Plteen::Labellet* char_label;
        Plteen::Labellet* line_label;
        Plteen::Sprite* char_cloud;
        Plteen::Sprite* line_cloud;
        Plteen::Sprite* char_sign;
        Plteen::Sprite* line_sign;
        Plteen::Sprite* char_filter;
        Plteen::Sprite* line_filter;
        Plteen::MarioVPipe* char_pipe;
        Plteen::MarioVPipe* line_pipe;
        std::deque<Plteen::Labellet*> chars;
        std::deque<Plteen::Labellet*> line_chars;
        std::deque<Plteen::Labellet*> lines;

    private:
        std::string stream_source;
        float stream_source_size;
        std::string stream_buffer;
        std::ifstream charin;
        std::ifstream linein;
    };
}
