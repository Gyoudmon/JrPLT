#include "../digitama/gydm_stem/game.hpp"
// #include <gydm_stem/game.hpp>

#include <vector>
#include <filesystem>

/*************************************************************************************************/
namespace WarGrey::STEM {
    class JrPlane : public Plane {
    public:
        JrPlane(Cosmos* master) : Plane("青少计算机科学"), master(master) {}

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override;
        void reflow(float width, float height) override;

        void update(uint64_t count, uint32_t interval, uint64_t uptime) override;
        void on_mission_start(float width, float height) override;

    public:
        bool can_select(WarGrey::STEM::IMatter* m) override;
        void on_tap(WarGrey::STEM::IMatter* m, float x, float y) override;

    protected:
        bool update_tooltip(WarGrey::STEM::IMatter* m, float local_x, float local_y, float global_x, float global_y) override;

    private:
        void load_for_demo(float width, float height);
        void load_for_plot(float width, float height);

        void reflow_demo(float width, float height);
        void reflow_plot(float width, float height);
    
    private:
        WarGrey::STEM::Linkmon* agent;
        WarGrey::STEM::Labellet* title;
        WarGrey::STEM::Labellet* tooltip;
        std::vector<Sprite*> coins;
        std::vector<Labellet*> names;
        WarGrey::STEM::Tuxmon* tux;

    private: // for lambda demos
        // WarGrey::STEM::ConveyerBeltlet* conveyer;

    private: // for the plot
        WarGrey::STEM::PlanetCuteAtlas* stage;
        WarGrey::STEM::Citizen* host;
        WarGrey::STEM::Citizen* wife;
        WarGrey::STEM::Citizen* concubine;
        WarGrey::STEM::Citizen* handsome;
        
    private:
        WarGrey::STEM::Cosmos* master;
        int target_plane = 0;
    };
}
