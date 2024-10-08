#ifdef _USE_EXTERNAL_INCLUDING
#include <plteen/game.hpp>
#else
#include "../digitama/plteen/game.hpp"
#endif

#include <vector>
#include <filesystem>

/*************************************************************************************************/
namespace Plteen {
    class JrPlane : public Plane {
    public:
        JrPlane(Cosmos* master) : Plane("青少计算机科学"), master(master) {}

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override;
        void reflow(float width, float height) override;

        void update(uint64_t count, uint32_t interval, uint64_t uptime) override;
        void on_mission_start(float width, float height) override;

    public:
        bool can_select(Plteen::IMatter* m) override;
        void on_tap(Plteen::IMatter* m, float x, float y) override;

    protected:
        bool update_tooltip(Plteen::IMatter* m, float local_x, float local_y, float global_x, float global_y) override;

    private:
        void load_for_demo(float width, float height);
        void load_for_plot(float width, float height);

        void reflow_demo(float width, float height);
        void reflow_plot(float width, float height);
    
    private:
        Plteen::Linkmon* agent;
        Plteen::Labellet* title;
        Plteen::Labellet* tooltip;
        std::vector<Sprite*> coins;
        std::vector<Labellet*> names;
        Plteen::Tuxmon* tux;

    private: // for lambda demos
        // Plteen::ConveyerBeltlet* conveyer;

    private: // for the plot
        Plteen::PlanetCuteAtlas* stage;
        Plteen::Citizen* host;
        Plteen::Citizen* wife;
        Plteen::Citizen* concubine;
        Plteen::Citizen* handsome;
        
    private:
        Plteen::Cosmos* master;
        int target_plane = 0;
    };
}
