#ifdef _USE_EXTERNAL_INCLUDING
#include <gydm/game.hpp>
#else
#include "../digitama/gydm/game.hpp"
#endif

#include <vector>
#include <filesystem>

/*************************************************************************************************/
namespace GYDM {
    class JrPlane : public Plane {
    public:
        JrPlane(Cosmos* master) : Plane("青少计算机科学"), master(master) {}

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override;
        void reflow(float width, float height) override;

        void update(uint64_t count, uint32_t interval, uint64_t uptime) override;
        void on_mission_start(float width, float height) override;

    public:
        bool can_select(GYDM::IMatter* m) override;
        void on_tap(GYDM::IMatter* m, float x, float y) override;

    protected:
        bool update_tooltip(GYDM::IMatter* m, float local_x, float local_y, float global_x, float global_y) override;

    private:
        void load_for_demo(float width, float height);
        void load_for_plot(float width, float height);

        void reflow_demo(float width, float height);
        void reflow_plot(float width, float height);
    
    private:
        GYDM::Linkmon* agent;
        GYDM::Labellet* title;
        GYDM::Labellet* tooltip;
        std::vector<Sprite*> coins;
        std::vector<Labellet*> names;
        GYDM::Tuxmon* tux;

    private: // for lambda demos
        // GYDM::ConveyerBeltlet* conveyer;

    private: // for the plot
        GYDM::PlanetCuteAtlas* stage;
        GYDM::Citizen* host;
        GYDM::Citizen* wife;
        GYDM::Citizen* concubine;
        GYDM::Citizen* handsome;
        
    private:
        GYDM::Cosmos* master;
        int target_plane = 0;
    };
}
