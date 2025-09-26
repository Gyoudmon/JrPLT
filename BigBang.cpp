#include <plteen/game.hpp>

#include "village/splash.hpp"

#include "village/pltmos/carry.hpp"
#include "village/pltmos/terminal.hpp"
#include "village/pltmos/stream.hpp"
#include "village/stemos/motion/lottery.hpp"
#include "village/stemos/schematics/optics/pinhole.hpp"
#include "village/stemos/schematics/optics/chromaticity.hpp"

using namespace Plteen;
using namespace WarGrey::PLT;
using namespace WarGrey::STEM;

/*************************************************************************************************/
namespace {
    enum class CmdlineOps { TopCount, GroupSize, _ };

    class JrCosmos : public Cosmos {
    public:
        JrCosmos(const char* process_path) : Cosmos(60) {
            enter_digimon_zone(process_path);
            imgdb_setup(digimon_zonedir().append("stone"));
            
#ifdef __windows__
            digimon_appdata_setup("C:\\opt\\JrPLT\\");
            digimon_mascot_setup("C:\\opt\\JrPLT\\stone\\mascot");
#else
            digimon_appdata_setup("/opt/JrPLT/");
            digimon_mascot_setup("/opt/JrPLT/stone/mascot");
#endif
        }

        virtual ~JrCosmos() {
            imgdb_teardown();
        }

    public:  // 覆盖游戏基本方法
        void construct(int argc, char* argv[]) override {
            GameFont::fontsize(21);
            this->parse_cmdline_options(argc, argv);

#ifdef NDEBUG
            this->toggle_window_fullscreen();
#else
            this->set_window_size(0, 0);
#endif

            this->splash = this->push_plane(new JrPlane(this));

            this->push_plane(new TerminalPlane());
            this->push_plane(new DotAndCarryOnePlane());
            this->push_plane(new StreamPlane());
            this->push_plane(new LotteryPlane());
            this->push_plane(new PinholePlane());
            this->push_plane(new ChromaticityDiagramPlane());
        }

    protected:
        void update(uint64_t count, uint32_t interval, uint64_t uptime) override {
            if (this->has_current_mission_completed()) {
                this->transfer_to_plane(0);
            }
        }

        bool can_exit() override {
            return this->splash->has_mission_completed();
        }

    private:
        void parse_cmdline_options(int argc, char* argv[]) {}

    private:
        IPlane* splash;
    };
}

/*************************************************************************************************/
int main(int argc, char* args[]) {
    JrCosmos universe(args[argc]);

    universe.construct(argc, args);
    universe.big_bang();

    return 0;
}
