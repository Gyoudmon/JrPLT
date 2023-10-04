#include "../digitama/gydm_stem/game.hpp"
#include "../digitama/gydm_stem/bang.hpp"

using namespace WarGrey::STEM;

/*************************************************************************************************/
namespace {
    class IMEPlane : public TheBigBang {
    public:
        IMEPlane(Cosmos* master) : TheBigBang("IME") {
            this->the_name("Tamer");
        }

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override {
            TheBigBang::load(width, height);
            
            this->message = this->insert(new Labellet(GameFont::fangsong(), DIMGRAY, "PRESS ANY KEY"));
            this->ime_msg = this->insert(new Labellet(GameFont::fangsong(), DIMGRAY, ""));
        }
        
        void reflow(float width, float height) override {
            TheBigBang::reflow(width, height);

            this->move_to(this->message, this->agent, MatterAnchor::LB, MatterAnchor::LT);
            this->move_to(this->ime_msg, width, height, MatterAnchor::RB);
        }

        void on_enter(IPlane* from) override {
            this->agent->play("Greeting", 1);
        }

    protected:
        void on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) override {
            if (pressed) {
                this->message->set_text(MatterAnchor::LT, "you pressed '%c'", key);
            }
        }

        void on_text(const char* text, size_t size, bool entire) override {
            this->ime_msg->set_text(MatterAnchor::RB, "%s", text);
        }

        void on_editing_text(const char* text, int pos, int span) override {
            this->ime_msg->set_text(MatterAnchor::RB, "%s", text);
        }

    private:
        Labellet* message;
        Labellet* ime_msg;
    };

    /*********************************************************************************************/
    class IMECosmos : public Cosmos {
    public:
        IMECosmos(const char* process_path) : Cosmos(60) {
            enter_digimon_zone(process_path);
            imgdb_setup(digimon_zonedir().append("stone"));

#ifdef __windows__
            digimon_appdata_setup("C:\\opt\\GYDMstem\\");
            digimon_mascot_setup("C:\\opt\\GYDMstem\\stone\\mascot");
#else
            digimon_appdata_setup("/opt/GYDMstem/");
            digimon_mascot_setup("/opt/GYDMstem/stone/mascot");
#endif
        }

        virtual ~IMECosmos() {
            imgdb_teardown();
        }

    public:  // 覆盖游戏基本方法
        void construct(int argc, char* argv[]) override {
            this->set_window_size(400, 300);
            GameFont::fontsize(21);

            this->push_plane(new IMEPlane(this));
        }
    };
}

/*************************************************************************************************/
int main(int argc, char* args[]) {
    IMECosmos universe(args[argc]);

    universe.construct(argc, args);
    universe.big_bang();

    return 0;
}
