#include "../digitama/gydm_stem/game.hpp"

using namespace WarGrey::STEM;

/*************************************************************************************************/
namespace {
    class SplashPlane : public Plane {
    public:
        SplashPlane(Cosmos* master) : Plane("Blank!"), master(master) {}

    public:  // 覆盖游戏基本方法
        void load(float width, float height) override {
            this->title = this->insert(new Labellet(GameFont::Title(), BLACK, "%s", this->name()));
            
            this->agent = this->insert(new Linkmon());
            this->agent->scale(-1.0F, 1.0F);
            
            this->message = this->insert(new Labellet(GameFont::fangsong(), DIMGRAY, "%s", this->name()));
            this->ime_msg = this->insert(new Labellet(GameFont::fangsong(), DIMGRAY, "%s", this->name()));
        }
        
        void reflow(float width, float height) override {
            this->move_to(this->title, this->agent, MatterAnchor::RB, MatterAnchor::LB);
            this->move_to(this->message, this->agent, MatterAnchor::LB, MatterAnchor::LT);
            this->move_to(this->ime_msg, width, height, MatterAnchor::RB);
        }

        void update(uint64_t count, uint32_t interval, uint64_t uptime) override {}

        void on_enter(IPlane* from) override {
            this->agent->play("Greeting", 1);
        }

    public:
        bool can_select(IMatter* m) override {
            return true;
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
        Linkmon* agent;
        Labellet* title;
        Labellet* message;
        Labellet* ime_msg;
        
    private:
        Cosmos* master;
    };

    class BlankCosmos : public Cosmos {
    public:
        BlankCosmos(const char* process_path) : Cosmos(60) {
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

        virtual ~BlankCosmos() {
            imgdb_teardown();
        }

    public:  // 覆盖游戏基本方法
        void construct(int argc, char* argv[]) override {
            this->set_window_size(400, 300);
            GameFont::fontsize(21);

            this->push_plane(new SplashPlane(this));
        }
    };
}

/*************************************************************************************************/
int main(int argc, char* args[]) {
    BlankCosmos universe(args[argc]);

    universe.construct(argc, args);
    universe.big_bang();

    return 0;
}
