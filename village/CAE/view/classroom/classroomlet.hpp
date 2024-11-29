#include <plteen/game.hpp>

#include "../desklet.hpp"
#include "../studentlet.hpp"

/*************************************************************************************************/
namespace WarGrey::CAE {
    class ClassRoomlet : public Plteen::Plane {
    public:
        ClassRoomlet(std::string caein, std::string caeout) : Plane("ClassRoom") {}
        virtual ~ClassRoomlet() {}

    public:
        void load(float width, float height) override;
        void reflow(float width, float height) override;

    private:
        Plteen::Rectanglet* platform;
        std::vector<WarGrey::CAE::IDesk*> desks;
        std::map<uint64_t, WarGrey::CAE::StudentSprite*> students;
    };
}
