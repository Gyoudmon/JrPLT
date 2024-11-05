#include <plteen/game.hpp>

/*************************************************************************************************/
namespace WarGrey::CAE {
    class JrPLTCRCosmos : public Plteen::Cosmos {
    public:
        JrPLTCRCosmos() : Plteen::Cosmos(60) {}

    public:
        void construct(int argc, char* argv[]) override;

    private:
        void parse_commandline_argument(int argc, char* argv[]);

    private:
        std::string caein;
        std::string caeout;
    };
}
