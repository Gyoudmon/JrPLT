#include "plt.hpp"

using namespace Plteen;
using namespace WarGrey::PLT;

/*************************************************************************************************/
bool WarGrey::PLT::ThePLTPlane::update_atlas_position_for_tooltip(IMatter* m, float x, float y) {
    bool updated = false;
    GridAtlas* atlas = dynamic_cast<GridAtlas*>(m);

    if (atlas != nullptr) {
        int row, col;

        atlas->map_tile_index(x, y, &row, &col);
        this->tooltip->set_text(" (%d, %d) ", row, col);
        updated = true;
    }

    return updated;
}
