#include "libutreexo.h"

int main() {
    forest* f = NULL;
    leaf leaf_list[3] = { { .first = 0x0000000000000001, .second = 0x0000000000000000 },
                          { .first = 0x0000000000000002, .second = 0x0000000000000000 },
                          { .first = 0x0000000000000003, .second = 0x0000000000000000 },
                        };
    forest* new_forest = cForestAdd(f, leaf_list, 3);
    if (!new_forest) abort();
    cForestPrint(new_forest);
    cForestFree(new_forest);
}
