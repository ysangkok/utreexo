#include <stddef.h>
#include "forest.h"

leaf init_leaf(__int128 first, __int128 second) {
	leaf l;
	l.first = first;
	l.second = second;
	return l;
}
minipos init_minipos(__int128 mini, uint64_t pos) {
	minipos l;
	l.mini = mini;
	l.pos = pos;
	return l;
}
minipos* index_posmap(minipos* a, uint64_t idx) {
	return &a[idx];
}
leaf* index_leaves(leaf* a, uint64_t idx) {
	return &a[idx];
}
uint64_t index_positions(uint64_t* a, size_t idx) {
  return a[idx];
}
