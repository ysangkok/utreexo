#include <stdint.h>

typedef struct {
	__int128 first;
	__int128 second;
} leaf;
typedef struct {
	__int128 mini;
	uint64_t pos;
} minipos;
typedef struct {
	uint64_t num_leaves;
	uint8_t height;
	minipos* position_map; // actually just stored as a contiguous list...
	leaf* leaves;
	uint64_t leaves_size;
} forest;
