// runtime prototype for genrefs
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct {
    uint32_t index;
    uint32_t gen;
} GenRef;

typedef struct {
    uint32_t gen;
    void* data; 
} Slot;

typedef struct {
    Slot* slots;
    uint32_t capacity;
    uint32_t* free_indices; 
    uint32_t free_top;
} Allocator;

extern Allocator global_heap;

void init_allocator(Allocator* a, uint32_t cap);

GenRef gen_alloc(Allocator* a, size_t size);

static inline void* gen_access(Allocator* a, GenRef r) {
    
    if (r.index >= a->capacity) {
        fprintf(stderr, "Segfault: Index out of bounds\n");
        exit(1);
    }
    
    if (a->slots[r.index].gen != r.gen) {
        fprintf(stderr, "Segfault: Use After Free (Generation Mismatch)\n");
        exit(1);
    }
    
    return a->slots[r.index].data;
}

void gen_free(Allocator* a, GenRef r);