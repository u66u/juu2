#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#define HEAP_CAPACITY 1024 * 1024

typedef struct {
    uint32_t index;
    uint32_t gen;
} GenRef;

typedef struct {
    uint32_t gen;
    void* pointer;
} IndirectionSlot;

IndirectionSlot heap[HEAP_CAPACITY];
uint32_t free_indices[HEAP_CAPACITY];
uint32_t free_top = 0;

void runtime_init() {
    for (int i = 0; i < HEAP_CAPACITY; i++) {
        heap[i].gen = 0;
        heap[i].pointer = NULL;
        free_indices[i] = (HEAP_CAPACITY - 1) - i;
    }
    free_top = HEAP_CAPACITY;
}

GenRef juu_alloc(size_t size) {
    if (free_top == 0) {
        fprintf(stderr, "Download more ram\n");
        exit(1);
    }
    uint32_t idx = free_indices[--free_top];
    heap[idx].gen++; 
    heap[idx].pointer = malloc(size);
    return (GenRef){ .index = idx, .gen = heap[idx].gen };
}

void* juu_access(GenRef r) {
    if (r.index >= HEAP_CAPACITY) {
        fprintf(stderr, "Runtime Error: Index out of bounds\n");
        exit(1);
    }
    if (heap[r.index].gen != r.gen) {
        fprintf(stderr, "Runtime Error: Use after free! (obj gen: %d, ref gen: %d)\n", heap[r.index].gen, r.gen);
        exit(1);
    }
    return heap[r.index].pointer;
}
// --- Generated Code ---

typedef struct Point Point;
typedef struct Gen_Int Gen_Int;

struct Point {
    int x;
    int y;
};

struct Gen_Int {
    int internal;
};

void user_main();
GenRef new_Point(int x, int y);
GenRef new_Gen_Int(int internal);
int Point__add(GenRef obj);
char* Gen__bark_Int(GenRef obj);

void user_main() {
    int xd = 5;
    GenRef g = new_Gen_Int(5);
    Gen__bark_Int(g);
    GenRef a = new_Point(5, 5);
    Point__add(a);
}

GenRef new_Point(int x, int y) {
    GenRef r = juu_alloc(sizeof(Point));
    ((Point*)juu_access(r))->x = x;
    ((Point*)juu_access(r))->y = y;
    return r;
}

GenRef new_Gen_Int(int internal) {
    GenRef r = juu_alloc(sizeof(Gen_Int));
    ((Gen_Int*)juu_access(r))->internal = internal;
    return r;
}

int Point__add(GenRef obj) {
    return (((Point*)juu_access(obj))->x + ((Point*)juu_access(obj))->y);
}

char* Gen__bark_Int(GenRef obj) {
    return "bark";
}

int main() {
    runtime_init();
    user_main();
    return 0;
}
