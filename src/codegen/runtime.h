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