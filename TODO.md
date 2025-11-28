## Type system: algebrainc types with Hindley-Milner algo for inference + rust-like constraint checking (not as good ofc)

## Mem: gemerational references

### Upgrades:

- Escape Analysis (The "Low Hanging Fruit")

        The compiler looks at a variable: "Does this pointer ever leave the function?"

        No: Allocate it on the raw stack. No Generation ID. No Checks. Speed = C.

        Yes: Allocate as GenRef. (need an ast graph traversal)

        ~80% of variables become raw C pointers. 20% remain GenRefs.

- Generational arenas (zig)

        Instead of inferring regions for every single variable, group objects into explicit arenas.

        Every function creates a temporary LocalArena.

        Allocation: new Object() allocates into LocalArena by default (Fast, Bump pointer).

        Safety: The LocalArena has a "Open/Closed" generation state.

        If you return a pointer from the function, the compiler sees "Return type uses LocalArena."

        Logic Solver: "Error: LocalArena dies at return."

        Fix: User must explicitly write new(Heap) Object().

