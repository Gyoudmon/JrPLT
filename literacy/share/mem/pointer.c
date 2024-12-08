#include <stdio.h>
#include <stdint.h>

/*************************************************************************************************/
#define define(type, name) type name; watch_variable(#name, #type, &name, "stack")
#define define_init(type, name, datum) type name = datum; watch_variable(#name, #type, &name, "stack")
#define define_const(type, name, datum) const type name = datum; watch_variable(#name, #type, &name, "stack")
#define define_bss(type, name) static type name; watch_variable(#name, #type, &name, "bss")
#define define_static(type, name, datum) static type name = datum; watch_variable(#name, #type, &name, "data")

static void take_null(const char* state) {}

#ifdef __racket__
typedef void (*watch_variable_t)(const char* name, const char* type, const void* address, const char* segment);
typedef void (*take_snapshot_t)(const char* reason);

static void watch_nothing(const char* name, const char* type, const void* address, const char* segement) {}

__ffi__ watch_variable_t watch_variable = watch_nothing;
__ffi__ take_snapshot_t  take_snapshot = take_null;

#else
#define take_snapshot take_null

static void watch_variable(const char* name, const char* type, const void* address, const char* segement) {
    printf("%s @%p [%s]\n", name, address, segement);
}
#endif

/*************************************************************************************************/
int main() {
    define_init(int, var, 20241206);
    int *ptr = &var;

    watch_variable("ptr", "uintptr_t", &ptr, "stack");
    take_snapshot("done");

    return 0;
}
