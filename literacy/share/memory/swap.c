#include <stdio.h>
#include <stdint.h>
#include <math.h>

/*************************************************************************************************/
#define define(type, name) type name; watch_variable(#name, #type, &name, "stack")

#ifdef __memory__
typedef void (*watch_variable_t)(const char* name, const char* type, const void* address, const char* segment);
typedef void (*take_snapshot_t)(const char* reason);

__ffi__ watch_variable_t watch_variable = NULL;
__ffi__ take_snapshot_t  take_snapshot = NULL;

#else
static void watch_variable(const char* name, const char* type, const void* address, const char* segement) {
    printf("%s @%p [%s]\n", name, address, segement);
}

static void take_snapshot(const char* state) {}
#endif

static const char sc;
static char s;
const char c;
char v;

static void _foo() {}
void bar() {}

/*************************************************************************************************/
__ffi__ int main() {
    static char fs;
    define(char, a);
    define(char, b);
    define(char, _t);

    watch_variable("sc", "char", &sc, "static");
    watch_variable("s", "char", &s, "static");
    watch_variable("fs", "char", &fs, "static");
    watch_variable("bar", "uintptr", &bar, "static");

    watch_variable("c", "char", &c, "global");
    watch_variable("v", "char", &v, "global");
    watch_variable("watch", "uintptr", &watch_variable, "global");
    watch_variable("snapshot", "uintptr", &take_snapshot, "global");
    watch_variable("foo", "uintptr", &_foo, "global");
    watch_variable("main", "uintptr", &main, "global");
   
    take_snapshot("defined");

    a = 32;
    b = 64;
    take_snapshot("initialized");
    
    _t = a;
    take_snapshot("a -> t");
    a = b;
    take_snapshot("b -> a");
    b = _t;
    take_snapshot("t -> b");

    return 0;
}
