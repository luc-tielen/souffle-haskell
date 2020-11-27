// List of defines, has to be same as for the actual C++ code being compiled.
// This means compiling as embedded Souffle and enabling all possible feature flags.
#define __EMBEDDED_SOUFFLE__
#define USE_LIBZ
#define USE_SQLITE
#define USE_NCURSES

// Actual include used to find all transitive includes.
#include "souffle/CompiledSouffle.h"

// Dummy program :)
int main(int, char**) { return 0; }
