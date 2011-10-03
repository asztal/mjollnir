#include <stdint.h>

typedef uintptr_t Value;

#define makeWord(x) (((x)<<1)|1)

Value makeReal(double);
Value makeString(const char*);
const static Value nil = 0;
