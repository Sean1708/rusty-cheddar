#include "general.h"

int main() {
    Person pete = Person_create((int8_t)18, Blue, 117.6, 69.2);
    Person_describe(pete);
}
