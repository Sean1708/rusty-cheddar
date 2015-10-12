#include "general.h"

int main() {
    Person pete = Person_create((int8_t)18, 117.6, 69.2);
    Eye blue = Blue;
    Person_describe(pete, blue);
}
