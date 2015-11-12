#include <stdlib.h>

void bar(char **argv) {
  system(argv[1]);
}

void foo (char **argv) {
  bar(argv);
}

int main(int argc, char **argv) {
  foo(argv);
}
