#include <stdlib.h>

void bar() {
  system("cat /etc/%s");
}

void foo () {
  bar();
}

int main() {
  foo();
}
