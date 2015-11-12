#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {

  char s[100];

  sprintf(s, "Recent sprintf %s", argv[1]);

  system(s);
}

