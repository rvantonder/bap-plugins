#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {

  char s[100];
  char w[50];

  sprintf(w, "Other printf %s", argv[1]);

  sprintf(s, "Recent sprintf %s", argv[1]);

  system(w);
}

