#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {

  char s[100];

  sprintf(s, "WRONG not most recent %s", argv[1]);
  sprintf(s, "most recent %s", argv[1]);

  system(s);
}

