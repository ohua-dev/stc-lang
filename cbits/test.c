#include <stdio.h>
#include "my_state.h"

int testfn(int a, int s, my_state * result) {
  printf( "hello world from c\n" );
  printf("input: %i\n", a);
  printf("state: %i\n", s);

  result->s1 = a;
  result->s2 = s;

  return 0;
}
