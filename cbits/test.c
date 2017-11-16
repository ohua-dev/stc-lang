#include <stdio.h>
#include "my_state.h"

int testfn(int a, int s, my_state * result) {
  printf( "hello world from c\n" );
  printf("input: %i\n", a);
  printf("state: %i\n", s);

  result->val = a;
  result->state = s;

  return 0;
}

int f1(int a, int s, my_state * result) {
  printf( "calling f1 in c\n" );
  result->val = a + 2;
  result->state = s + 2;
  return 0;
}

int f2(int a, int s, my_state * result) {
  printf( "calling f2 in c\n" );
  result->val = a * 3;
  result->state = s + 3;
  return 0;
}

int f3(int a, int s, my_state * result) {
  printf( "calling f3 in c\n" );
  result->val = a;
  result->state = s + 5;
  return 0;
}
