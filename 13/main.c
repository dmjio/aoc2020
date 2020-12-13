#include <stdio.h>

/* try it in C */

int allDivisible (int busses[], int offset[], int t) {
  for (int i = 0; i < 10; i++) {
    if ((t + offset[i]) % busses[i] != 0) {
      return 0;
    }
  }
  return 1;
}

int findEarly (int busses[], int offset[], int t) {
  printf ("t = %d\n", t);
  if (allDivisible (busses, offset, t) == 1) {
    return t;
  }
  return findEarly (busses,offset,t+busses[0]);
}

int main () {
  int busses[] = { 41,37,557,29,13,17,23,419,19 };
  int offset[] = { 0,35,41,43,54,58,64,72,91 };
  /* int busses[] = { 17,13,19 }; */
  /* int offset[] = {  }; */
  int x = findEarly (busses, offset, 0);
  printf ("found: %d\n", x);
}
