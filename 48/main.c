#include <math.h>
#include <stdio.h>

int main () {
  int n = 1000;
  int digits = 10;

  unsigned long mod_val = (unsigned long) pow(10, digits);

  int ii, jj;
  unsigned long out = 1;

  for (ii = 2; ii <= n; ++ii) {
    unsigned long this_pow = 1;
    for (jj = 0; jj < ii; ++jj) {
      this_pow *= ii;
      this_pow = this_pow % mod_val;
    }

    out += this_pow;
    out = out % mod_val;
  }

  printf("%lu %lu\n", mod_val, out);

  return 0;
}
