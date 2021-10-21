#include <stdio.h>

int isPrime(int n)
{
  int m;
  int found_factor;

  found_factor = 0;

  for (m = 2; m <= n - 1; m++)
  {
    if (n % m == 0)
    {
      found_factor = 1;
      break;
    }
  }

  return !found_factor;
}

int main()
{
  int n;

  n = 37;

  printf("%d\n", isPrime(n));
  return 0;
}
