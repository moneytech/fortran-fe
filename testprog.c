#include <stdio.h>

int factorial(int *);

int main(int argc, char ** argv)
{
  int i;
  i=argc; /* avoid unused msg */
  i=(int)argv; /* avoid unused msg */
  for (i=2; i<10; i++)
    {
      printf("factorial(%d) = %d\n", i, factorial(&i));
    }
  return 0;
}

