#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main(int argc, char** argv)
{
  if (argc != 2)
  {
    return 1;
  }

  char output[32];
  char* input = argv[1];
  int k = 0;
  int l = strlen(input);

  for (int i = 0; i <= l; i++)
  {
    if (i == l || isspace(input[i]))
    {
      if (k > 0)
      {
        output[k] = '\0';
        k = 0;
        printf("%s\n", output);
      }
    }
    else
    {
      output[k] = input[i];
      k++;
    }
  }
  
  return 0;
}
