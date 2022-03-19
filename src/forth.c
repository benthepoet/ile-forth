#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int data_stack[127];
char data_sp = -1;

void handle_token(char* buffer, int endpos, int alpha);

int main(int argc, char** argv)
{
  if (argc != 2)
  {
    printf("Input string was not supplied.\n");
    return 1;
  }

  char buffer[32];
  char* input = argv[1];
  int length = strlen(input);
  int k = 0;
  int alpha = 0;
  for (int i = 0; i < length; i++)
  {
    char c = input[i];
    if (c == ' ')
    {
      handle_token(buffer, k, alpha);
      alpha = k = 0;
    }
    else
    {
      if (c < '0' || c > '9')
      {
        alpha = 1;
      }
      
      buffer[k] = c;
      if (i == length - 1)
      {
        handle_token(buffer, k + 1, alpha);
      }
      else
      {
        k++;
      }
    }
  }

  printf("Dumping stack\n");
  for (int i = data_sp; i > -1; i--)
  {
    printf("%d\n", data_stack[i]);
  }
  
  return 0;
}

void handle_token(char* buffer, int endpos, int alpha)
{
  buffer[endpos] = '\0';

  if (alpha == 0)
  {
    data_stack[++data_sp] = strtol(buffer, NULL, 10);
  }
  
  printf("%s - %d\n", buffer, alpha);
}
