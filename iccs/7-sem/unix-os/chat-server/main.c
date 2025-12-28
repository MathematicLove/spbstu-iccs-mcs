#include "server.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
  int port = 3425;

  if (argc > 1)
  {
    port = atoi(argv[1]);
    if (port <= 0 || port > 65535)
    {
      fprintf(stderr, "Invalid port: %s\n", argv[1]);
      fprintf(stderr, "Oops! must be between 1 and 65535\n");
      return 1;
    }
  }

  printf("Я сказала стартуем! %d...\n", port);

  chat_server(port);

  return 0;
}
