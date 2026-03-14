#include <unistd.h>
#include <stdio.h>

#define PYTHON3 "/usr/bin/python3"
#define DEFAULT_SCRIPT "/Users/practice2/bin/access.py"

#define MAX_ARGS 64
int main(int argc, char *argv[])
{
    const char *script = (argc >= 2) ? argv[1] : DEFAULT_SCRIPT;
    char *args[MAX_ARGS];
    int i, n = 0;
    args[n++] = "python3";
    args[n++] = (char *)script;
    for (i = 2; i < argc && n < MAX_ARGS - 1; i++)
        args[n++] = argv[i];
    args[n] = NULL;
    if (execv(PYTHON3, args) == -1)
    {
        perror("execv");
        return 1;
    }
    return 0;
}
