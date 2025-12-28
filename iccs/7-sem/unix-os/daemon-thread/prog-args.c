#include "prog-args.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <strings.h>
#include <ctype.h>
#include <limits.h>
#include <unistd.h>

static unsigned int parse_log_level(const char *s)
{
    if (!s)
        return LOG_NOTICE;
    if (strcasecmp(s, "debug") == 0)
        return LOG_DEBUG;
    if (strcasecmp(s, "info") == 0)
        return LOG_INFO;
    if (strcasecmp(s, "notice") == 0)
        return LOG_NOTICE;
    if (strcasecmp(s, "warning") == 0)
        return LOG_WARNING;
    if (strcasecmp(s, "err") == 0 || strcasecmp(s, "error") == 0)
        return LOG_ERR;
    char *end = NULL;
    unsigned long v = strtoul(s, &end, 10);
    if (end != s && *end == '\0')
        return (unsigned int)v;
    return LOG_NOTICE;
}

static char *make_absolute_path(const char *path)
{
    if (!path)
        return NULL;

    if (path[0] == '/')
    {
        return strdup(path);
    }

    char cwd[PATH_MAX];
    if (getcwd(cwd, sizeof(cwd)) == NULL)
    {
        return NULL;
    }

    char *absolute_path = malloc(PATH_MAX);
    if (!absolute_path)
        return NULL;

    snprintf(absolute_path, PATH_MAX, "%s/%s", cwd, path);
    return absolute_path;
}

void prog_print_help(const char *progname)
{
    fprintf(stderr,
            "Usage: %s [OPTIONS]\n"
            "  -h, --help            Show this help message\n"
            "  -d, --daemonize       Daemonize (double-fork)\n"
            "  -c, --config FILE     Config file (default: ./config.txt)\n"
            "  -l, --log-upto LEVEL  Syslog level (debug,info,notice,warning,err or numeric)\n",
            progname ? progname : "daemon");
}

struct prog_args prog_parse(int argc, char *argv[])
{
    struct prog_args pa;
    pa.progname = (argc > 0 ? argv[0] : "daemon");
    pa.help = false;
    pa.daemonize = false;
    pa.log_upto = LOG_NOTICE;
    pa.config_file = "./config.txt";
    pa.config_file_allocated = NULL;

    for (int i = 1; i < argc; ++i)
    {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0)
        {
            pa.help = true;
            continue;
        }
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--daemonize") == 0)
        {
            pa.daemonize = true;
            continue;
        }
        if ((strcmp(argv[i], "-c") == 0 || strcmp(argv[i], "--config") == 0) && i + 1 < argc)
        {
            pa.config_file_allocated = make_absolute_path(argv[++i]);
            if (pa.config_file_allocated)
            {
                pa.config_file = pa.config_file_allocated;
            }
            else
            {
                pa.config_file = argv[i];
            }
            continue;
        }
        if ((strcmp(argv[i], "-l") == 0 || strcmp(argv[i], "--log-upto") == 0) && i + 1 < argc)
        {
            pa.log_upto = parse_log_level(argv[++i]);
            continue;
        }
        fprintf(stderr, "Unknown option: %s\n", argv[i]);
        prog_print_help(pa.progname);
        exit(EXIT_FAILURE);
    }

    if (!pa.config_file_allocated)
    {
        pa.config_file_allocated = make_absolute_path(pa.config_file);
        if (pa.config_file_allocated)
        {
            pa.config_file = pa.config_file_allocated;
        }
    }

    return pa;
}

void prog_args_cleanup(struct prog_args *pa)
{
    if (pa && pa->config_file_allocated)
    {
        free(pa->config_file_allocated);
        pa->config_file_allocated = NULL;
    }
}