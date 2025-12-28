#ifndef INCLUDED_PROG_ARGS_H
#define INCLUDED_PROG_ARGS_H

#include <stdbool.h>
#include <syslog.h>

struct prog_args
{
    const char *progname;
    bool help;
    bool daemonize;
    unsigned int log_upto;
    const char *config_file;
    char *config_file_allocated;
};

struct prog_args prog_parse(int argc, char *argv[]);
void prog_print_help(const char *progname);
void prog_args_cleanup(struct prog_args *pa);

#endif