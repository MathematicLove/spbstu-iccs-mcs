#ifndef INCLUDED_CONF_H
#define INCLUDED_CONF_H

#include <stdbool.h>
#include <limits.h>

struct conf
{
    char directory[PATH_MAX];
    unsigned int interval;
};

bool read_config(const char *filename, struct conf *out);

#endif