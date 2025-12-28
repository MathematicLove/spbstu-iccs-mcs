#define _GNU_SOURCE
#include "conf.h"
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include "utils.h"
#include <errno.h>
#include <stdlib.h>
#include <limits.h>
#include <ctype.h>
#include <syslog.h>

static char *trim(char *s)
{
    if (!s)
        return s;
    while (isspace((unsigned char)*s))
        s++;
    if (*s == '\0')
        return s;
    char *end = s + strlen(s) - 1;
    while (end > s && isspace((unsigned char)*end))
    {
        *end = '\0';
        end--;
    }
    return s;
}

bool read_config(const char *filename, struct conf *out)
{
    syslog(LOG_DEBUG, "read_config: attempting to open: %s", filename);
    printf("read_config: attempting to open: %s\n", filename);

    if (!filename || !out)
        return false;

    FILE *f = fopen(filename, "r");
    if (!f)
    {
        syslog(LOG_ERR, "read_config: fopen(%s) failed: %s", filename, strerror(errno));
        printf("read_config: fopen(%s) failed: %s\n", filename, strerror(errno));
        return false;
    }

    char *line = NULL;
    size_t cap = 0;
    ssize_t len;
    bool got_directory = false;
    bool got_interval = false;

    out->directory[0] = '\0';
    out->interval = 5;

    unsigned long lineno = 0;
    while ((len = getline(&line, &cap, f)) != -1)
    {
        lineno++;
        if (len <= 0)
            continue;
        char *p = line;
        if (p[len - 1] == '\n')
            p[len - 1] = '\0';
        char *t = trim(p);
        if (!t || *t == '\0' || *t == '#')
            continue;

        char *eq = strchr(t, '=');
        if (!eq)
        {
            syslog(LOG_WARNING, "read_config: ignoring malformed line %lu in %s", lineno, filename);
            continue;
        }
        *eq = '\0';
        char *key = trim(t);
        char *val = trim(eq + 1);
        if (!key || !val)
            continue;

        if (strcasecmp(key, "directory") == 0)
        {
            if (strlen(val) >= sizeof(out->directory))
            {
                syslog(LOG_ERR, "read_config: directory value too long on line %lu", lineno);
                free(line);
                fclose(f);
                return false;
            }
            strncpy(out->directory, val, sizeof(out->directory) - 1);
            out->directory[sizeof(out->directory) - 1] = '\0';
            got_directory = true;
        }
        else if (strcasecmp(key, "interval") == 0)
        {
            char *endptr = NULL;
            unsigned long v = strtoul(val, &endptr, 10);
            if (endptr == val || *endptr != '\0')
            {
                syslog(LOG_ERR, "read_config: invalid interval on line %lu in %s", lineno, filename);
                free(line);
                fclose(f);
                return false;
            }
            if (v == 0)
                v = 1;
            if (v > 86400)
                v = 86400;
            out->interval = (unsigned int)v;
            got_interval = true;
        }
        else
        {
            syslog(LOG_WARNING, "read_config: unknown key '%s' on line %lu in %s", key, lineno, filename);
        }
    }

    free(line);
    fclose(f);

    if (!got_directory)
    {
        syslog(LOG_ERR, "read_config: config did not specify 'directory'");
        return false;
    }
    UNUSED(got_interval);
    return true;
}