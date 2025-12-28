#define _XOPEN_SOURCE 700
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <syslog.h>
#include <time.h>
#include <ftw.h>
#include <limits.h>
#include <stdint.h>

#include "prog-args.h"
#include "utils.h"
#include "conf.h"

#ifndef HASH_TABLE_SIZE
#define HASH_TABLE_SIZE 4096
#endif

static char g_config_path[PATH_MAX];
volatile sig_atomic_t g_should_stop = 0;
volatile sig_atomic_t g_should_reload = 0;

static void signal_handler(int sig)
{
    switch (sig)
    {
    case SIGHUP:
        syslog(LOG_INFO, "Received SIGHUP -> reload requested");
        g_should_reload = 1;
        break;
    case SIGINT:
    case SIGTERM:
        syslog(LOG_INFO, "Received signal %d -> termination requested", sig);
        g_should_stop = 1;
        break;
    default:
        break;
    }
}

struct file_entry
{
    char *path;
    dev_t dev;
    ino_t ino;
    time_t mtime;
    struct file_entry *next;
};

struct file_table
{
    struct file_entry **buckets;
    size_t size;
};

static inline uint32_t hash_path(const char *s)
{
    uint32_t h = 5381;
    for (; *s; ++s)
        h = ((h << 5) + h) + (uint8_t)(*s);
    return h;
}

static struct file_table *file_table_create(size_t buckets)
{
    struct file_table *t = calloc(1, sizeof(*t));
    if (!t)
        return NULL;
    t->buckets = calloc(buckets, sizeof(*t->buckets));
    if (!t->buckets)
    {
        free(t);
        return NULL;
    }
    t->size = buckets;
    return t;
}

static void file_entry_free(struct file_entry *e)
{
    if (!e)
        return;
    free(e->path);
    free(e);
}

static void file_table_free(struct file_table *t)
{
    if (!t)
        return;
    for (size_t i = 0; i < t->size; ++i)
    {
        struct file_entry *e = t->buckets[i];
        while (e)
        {
            struct file_entry *n = e->next;
            file_entry_free(e);
            e = n;
        }
    }
    free(t->buckets);
    free(t);
}

static struct file_entry *file_table_get(struct file_table *t, const char *path)
{
    if (!t || !path)
        return NULL;
    uint32_t h = hash_path(path) % t->size;
    for (struct file_entry *e = t->buckets[h]; e; e = e->next)
    {
        if (strcmp(e->path, path) == 0)
            return e;
    }
    return NULL;
}

static bool file_table_put(struct file_table *t, const char *path, dev_t dev, ino_t ino, time_t mtime)
{
    if (!t || !path)
        return false;
    uint32_t h = hash_path(path) % t->size;
    struct file_entry *existing = file_table_get(t, path);
    if (existing)
    {
        existing->dev = dev;
        existing->ino = ino;
        existing->mtime = mtime;
        return true;
    }
    struct file_entry *e = malloc(sizeof(*e));
    if (!e)
        return false;
    e->path = strdup(path);
    if (!e->path)
    {
        free(e);
        return false;
    }
    e->dev = dev;
    e->ino = ino;
    e->mtime = mtime;
    e->next = t->buckets[h];
    t->buckets[h] = e;
    return true;
}

static struct file_table *g_old_table = NULL;
static struct file_table *g_new_table = NULL;
static struct conf g_conf_current;

struct scan_ctx
{
    struct file_table *target;
    bool had_errors;
};

static int visit_callback(const char *fpath, const struct stat *sb, int typeflag, struct FTW *ftwbuf)
{
    UNUSED(ftwbuf);
    struct scan_ctx *ctx = NULL;
    extern struct scan_ctx *g_scan_ctx;
    ctx = g_scan_ctx;
    if (!ctx || !ctx->target)
        return 0;

    if (typeflag == FTW_F || typeflag == FTW_SL || typeflag == FTW_D || typeflag == FTW_DP)
    {
        if (!file_table_put(ctx->target, fpath, sb->st_dev, sb->st_ino, sb->st_mtime))
        {
            syslog(LOG_ERR, "Out of memory while recording %s", fpath);
            ctx->had_errors = true;
            return -1;
        }
    }
    return 0;
}

struct scan_ctx *g_scan_ctx = NULL;

// Модифицируем функцию scan_directory для лучшей обработки ошибок
static bool scan_directory(const char *dir, struct file_table *out_table)
{
    if (!dir || !out_table)
        return false;

    struct stat st;
    if (stat(dir, &st) == -1)
    {
        syslog(LOG_ERR, "Cannot access directory %s: %s", dir, strerror(errno));
        return false;
    }

    if (!S_ISDIR(st.st_mode))
    {
        syslog(LOG_ERR, "%s is not a directory", dir);
        return false;
    }

    struct scan_ctx ctx = {.target = out_table, .had_errors = false};
    g_scan_ctx = &ctx;

    int nftw_flags = FTW_PHYS | FTW_DEPTH;
    if (nftw(dir, visit_callback, 20, nftw_flags) == -1)
    {
        syslog(LOG_ERR, "Error while scanning %s: %s", dir, strerror(errno));
        g_scan_ctx = NULL;
        return false;
    }

    g_scan_ctx = NULL;
    return !ctx.had_errors;
}

static void compare_and_log(struct file_table *old, struct file_table *newt)
{
    for (size_t i = 0; i < newt->size; ++i)
    {
        struct file_entry *e = newt->buckets[i];
        while (e)
        {
            struct file_entry *old_e = (old ? file_table_get(old, e->path) : NULL);
            if (!old_e)
            {
                syslog(LOG_NOTICE, "CREATED: %s", e->path);
            }
            else
            {
                if (e->mtime != old_e->mtime || e->ino != old_e->ino || e->dev != old_e->dev)
                {
                    syslog(LOG_INFO, "MODIFIED: %s (old mtime=%ld new mtime=%ld)", e->path, (long)old_e->mtime, (long)e->mtime);
                }
            }
            e = e->next;
        }
    }

    if (!old)
        return;
    for (size_t i = 0; i < old->size; ++i)
    {
        struct file_entry *e = old->buckets[i];
        while (e)
        {
            struct file_entry *new_e = file_table_get(newt, e->path);
            if (!new_e)
            {
                syslog(LOG_WARNING, "DELETED: %s", e->path);
            }
            e = e->next;
        }
    }
}

static bool reload_config(const char *cfgpath, struct conf *out_conf)
{
    syslog(LOG_DEBUG, "reload_config: trying to load from: %s", cfgpath);
    printf("reload_config: trying to load from: %s\n", cfgpath);

    struct conf tmp;
    if (!read_config(cfgpath, &tmp))
    {
        syslog(LOG_ERR, "Failed to read config file %s", cfgpath);
        printf("Failed to read config file %s\n", cfgpath);
        return false;
    }

    // УБРАНА ПРОВЕРКА ДИРЕКТОРИИ - КОНФИГ ВСЕГДА ПРИМЕНЯЕТСЯ
    *out_conf = tmp;
    syslog(LOG_INFO, "Configuration loaded: directory=%s interval=%u", out_conf->directory, out_conf->interval);
    printf("Configuration loaded: directory=%s interval=%u\n", out_conf->directory, out_conf->interval);
    return true;
}

static void
do_daemonize(void)
{
    pid_t pid = fork();
    if (pid < 0)
    {
        exit(EXIT_FAILURE);
    }
    if (pid > 0)
    {
        exit(EXIT_SUCCESS);
    }

    if (setsid() == -1)
    {
        exit(EXIT_FAILURE);
    }

    pid = fork();
    if (pid < 0)
    {
        exit(EXIT_FAILURE);
    }
    if (pid > 0)
        exit(EXIT_SUCCESS);

    umask(0);

    if (chdir("/") == -1)
    {
        // Просто логируем, но не выходим
    }

    // ???? незакрывая stdio
    // /dev/null
    int fd = open("/dev/null", O_RDWR);
    if (fd != -1)
    {
        dup2(fd, STDIN_FILENO);
        dup2(fd, STDOUT_FILENO);
        dup2(fd, STDERR_FILENO);
        if (fd > STDERR_FILENO)
            close(fd);
    }
}

static char g_config_path[PATH_MAX];

int main(int argc, char *argv[])
{
    struct prog_args pa = prog_parse(argc, argv);

    if (pa.help)
    {
        prog_print_help(pa.progname);
        prog_args_cleanup(&pa);
        return EXIT_SUCCESS;
    }

    // abs
    if (pa.config_file[0] == '/')
    {
        strncpy(g_config_path, pa.config_file, sizeof(g_config_path) - 1);
    }
    else
    {
        char cwd[PATH_MAX];
        if (getcwd(cwd, sizeof(cwd)) != NULL)
        {
            snprintf(g_config_path, sizeof(g_config_path), "%s/%s", cwd, pa.config_file);
        }
        else
        {
            strncpy(g_config_path, pa.config_file, sizeof(g_config_path) - 1);
        }
    }
    g_config_path[sizeof(g_config_path) - 1] = '\0';

    // syslog ДООО
    openlog("daemon", LOG_PERROR, LOG_DAEMON);
    setlogmask(LOG_UPTO(LOG_INFO));

    syslog(LOG_INFO, "Program starting (daemonize=%d) pid=%d", pa.daemonize ? 1 : 0, (int)getpid());

    // хАнДлЕр
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = signal_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGHUP, &sa, NULL);
    sigaction(SIGINT, &sa, NULL);
    sigaction(SIGTERM, &sa, NULL);

    if (pa.daemonize)
    {
        syslog(LOG_INFO, "Daemonizing...");
        do_daemonize();
        // незакрывая syslog
        syslog(LOG_INFO, "Daemonized successfully");
    }

    // конфиг
    if (!reload_config(g_config_path, &g_conf_current))
    {
        syslog(LOG_ERR, "Initial config load failed; exiting");
        closelog();
        prog_args_cleanup(&pa);
        return EXIT_FAILURE;
    }

    // инит
    g_old_table = file_table_create(HASH_TABLE_SIZE);
    if (!g_old_table)
    {
        syslog(LOG_ERR, "Unable to allocate memory for state table");
        closelog();
        prog_args_cleanup(&pa);
        return EXIT_FAILURE;
    }

    syslog(LOG_INFO, "Performing initial scan of %s", g_conf_current.directory);

    struct stat st;
    if (stat(g_conf_current.directory, &st) == -1)
    {
        syslog(LOG_ERR, "Cannot access directory %s: %s", g_conf_current.directory, strerror(errno));
        file_table_free(g_old_table);
        closelog();
        prog_args_cleanup(&pa);
        return EXIT_FAILURE;
    }

    if (!scan_directory(g_conf_current.directory, g_old_table))
    {
        syslog(LOG_ERR, "Initial scan failed; exiting");
        file_table_free(g_old_table);
        closelog();
        prog_args_cleanup(&pa);
        return EXIT_FAILURE;
    }

    syslog(LOG_INFO, "Entering main loop; interval=%u", g_conf_current.interval);

    while (!g_should_stop)
    {
        sleep(g_conf_current.interval);

        if (g_should_stop)
            break;

        if (g_should_reload)
        {
            syslog(LOG_INFO, "Reload requested");
            struct conf newconf;
            if (reload_config(g_config_path, &newconf))
            {
                g_conf_current = newconf;
                syslog(LOG_INFO, "Reloaded configuration applied: directory=%s", newconf.directory);
            }
            else
            {
                syslog(LOG_WARNING, "Reload failed; keeping previous configuration");
            }
            g_should_reload = 0;
        }

        // ДОБАВЛЕНО: проверка существования директории каждый интервал
        struct stat st;
        if (stat(g_conf_current.directory, &st) == -1)
        {
            syslog(LOG_ERR, "Directory %s does not exist: %s", g_conf_current.directory, strerror(errno));
            continue; // Пропускаем итерацию, но не выходим
        }

        if (!S_ISDIR(st.st_mode))
        {
            syslog(LOG_ERR, "Path %s is not a directory", g_conf_current.directory);
            continue; // Пропускаем итерацию, но не выходим
        }

        // новая таб
        g_new_table = file_table_create(HASH_TABLE_SIZE);
        if (!g_new_table)
        {
            syslog(LOG_ERR, "Unable to allocate new table; skipping iteration");
            continue;
        }

        // сканируем директорию
        if (!scan_directory(g_conf_current.directory, g_new_table))
        {
            syslog(LOG_ERR, "Scan failed; skipping iteration");
            file_table_free(g_new_table);
            continue;
        }

        // сравниваем и логируем изменения
        compare_and_log(g_old_table, g_new_table);

        // обновляем таб
        file_table_free(g_old_table);
        g_old_table = g_new_table;
        g_new_table = NULL;
    }

    syslog(LOG_NOTICE, "Daemon shutting down");
    file_table_free(g_old_table);
    file_table_free(g_new_table);
    closelog();
    prog_args_cleanup(&pa);
    return EXIT_SUCCESS;
}