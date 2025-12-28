/* Вспомогательные функции для демонизации сервера */

#include <dirent.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <syslog.h>
#include <unistd.h>

pid_t pid, sid;

void daemonize()
{
  syslog(LOG_DEBUG, " --- STARTING DAEMON --- ");

  /* Закрываем стандартные файловые дескприторы */
  close(STDIN_FILENO);
  close(STDOUT_FILENO);
  close(STDERR_FILENO);

  pid = fork();

  if (pid < 0)
  {
    syslog(LOG_CRIT, "Unable to fork process!");
    exit(EXIT_FAILURE);
  }

  /* Если fork получился, то родительский процесс можно завершить */
  if (pid > 0)
  {
    syslog(LOG_DEBUG, "Killed parent process");
    exit(EXIT_SUCCESS);
  }

  umask(0);

  /* Sid для дочернего процесса */
  sid = setsid();

  if (sid < 0)
  {
    syslog(LOG_CRIT, "Unable to set session id");
    exit(EXIT_FAILURE);
  }

  /* Изменяем текущий рабочий каталог */
  if ((chdir("/")) < 0)
  {
    syslog(LOG_CRIT, "Unable to change working directory");
    exit(EXIT_FAILURE);
  }
}
