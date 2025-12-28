#include "daemon.h"
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/syslog.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#define MAX_CLIENTS 100
#define BUFFER_SIZE 1024
#define NAME_SIZE 32

typedef struct
{
  int socket;
  char name[NAME_SIZE];
  int has_name;
} Client;

static int stop = 0;
static Client clients[MAX_CLIENTS];
static int client_count = 0;

void signal_handler(int sig)
{
  switch (sig)
  {
  case SIGTERM:
  case SIGINT:
    syslog(LOG_INFO, "Terminate signal catched. Stopping chat server...");
    stop = 1;
    break;
  }
}

void get_timestamp(char *buffer, size_t size)
{
  time_t now = time(NULL);
  struct tm *tm_info = localtime(&now);
  strftime(buffer, size, "%H:%M:%S", tm_info);
}

void send_user_list(int client_socket)
{
  char list[BUFFER_SIZE * 2];
  int offset = 0;
  int count = 0;

  // Исправление 1: считаем только пользователей с именами
  int named_client_count = 0;
  for (int i = 0; i < MAX_CLIENTS; i++)
  {
    if (clients[i].socket != -1 && clients[i].has_name)
    {
      named_client_count++;
    }
  }

  offset += snprintf(list + offset, sizeof(list) - offset,
                     "---------- Users in chat (%d) ----------\n", named_client_count);

  for (int i = 0; i < MAX_CLIENTS; i++)
  {
    if (clients[i].socket != -1 && clients[i].has_name)
    {
      offset += snprintf(list + offset, sizeof(list) - offset, "  - %s\n",
                         clients[i].name);
      count++;
    }
  }

  if (count == 0)
  {
    offset += snprintf(list + offset, sizeof(list) - offset,
                       "  (no other users yet!)\n");
  }

  offset += snprintf(list + offset, sizeof(list) - offset,
                     "------------------------------------\n");

  send(client_socket, list, strlen(list), 0);
}

void init_clients()
{
  for (int i = 0; i < MAX_CLIENTS; i++)
  {
    clients[i].socket = -1;
    clients[i].has_name = 0;
    memset(clients[i].name, 0, NAME_SIZE);
  }
  client_count = 0;
}

int add_client(int sock)
{
  for (int i = 0; i < MAX_CLIENTS; i++)
  {
    if (clients[i].socket == -1)
    {
      clients[i].socket = sock;
      clients[i].has_name = 0;
      client_count++;
      syslog(LOG_INFO, "New client connected (slot %d, total connections: %d)", i,
             client_count);
      return i;
    }
  }
  return -1;
}

void remove_client(int index)
{
  if (clients[index].socket != -1)
  {
    syslog(LOG_INFO, "Client '%s' disconnected (slot %d)", clients[index].name,
           index);

    // Уведомляем других пользователей об отключении только если у клиента было имя
    if (clients[index].has_name)
    {
      char notification[256];
      snprintf(notification, sizeof(notification), "*** %s left the chat :( ***\n",
               clients[index].name);

      for (int i = 0; i < MAX_CLIENTS; i++)
      {
        if (clients[i].socket != -1 && i != index && clients[i].has_name)
        {
          send(clients[i].socket, notification, strlen(notification), 0);
        }
      }
    }

    close(clients[index].socket);
    clients[index].socket = -1;
    clients[index].has_name = 0;
    memset(clients[index].name, 0, NAME_SIZE);
    client_count--;
  }
}

Client *find_client_by_name(const char *name)
{
  for (int i = 0; i < MAX_CLIENTS; i++)
  {
    if (clients[i].socket != -1 && clients[i].has_name &&
        strcmp(clients[i].name, name) == 0)
    {
      return &clients[i];
    }
  }
  return NULL;
}

void broadcast_message(const char *message, int sender_index)
{
  char timestamp[16];
  char formatted[BUFFER_SIZE + NAME_SIZE + 32];

  get_timestamp(timestamp, sizeof(timestamp));
  snprintf(formatted, sizeof(formatted), "[%s] %s: %s\n", timestamp,
           clients[sender_index].name, message);

  for (int i = 0; i < MAX_CLIENTS; i++)
  {
    if (clients[i].socket != -1 && i != sender_index && clients[i].has_name)
    {
      send(clients[i].socket, formatted, strlen(formatted), 0);
    }
  }

  syslog(LOG_INFO, "Broadcast from '%s': %s", clients[sender_index].name,
         message);
}

void send_private_message(const char *message, int sender_index,
                          const char *recipient_name)
{
  Client *recipient = find_client_by_name(recipient_name);

  if (recipient == NULL)
  {
    char error_msg[BUFFER_SIZE];
    snprintf(error_msg, sizeof(error_msg), "Error: User '%s' not found\n",
             recipient_name);
    send(clients[sender_index].socket, error_msg, strlen(error_msg), 0);
    return;
  }

  char timestamp[16];
  char formatted[BUFFER_SIZE + NAME_SIZE + 32];

  get_timestamp(timestamp, sizeof(timestamp));

  snprintf(formatted, sizeof(formatted), "[%s] %s -> you: %s\n", timestamp,
           clients[sender_index].name, message);
  send(recipient->socket, formatted, strlen(formatted), 0);

  snprintf(formatted, sizeof(formatted), "[%s] you -> %s: %s\n", timestamp,
           recipient_name, message);
  send(clients[sender_index].socket, formatted, strlen(formatted), 0);

  syslog(LOG_INFO, "Private message from '%s' to '%s': %s",
         clients[sender_index].name, recipient_name, message);
}

void send_group_message(const char *message, int sender_index,
                        const char *recipients_list)
{
  char recipients_copy[BUFFER_SIZE];

  strncpy(recipients_copy, recipients_list, BUFFER_SIZE - 1);
  recipients_copy[BUFFER_SIZE - 1] = '\0';

  char *token = strtok(recipients_copy, ",");
  while (token != NULL)
  {
    while (*token == ' ')
      token++;
    char *end = token + strlen(token) - 1;
    while (end > token && *end == ' ')
    {
      *end = '\0';
      end--;
    }

    if (strlen(token) > 0)
    {
      send_private_message(message, sender_index, token);
    }

    token = strtok(NULL, ",");
  }
}

void handle_rename(int client_index)
{
  clients[client_index].has_name = 0;
  memset(clients[client_index].name, 0, NAME_SIZE);

  const char *rename_prompt = "Enter your new name:\n";
  send(clients[client_index].socket, rename_prompt, strlen(rename_prompt), 0);

  syslog(LOG_INFO, "Client in slot %d initiated rename", client_index);
}

void handle_message(int client_index, char *message)
{
  char *newline = strchr(message, '\n');
  if (newline)
    *newline = '\0';
  newline = strchr(message, '\r');
  if (newline)
    *newline = '\0';

  if (strlen(message) == 0)
    return;

  // rename
  if (clients[client_index].has_name && strcmp(message, "#rename") == 0)
  {
    handle_rename(client_index);
    return;
  }

  // Если у клиента еще нет имени, первое сообщение - это его имя
  if (!clients[client_index].has_name)
  {
    // Проверяем, не занято ли это имя
    for (int i = 0; i < MAX_CLIENTS; i++)
    {
      if (clients[i].socket != -1 && clients[i].has_name && i != client_index &&
          strcmp(clients[i].name, message) == 0)
      {
        char error_msg[256];
        snprintf(
            error_msg, sizeof(error_msg),
            "Oops!!!: Name '%s' is already taken. Please enter another name:\n",
            message);
        send(clients[client_index].socket, error_msg, strlen(error_msg), 0);
        syslog(LOG_WARNING, "Client in slot %d tried to use taken name: %s",
               client_index, message);
        return;
      }
    }

    strncpy(clients[client_index].name, message, NAME_SIZE - 1);
    clients[client_index].has_name = 1;

    char welcome[256];

    // показываем количество пользователей
    int named_client_count = 0;
    for (int i = 0; i < MAX_CLIENTS; i++)
    {
      if (clients[i].socket != -1 && clients[i].has_name)
      {
        named_client_count++;
      }
    }

    snprintf(welcome, sizeof(welcome), "WELCOMEEEE, %s! Users now: %d\n",
             message, named_client_count);
    send(clients[client_index].socket, welcome, strlen(welcome), 0);

    send_user_list(clients[client_index].socket);

    // Уведомляем других о новом пользователе
    char notification[256];
    snprintf(notification, sizeof(notification), "*** %s is here now ***\n",
             message);
    for (int i = 0; i < MAX_CLIENTS; i++)
    {
      if (clients[i].socket != -1 && i != client_index && clients[i].has_name)
      {
        send(clients[i].socket, notification, strlen(notification), 0);
      }
    }

    syslog(LOG_INFO, "Client in slot %d set name: %s", client_index, message);
    return;
  }

  // Проверка @
  if (message[0] == '@')
  {
    char *colon = strchr(message, ':');
    if (colon != NULL)
    {
      *colon = '\0';
      char *recipients = message + 1;
      char *msg_text = colon + 1;

      while (*msg_text == ' ')
        msg_text++;

      if (strchr(recipients, ',') != NULL)
      {
        send_group_message(msg_text, client_index, recipients);
      }
      else
      {
        send_private_message(msg_text, client_index, recipients);
      }
      return;
    }
  }

  broadcast_message(message, client_index);
}

int create_server_socket(int port)
{
  struct sockaddr_in addr;

  int listener = socket(AF_INET, SOCK_STREAM, 0);
  if (listener < 0)
  {
    syslog(LOG_ERR, "Unable to create server socket");
    exit(1);
  }

  if (fcntl(listener, F_SETFL, O_NONBLOCK) < 0)
  {
    syslog(LOG_ERR, "Unable to set non-blocking mode");
    exit(1);
  }

  int opt = 1;
  if (setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) < 0)
  {
    syslog(LOG_WARNING, "Unable to set SO_REUSEADDR");
  }

  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = INADDR_ANY;

  if (bind(listener, (struct sockaddr *)&addr, sizeof(addr)) < 0)
  {
    syslog(LOG_ERR, "Unable to bind socket to port %d", port);
    exit(1);
  }

  if (listen(listener, 10) < 0)
  {
    syslog(LOG_ERR, "Unable to listen");
    exit(1);
  }

  return listener;
}

int chat_server(int port)
{
  openlog("CHAT-SERVER", 0, LOG_DAEMON);

  daemonize();

  signal(SIGTERM, signal_handler);
  signal(SIGINT, signal_handler);

  init_clients();

  int listener = create_server_socket(port);
  syslog(LOG_INFO, "Chat server started on port %d", port);

  while (!stop)
  {
    fd_set readset;
    FD_ZERO(&readset);
    FD_SET(listener, &readset);

    int max_fd = listener;

    for (int i = 0; i < MAX_CLIENTS; i++)
    {
      if (clients[i].socket != -1)
      {
        FD_SET(clients[i].socket, &readset);
        if (clients[i].socket > max_fd)
        {
          max_fd = clients[i].socket;
        }
      }
    }

    struct timeval timeout;
    timeout.tv_sec = 1;
    timeout.tv_usec = 0;

    int activity = select(max_fd + 1, &readset, NULL, NULL, &timeout);

    if (activity < 0 && errno != EINTR)
    {
      syslog(LOG_ERR, "Select error: %s", strerror(errno));
      continue;
    }

    if (activity == 0)
    {
      continue;
    }

    if (FD_ISSET(listener, &readset))
    {
      int new_socket = accept(listener, NULL, NULL);
      if (new_socket >= 0)
      {
        if (fcntl(new_socket, F_SETFL, O_NONBLOCK) < 0)
        {
          syslog(LOG_WARNING, "Unable to set non-blocking mode for client");
        }

        int index = add_client(new_socket);
        if (index == -1)
        {
          syslog(LOG_WARNING, "Maximum clients reached, rejecting connection");
          const char *msg = "Server full, please try again later\n";
          send(new_socket, msg, strlen(msg), 0);
          close(new_socket);
        }
        else
        {
          const char *greeting = "Connected to chat server. Please enter your "
                                 "name:\n";
          send(new_socket, greeting, strlen(greeting), 0);
        }
      }
    }

    for (int i = 0; i < MAX_CLIENTS; i++)
    {
      if (clients[i].socket == -1)
        continue;

      if (FD_ISSET(clients[i].socket, &readset))
      {
        char buffer[BUFFER_SIZE];
        int bytes_read = recv(clients[i].socket, buffer, BUFFER_SIZE - 1, 0);

        if (bytes_read <= 0)
        {
          remove_client(i);
        }
        else
        {
          buffer[bytes_read] = '\0';
          handle_message(i, buffer);
        }
      }
    }
  }

  close(listener);
  for (int i = 0; i < MAX_CLIENTS; i++)
  {
    if (clients[i].socket != -1)
    {
      close(clients[i].socket);
    }
  }

  syslog(LOG_INFO, "Chat server stopped");
  closelog();
  return 0;
}