#include <cstdio>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <fcntl.h>
#include <signal.h>
#include <time.h>
#include <errno.h>
#include <dirent.h>

char* method,
* uri,       // "/index.html"
* qs,        // "a=1&b=2"
* prot;      // "HTTP/1.1"

void startServer(const char* port);
void route(char* request_body, int body_length);
void log_message(const char* filename, const char* message);
const char* getExtension(const char* path);
const char* getMimeType(const char* ext);
void returnFile(const char* uri);
void saveFile(const char* uri, char* request_body, int body_length);
void respond(int n);
int directoryExists(const char* path);
int isDirectory(const char* path);

#define LOG_FILE "/Users/monadayzek/Desktop/Ayzek/1_SPbSTU/1_SPbSTU_ICCS_MCS/ICCS/7_sem/UNIX_OS/Server/Server/server.log"
#define ROOT "/Users/monadayzek/Desktop/Ayzek/1_SPbSTU/1_SPbSTU_ICCS_MCS/ICCS/7_sem/UNIX_OS/Server/Server"
#define FIRST_PAGE "/start.html"
#define CONNMAX 1000
#define BUFFER_SIZE 65536

static int listenfd, clients[CONNMAX];
typedef struct { char* name, * value; } header_t;
static header_t reqhdr[17] = { {"\0", "\0"} };
static int clientfd;
static char* buf;

void log_message(const char* filename, const char* message) {
    FILE* logfile;
    logfile = fopen(filename, "a");
    if (!logfile)
        return;
    
    time_t now = time(NULL);
    struct tm* tm_info = localtime(&now);
    char timestamp[20];
    strftime(timestamp, 20, "%Y-%m-%d %H:%M:%S", tm_info);
    
    fprintf(logfile, "[%s] my_server: %s\n", timestamp, message);
    fclose(logfile);
}

// Проверка существования директории
int directoryExists(const char* path) {
    struct stat st;
    if (stat(path, &st) == 0 && S_ISDIR(st.st_mode)) {
        return 1;
    }
    return 0;
}

// Проверка является ли путь директорией
int isDirectory(const char* path) {
    struct stat st;
    if (stat(path, &st) == 0) {
        return S_ISDIR(st.st_mode);
    }
    return 0;
}

void startServer(const char* port) {
    struct addrinfo hints, * res, * p;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;
    if (getaddrinfo(NULL, port, &hints, &res) != 0) {
        perror("getaddrinfo() error");
        log_message(LOG_FILE, "getaddrinfo() error");
        exit(1);
    }
    
    for (p = res; p != NULL; p = p->ai_next) {
        int option = 1;
        listenfd = socket(p->ai_family, p->ai_socktype, 0);
        setsockopt(listenfd, SOL_SOCKET, SO_REUSEADDR, &option, sizeof(option));
        if (listenfd == -1) continue;
        if (bind(listenfd, p->ai_addr, p->ai_addrlen) == 0) break;
    }
    if (p == NULL) {
        perror("socket() or bind()");
        log_message(LOG_FILE, "error in socket() or bind()");
        exit(1);
    }
    freeaddrinfo(res);

    if (listen(listenfd, 1000000) != 0) {
        perror("listen() error");
        log_message(LOG_FILE, "listen() error");
        exit(1);
    }
}

const char* getExtension(const char* path) {
    const char* ptr = path + (strlen(path) - 1);
    do {
        if (*ptr == '.')
            return ptr + 1;
        if (*ptr == '\\' || *ptr == '/')
            return "";
    } while (--ptr >= path);
    return "";
}

const char* getMimeType(const char* ext) {
    if (strcasecmp(ext, "html") == 0 || strcasecmp(ext, "htm") == 0) return "text/html";
    if (strcasecmp(ext, "css") == 0) return "text/css";
    if (strcasecmp(ext, "js") == 0) return "application/javascript";
    if (strcasecmp(ext, "json") == 0) return "application/json";
    if (strcasecmp(ext, "txt") == 0) return "text/plain";
    if (strcasecmp(ext, "jpg") == 0 || strcasecmp(ext, "jpeg") == 0) return "image/jpeg";
    if (strcasecmp(ext, "png") == 0) return "image/png";
    if (strcasecmp(ext, "gif") == 0) return "image/gif";
    if (strcasecmp(ext, "bmp") == 0) return "image/bmp";
    if (strcasecmp(ext, "ico") == 0) return "image/x-icon";
    if (strcasecmp(ext, "svg") == 0) return "image/svg+xml";
    if (strcasecmp(ext, "pdf") == 0) return "application/pdf";
    if (strcasecmp(ext, "zip") == 0) return "application/zip";
    return "application/octet-stream";
}

// save PUT
void saveFile(const char* uri, char* request_body, int body_length) {
    char path[2048];
    sprintf(path, "%s%s", ROOT, uri);
    
    char log_msg[512];
    sprintf(log_msg, "PUT: Saving to: %s, body_length: %d", path, body_length);
    log_message(LOG_FILE, log_msg);
    
    // parent dir ?
    char parent_dir[2048];
    strcpy(parent_dir, path);
    char* last_slash = strrchr(parent_dir, '/');
    if (last_slash) {
        *last_slash = '\0';
        if (!directoryExists(parent_dir)) {
            // todo : na 403 vmesto 404
            printf("HTTP/1.1 403 Forbidden\r\n");
            printf("Content-Type: text/html\r\n");
            printf("Connection: close\r\n");
            printf("\r\n");
            printf("<html><body><h1>403 Forbidden</h1><p>Cannot create file - parent directory does not exist: %s</p></body></html>", parent_dir);
            
            sprintf(log_msg, "PUT: Forbidden - parent directory does not exist: %s", parent_dir);
            log_message(LOG_FILE, log_msg);
            return;
        }
    }
    
    // Ищем Content-Length в заголовках
    int content_length = 0;
    header_t* h = reqhdr;
    while (h->name && h < reqhdr + 16) {
        if (strcasecmp(h->name, "Content-Length") == 0) {
            content_length = atoi(h->value);
            break;
        }
        h++;
    }
    
    // ИСПРАВЛЕНИЕ: Правильная обработка Content-Length
    // Если Content-Length указан И больше чем полученные данные - продолжаем чтение
    if (content_length > 0 && body_length < content_length) {
        // Выделяем буфер для полного тела запроса
        char* full_body = (char*)malloc(content_length);
        if (!full_body) {
            printf("HTTP/1.1 500 Internal Server Error\r\n");
            printf("Content-Type: text/html\r\n");
            printf("Connection: close\r\n");
            printf("\r\n");
            printf("<html><body><h1>500 Internal Server Error</h1><p>Memory allocation failed</p></body></html>");
            log_message(LOG_FILE, "PUT: Memory allocation failed");
            return;
        }
        
        // Копируем уже полученные данные
        memcpy(full_body, request_body, body_length);
        
        // Читаем оставшиеся данные
        int remaining = content_length - body_length;
        int total_received = body_length;
        
        while (remaining > 0) {
            int bytes_read = recv(clientfd, full_body + total_received, remaining, 0);
            if (bytes_read <= 0) {
                printf("HTTP/1.1 400 Bad Request\r\n");
                printf("Content-Type: text/html\r\n");
                printf("Connection: close\r\n");
                printf("\r\n");
                printf("<html><body><h1>400 Bad Request</h1><p>Incomplete data: received %d of %d bytes</p></body></html>",
                       total_received, content_length);
                
                sprintf(log_msg, "PUT: Incomplete data: %d of %d bytes", total_received, content_length);
                log_message(LOG_FILE, log_msg);
                free(full_body);
                return;
            }
            total_received += bytes_read;
            remaining -= bytes_read;
        }
        
        // Используем полное тело запроса
        request_body = full_body;
        body_length = content_length;
        
        // Сохраняем файл
        FILE* file = fopen(path, "wb");
        if (file) {
            size_t written = fwrite(request_body, 1, body_length, file);
            fclose(file);
            
            if (written == (size_t)body_length) {
                printf("HTTP/1.1 201 Created\r\n");
                printf("Content-Type: text/html\r\n");
                printf("Connection: close\r\n");
                printf("\r\n");
                printf("<html><body><h1>201 Created</h1><p>File %s saved (%zu bytes)</p></body></html>", uri, written);
                
                sprintf(log_msg, "PUT: File saved: %s (%zu bytes)", path, written);
                log_message(LOG_FILE, log_msg);
            } else {
                printf("HTTP/1.1 500 Internal Server Error\r\n");
                printf("Content-Type: text/html\r\n");
                printf("Connection: close\r\n");
                printf("\r\n");
                printf("<html><body><h1>500 Internal Server Error</h1><p>Error writing file (written %zu of %d bytes)</p></body></html>", written, body_length);
                
                sprintf(log_msg, "PUT: Error writing file: %s (%zu of %d bytes)", path, written, body_length);
                log_message(LOG_FILE, log_msg);
            }
        } else {
            printf("HTTP/1.1 500 Internal Server Error\r\n");
            printf("Content-Type: text/html\r\n");
            printf("Connection: close\r\n");
            printf("\r\n");
            printf("<html><body><h1>500 Internal Server Error</h1><p>Cannot create file: %s</p></body></html>", strerror(errno));
            
            sprintf(log_msg, "PUT: Cannot create file: %s - %s", path, strerror(errno));
            log_message(LOG_FILE, log_msg);
        }
        
        free(full_body);
    }
    else {
        // Если Content-Length не указан или данные уже полностью получены
        // Используем полученные данные как есть
        
        // Проверяем что у нас есть данные для записи
//        if (body_length <= 0) {
//            printf("HTTP/1.1 400 Bad Request\r\n");
//            printf("Content-Type: text/html\r\n");
//            printf("Connection: close\r\n");
//            printf("\r\n");
//            printf("<html><body><h1>400 Bad Request</h1><p>No data received</p></body></html>");
//            
//            log_message(LOG_FILE, "PUT: No data received");
//            return;
//        }
        
        // Сохраняем файл
        FILE* file = fopen(path, "wb");
        if (file) {
            size_t written = fwrite(request_body, 1, body_length, file);
            fclose(file);
            
            if (written == (size_t)body_length) {
                printf("HTTP/1.1 201 Created\r\n");
                printf("Content-Type: text/html\r\n");
                printf("Connection: close\r\n");
                printf("\r\n");
                printf("<html><body><h1>201 Created</h1><p>File %s saved (%zu bytes)</p></body></html>", uri, written);
                
                sprintf(log_msg, "PUT: File saved: %s (%zu bytes)", path, written);
                log_message(LOG_FILE, log_msg);
            } else {
                printf("HTTP/1.1 500 Internal Server Error\r\n");
                printf("Content-Type: text/html\r\n");
                printf("Connection: close\r\n");
                printf("\r\n");
                printf("<html><body><h1>500 Internal Server Error</h1><p>Error writing file (written %zu of %d bytes)</p></body></html>", written, body_length);
                
                sprintf(log_msg, "PUT: Error writing file: %s (%zu of %d bytes)", path, written, body_length);
                log_message(LOG_FILE, log_msg);
            }
        } else {
            printf("HTTP/1.1 500 Internal Server Error\r\n");
            printf("Content-Type: text/html\r\n");
            printf("Connection: close\r\n");
            printf("\r\n");
            printf("<html><body><h1>500 Internal Server Error</h1><p>Cannot create file: %s</p></body></html>", strerror(errno));
            
            sprintf(log_msg, "PUT: Cannot create file: %s - %s", path, strerror(errno));
            log_message(LOG_FILE, log_msg);
        }
    }
}

void returnFile(const char* uri) {
    char path[2048];
    sprintf(path, "%s%s", ROOT, uri);
    
    // Проверяем, не является ли путь директорией
    if (isDirectory(path)) {
        printf("HTTP/1.1 403 Forbidden\r\n");
        printf("Content-Type: text/html\r\n");
        printf("Connection: close\r\n");
        printf("\r\n");
        printf("<html><body><h1>403 Forbidden</h1><p>Cannot access directory</p></body></html>");
        
        char log_msg[512];
        sprintf(log_msg, "Attempt to access directory: %s", path);
        log_message(LOG_FILE, log_msg);
        return;
    }
    
    char log_msg[512];
    sprintf(log_msg, "Trying to open: %s", path);
    log_message(LOG_FILE, log_msg);
    
    FILE* file = fopen(path, "rb");
    if (file) {
        fseek(file, 0, SEEK_END);
        long fileLen = ftell(file);
        fseek(file, 0, SEEK_SET);
        
        // Логируем информацию о больших файлах
        if (fileLen > 1024 * 1024) { // > 1MB
            sprintf(log_msg, "Large file detected: %s (%ld bytes)", path, fileLen);
            log_message(LOG_FILE, log_msg);
        }
        
        // Получаем MIME type
        const char* extension = getExtension(path);
        const char* mime_type = getMimeType(extension);
        
        // Отправляем заголовки
        printf("HTTP/1.1 200 OK\r\n");
        printf("Content-Type: %s\r\n", mime_type);
        printf("Content-Length: %ld\r\n", fileLen);
        printf("Connection: close\r\n");
        printf("\r\n");
        fflush(stdout); // Важно: сбрасываем буфер stdout
        
        // Отправляем файл непосредственно через сокет
        char buffer[8192];
        size_t bytes_read;
        long total_sent = 0;
        
        while ((bytes_read = fread(buffer, 1, sizeof(buffer), file)) > 0) {
            ssize_t bytes_sent = send(clientfd, buffer, bytes_read, 0);
            if (bytes_sent < 0) {
                sprintf(log_msg, "Error sending file data: %s", strerror(errno));
                log_message(LOG_FILE, log_msg);
                break;
            }
            total_sent += bytes_sent;
        }
        
        fclose(file);
        
        sprintf(log_msg, "File sent successfully: %s (%ld bytes, type: %s, sent: %ld)",
                path, fileLen, mime_type, total_sent);
        log_message(LOG_FILE, log_msg);
    }
    else {
        printf("HTTP/1.1 404 Not Found\r\n");
        printf("Content-Type: text/html\r\n");
        printf("Connection: close\r\n");
        printf("\r\n");
        printf("<html><body><h1>404 Not Found</h1><p>File %s does not exist</p></body></html>");
        
        sprintf(log_msg, "File not found: %s", path);
        log_message(LOG_FILE, log_msg);
    }
}

void route(char* request_body, int body_length) {
    char log_msg[256];
    sprintf(log_msg, "Routing: method=%s, uri=%s, body_length=%d", method, uri, body_length);
    log_message(LOG_FILE, log_msg);
    
    if (strcmp("/", uri) == 0 && strcmp("GET", method) == 0) {
        returnFile(FIRST_PAGE);
    }
    else if (strcmp("GET", method) == 0) {
        returnFile(uri);
    }
    else if (strcmp("PUT", method) == 0) {
        saveFile(uri, request_body, body_length);
    }
    else {
        printf("HTTP/1.1 501 Not Implemented\r\n");
        printf("Content-Type: text/html\r\n");
        printf("Connection: close\r\n");
        printf("\r\n");
        printf("<html><body><h1>501 Not Implemented</h1></body></html>");
        
        log_message(LOG_FILE, "Method not implemented");
    }
}

//client connection
void respond(int n) {
    int rcvd;

    // Увеличиваем буфер для больших файлов
    buf = (char*)malloc(BUFFER_SIZE);
    rcvd = recv(clients[n], buf, BUFFER_SIZE - 1, 0);

    if (rcvd < 0) {
        log_message(LOG_FILE, "recv() error");
    }
    else if (rcvd == 0) {
        log_message(LOG_FILE, "Client disconnected unexpectedly.");
    }
    else {
        buf[rcvd] = '\0';

        char log_msg[512];
        sprintf(log_msg, "Received %d bytes from client", rcvd);
        log_message(LOG_FILE, log_msg);

        // копия буфера до strtok
        char* buf_copy = (char*)malloc(rcvd + 1);
        memcpy(buf_copy, buf, rcvd + 1);

        method = strtok(buf, " \t\r\n");
        uri = strtok(NULL, " \t");
        prot = strtok(NULL, " \t\r\n");

        if (!method || !uri || !prot) {
            printf("HTTP/1.1 400 Bad Request\r\n\r\nInvalid request");
            free(buf_copy);
            goto cleanup;
        }

        if (qs = strchr(uri, '?')) {
            *qs++ = '\0';
        }
        else {
            qs = NULL;
        }

        header_t* h = reqhdr;
        char* k, * v;
        while (h < reqhdr + 16) {
            k = strtok(NULL, "\r\n: \t");
            if (!k) break;
            v = strtok(NULL, "\r\n");
            if (!v) break;
            while (*v && *v == ' ') v++;
            h->name = k;
            h->value = v;
            h++;
        }

        char* request_body = NULL;
        int body_length = 0;
        char* double_newline = strstr(buf_copy, "\r\n\r\n");
        if (double_newline) {
            request_body = double_newline + 4;
            body_length = rcvd - (request_body - buf_copy);
        } else {
            double_newline = strstr(buf_copy, "\n\n");
            if (double_newline) {
                request_body = double_newline + 2;
                body_length = rcvd - (request_body - buf_copy);
            } else {
                request_body = buf_copy + strlen(buf_copy); // fallback
                body_length = 0;
            }
        }

        sprintf(log_msg, "Body found: pointer=%p, length=%d", request_body, body_length);
        log_message(LOG_FILE, log_msg);

        clientfd = clients[n];
        dup2(clientfd, STDOUT_FILENO);

        route(request_body, body_length);

        fflush(stdout);
        shutdown(STDOUT_FILENO, SHUT_WR);
        close(STDOUT_FILENO);
        
        free(buf_copy);
    }

cleanup:
    shutdown(clientfd, SHUT_RDWR);
    close(clientfd);
    clients[n] = -1;
    free(buf);
}

int main(int c, char** v) {
    // демонизация
    pid_t pid, sid;

    pid = fork();
    if (pid < 0) {
        log_message(LOG_FILE, "Failed to fork from parent process");
        exit(EXIT_FAILURE);
    }

    if (pid > 0) {
        exit(EXIT_SUCCESS);
    }

    umask(0);

    sid = setsid();
    if (sid < 0) {
        log_message(LOG_FILE, "setsid failed!");
        exit(EXIT_FAILURE);
    }

    int x;
    for (x = sysconf(_SC_OPEN_MAX); x >= 0; x--) {
        close(x);
    }

    log_message(LOG_FILE, "server daemonized!");

    // Проверяем существование корневой директории
    if (!directoryExists(ROOT)) {
        char log_msg[512];
        sprintf(log_msg, "Root directory does not exist: %s", ROOT);
        log_message(LOG_FILE, log_msg);
        exit(EXIT_FAILURE);
    }

    struct sockaddr_in clientaddr;
    socklen_t addrlen;
    int slot = 0;

    int i;
    for (i = 0; i <CONNMAX; i++)
        clients[i] = -1;
        
    startServer("8000");
    
    log_message(LOG_FILE, "server started!");

    while (1) {
        addrlen = sizeof(clientaddr);
        clients[slot] = accept(listenfd, (struct sockaddr*)&clientaddr, &addrlen);

        if (clients[slot] < 0) {
            log_message(LOG_FILE, "accept() error");
        }
        else {
            if (fork() == 0) {
                close(listenfd);
                respond(slot);
                log_message(LOG_FILE, "server responded");
                exit(0);
            }
            else {
                close(clients[slot]);
            }
        }

        while (clients[slot] != -1)
            slot = (slot + 1) % CONNMAX;
    }
    return 0;
}
