package org.httpServerAyzek;

import static org.junit.jupiter.api.Assertions.*;

import org.httpServerAyzek.http.handler.HttpHandler;
import org.httpServerAyzek.http.util.HttpReqParser;
import org.httpServerAyzek.http.*;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.*;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.TimeUnit;

public class RunServerTest {
    private static final String HOST = "localhost";
    private static final int PORT = 8081;
    private static HttpServer server;
    private static Thread serverThread;

    @BeforeAll
    public static void setUp() throws Exception {
        server = new HttpServer(HOST, PORT, 4, false);

        server.addListener("/test1", "GET", (req, res) -> res.setBody("GET: Тест пройден"));
        server.addListener("/test2", "POST", (req, res) ->
                res.setBody("POST: Тест пройден. Полученное тело: " +
                        (req.getBody() != null ? req.getBody() : "пусто")));
        server.addListener("/test3", "PUT", (req, res) ->
                res.setBody("PUT: Тест пройден. Полученное тело: " +
                        (req.getBody() != null ? req.getBody() : "пусто")));
        server.addListener("/test4", "PATCH", (req, res) ->
                res.setBody("PATCH: Тест пройден. Полученное тело: " +
                        (req.getBody() != null ? req.getBody() : "пусто")));
        server.addListener("/test5", "DELETE", (req, res) -> res.setBody("DELETE: Тест пройден"));

        server.addListener("/pic", "GET", (req, res) -> {
            byte[] data = new byte[]{1, 2, 3, 4, 5};
            res.addHeader("Content-Type", "image/png");
            res.setBody(data);
        });

        serverThread = new Thread(() -> {
            try {
                server.start();
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
        serverThread.setDaemon(true);
        serverThread.start();
        TimeUnit.MILLISECONDS.sleep(500);
    }

    @AfterAll
    public static void tearDown() { }

    private String sendRequest(String requestText) throws IOException {
        try (Socket socket = new Socket(HOST, PORT)) {
            socket.setSoTimeout(2000);
            OutputStream out = socket.getOutputStream();
            InputStream in = socket.getInputStream();
            out.write(requestText.getBytes(StandardCharsets.UTF_8));
            out.flush();

            ByteArrayOutputStream buffer = new ByteArrayOutputStream();
            byte[] buf = new byte[1024];
            int n;
            while ((n = in.read(buf)) != -1) {
                buffer.write(buf, 0, n);
            }
            return buffer.toString(StandardCharsets.UTF_8.name());
        }
    }

    private byte[] sendRequestBytes(String requestText) throws IOException {
        try (Socket socket = new Socket(HOST, PORT)) {
            socket.setSoTimeout(2000);
            OutputStream out = socket.getOutputStream();
            InputStream in = socket.getInputStream();
            out.write(requestText.getBytes(StandardCharsets.UTF_8));
            out.flush();

            ByteArrayOutputStream buffer = new ByteArrayOutputStream();
            byte[] buf = new byte[1024];
            int n;
            while ((n = in.read(buf)) != -1) {
                buffer.write(buf, 0, n);
            }
            return buffer.toByteArray();
        }
    }

    @Test
    public void testGET() throws IOException {
        String request = "GET /test1 HTTP/1.1\r\nHost: " + HOST + "\r\n\r\n";
        String response = sendRequest(request);
        assertTrue(response.contains("GET: Тест пройден"));
    }

    @Test
    public void testPOST() throws IOException {
        String body = "hello";
        String request = "POST /test2 HTTP/1.1\r\nHost: " + HOST + "\r\n" +
                "Content-Length: " + body.getBytes(StandardCharsets.UTF_8).length + "\r\n\r\n" + body;
        String response = sendRequest(request);
        assertTrue(response.contains("POST: Тест пройден. Полученное тело: hello"));
    }

    @Test
    public void testPUT() throws IOException {
        String body = "update";
        String request = "PUT /test3 HTTP/1.1\r\nHost: " + HOST + "\r\n" +
                "Content-Length: " + body.getBytes(StandardCharsets.UTF_8).length + "\r\n\r\n" + body;
        String response = sendRequest(request);
        assertTrue(response.contains("PUT: Тест пройден. Полученное тело: update"));
    }

    @Test
    public void testPATCH() throws IOException {
        String body = "partial";
        String request = "PATCH /test4 HTTP/1.1\r\nHost: " + HOST + "\r\n" +
                "Content-Length: " + body.getBytes(StandardCharsets.UTF_8).length + "\r\n\r\n" + body;
        String response = sendRequest(request);
        assertTrue(response.contains("PATCH: Тест пройден. Полученное тело: partial"));
    }

    @Test
    public void testDELETE() throws IOException {
        String request = "DELETE /test5 HTTP/1.1\r\nHost: " + HOST + "\r\n\r\n";
        String response = sendRequest(request);
        assertTrue(response.contains("DELETE: Тест пройден"));
    }

    @Test
    public void testNotFound() throws IOException {
        String request = "GET /nonexistent HTTP/1.1\r\nHost: " + HOST + "\r\n\r\n";
        String response = sendRequest(request);
        assertTrue(response.contains("404") || response.contains("Not Found"));
    }

    @Test
    public void testBadRequest() throws IOException {
        String request = "\r\n";
        String response = sendRequest(request);
        assertTrue(response.contains("400") || response.contains("Bad Request"));
    }

    @Test
    public void testBinaryGet() throws IOException {
        String request = "GET /pic HTTP/1.1\r\nHost: " + HOST + "\r\n\r\n";
        byte[] raw = sendRequestBytes(request);

        int split = -1;
        for (int i = 0; i < raw.length - 3; i++) {
            if (raw[i] == '\r' && raw[i + 1] == '\n' && raw[i + 2] == '\r' && raw[i + 3] == '\n') {
                split = i + 4;
                break;
            }
        }
        assertTrue(split != -1);

        String headers = new String(raw, 0, split, StandardCharsets.UTF_8);
        byte[] body = new byte[raw.length - split];
        System.arraycopy(raw, split, body, 0, body.length);

        assertTrue(headers.contains("Content-Type: image/png"));
        assertEquals(5, body.length);
        assertArrayEquals(new byte[]{1, 2, 3, 4, 5}, body);
    }
}
