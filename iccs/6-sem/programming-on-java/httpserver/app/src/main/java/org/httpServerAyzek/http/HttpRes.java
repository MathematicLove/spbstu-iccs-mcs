package org.httpServerAyzek.http;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class HttpRes {
    private int statusCode;
    private String reasonPhrase;
    private Map<String, String> headers;
    private byte[] body;

    public HttpRes() {
        this.statusCode = 200;
        this.headers = new HashMap<>();
    }

    public void setStatusCode(int statusCode) {
        this.statusCode = statusCode;
    }

    public int getStatusCode() {
        return this.statusCode;
    }

    public void setReasonPhrase(String reasonPhrase) {
        this.reasonPhrase = reasonPhrase;
    }

    public String getReasonPhrase() {
        return this.reasonPhrase;
    }

    public void setBody(String str) {
        this.body = (str == null) ? null : str.getBytes(StandardCharsets.UTF_8);
    }

    public void setBody(byte[] bytes) {
        this.body = bytes;
    }

    public byte[] getBody() {
        return body;
    }

    public Map<String, String> getHeaders() {
        return this.headers;
    }

    public void addHeader(String key, String value) {
        this.headers.put(key, value);
    }

    public void send(OutputStream out) throws IOException {
        if (reasonPhrase == null) reasonPhrase = "OK";

        StringBuilder head = new StringBuilder()
                .append("HTTP/1.1 ").append(statusCode).append(' ')
                .append(reasonPhrase).append("\r\n");

        byte[] respBody = (body == null) ? new byte[0] : body;
        headers.putIfAbsent("Content-Length", String.valueOf(respBody.length));

        for (Map.Entry<String,String> e : headers.entrySet()) {
            head.append(e.getKey()).append(": ").append(e.getValue()).append("\r\n");
        }
        head.append("\r\n");

        out.write(head.toString().getBytes(StandardCharsets.UTF_8));
        out.write(respBody);
        out.flush();
    }
}
