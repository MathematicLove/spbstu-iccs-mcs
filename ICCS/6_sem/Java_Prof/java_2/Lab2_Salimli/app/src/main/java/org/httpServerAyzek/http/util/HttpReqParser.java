package org.httpServerAyzek.http.util;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class HttpReqParser {
    private String method;
    private String path;
    private String httpVersion;
    private Map<String, String> headers;
    private String body;
    private byte[] bodyBytes;

    public String getMethod() {
        return method;
    }
    public void setMethod(String method) {
        this.method = method;
    }
    public String getPath() {
        return path;
    }
    public void setPath(String path) {
        this.path = path;
    }
    public String getHttpVersion() {
        return httpVersion;
    }
    public void setHttpVersion(String httpVersion) {
        this.httpVersion = httpVersion;
    }
    public Map<String, String> getHeaders() {
        return headers;
    }
    public void setHeaders(Map<String, String> headers) {
        this.headers = headers;
    }
    public String getBody() {
        return bodyBytes == null ? null
                : new String(bodyBytes, StandardCharsets.UTF_8);
    }

    public byte[] getBodyBytes() {
        return bodyBytes;
    }

    public void setBody(String body) {
        this.body = body;
    }

    public static HttpReqParser parse(InputStream in) throws IOException {
        BufferedInputStream bin = new BufferedInputStream(in);

        ByteArrayOutputStream headerBuf = new ByteArrayOutputStream();
        int prev = -1, cur;
        while ((cur = bin.read()) != -1) {
            headerBuf.write(cur);
            if (prev == '\r' && cur == '\n') {
                byte[] hdr = headerBuf.toByteArray();
                int len = hdr.length;
                if (len >= 4 &&
                        hdr[len - 4] == '\r' && hdr[len - 3] == '\n' &&
                        hdr[len - 2] == '\r' && hdr[len - 1] == '\n') {
                    break;
                }
            }
            prev = cur;
        }

        String headerStr = new String(headerBuf.toByteArray(), StandardCharsets.UTF_8);
        BufferedReader reader = new BufferedReader(new StringReader(headerStr));
        HttpReqParser req = new HttpReqParser();

        String requestLine = reader.readLine();
        if (requestLine == null || requestLine.isEmpty()) {
            throw new IOException("Empty Request");
        }
        String[] parts = requestLine.split(" ");
        if (parts.length < 3) {
            throw new IOException("Invalid Request line: " + requestLine);
        }
        req.setMethod(parts[0]);
        req.setPath(parts[1]);
        req.setHttpVersion(parts[2]);

        Map<String, String> hdrMap = new HashMap<>();
        String line;
        while ((line = reader.readLine()) != null && !line.isEmpty()) {
            int idx = line.indexOf(':');
            if (idx != -1) {
                String name = line.substring(0, idx).trim();
                String value = line.substring(idx + 1).trim();
                hdrMap.put(name, value);
            }
        }
        req.setHeaders(hdrMap);

        if (hdrMap.containsKey("Content-Length")) {
            int len = Integer.parseInt(hdrMap.get("Content-Length").trim());
            req.bodyBytes = bin.readNBytes(len);
        }
        return req;
    }
}
