package org.httpServerAyzek.http.handler;

import org.httpServerAyzek.http.HttpRes;
import org.httpServerAyzek.http.util.HttpReqParser;

import java.io.IOException;

public interface HttpHandler {
    void handle(HttpReqParser request, HttpRes response) throws IOException;
}
