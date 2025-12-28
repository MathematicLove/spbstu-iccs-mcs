package org.loadtesting.requests;

import org.loadtesting.Config;
import org.loadtesting.util.ErrorLogger;

import java.io.IOException;
import java.net.URI;
import java.net.http.*;
import java.time.Duration;
import java.util.UUID;
import java.util.concurrent.Callable;

public class RequestOne implements Callable<Long> {
    private final Config config;
    private final HttpClient client;

    public RequestOne(Config config, HttpClient client) {
        this.config = config;
        this.client = client;
    }

    @Override
    public Long call() {
        String id = UUID.randomUUID().toString();
        String payload = "{\"id\":\"" + id + "\",\"payload\":\"" + "X".repeat(config.payloadSize()) + "\"}";

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("http://" + config.host() + ":" + config.port() + "/request1"))
                .timeout(Duration.ofSeconds(5))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(payload))
                .build();

        long start = System.nanoTime();
        try {
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() != 200) {
                String err = "StatusCode: " + response.statusCode() + " | Body: " + response.body();
                System.out.println("[Request-1] Error: " + err);
                ErrorLogger.log("Request-1", -1, err);
                return -1L;
            }
        } catch (IOException | InterruptedException e) {
            System.out.println("[Request-1] IO Error: " + e.toString());
            ErrorLogger.log("Request-1", -1, e.toString());
            return -1L;
        }
        return System.nanoTime() - start;
    }
}
