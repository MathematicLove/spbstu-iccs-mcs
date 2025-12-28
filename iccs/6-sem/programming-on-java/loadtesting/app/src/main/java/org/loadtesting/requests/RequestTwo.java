package org.loadtesting.requests;

import org.loadtesting.Config;
import org.loadtesting.util.ErrorLogger;

import java.io.IOException;
import java.net.URI;
import java.net.http.*;
import java.time.Duration;
import java.util.Random;
import java.util.concurrent.Callable;

public class RequestTwo implements Callable<Long> {
    private final Config config;
    private final HttpClient client;
    private final Random random = new Random();

    public RequestTwo(Config config, HttpClient client) {
        this.config = config;
        this.client = client;
    }

    @Override
    public Long call() {
        String numbersJson = generateNumbersJson();

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("http://" + config.host() + ":" + config.port() + "/request2"))
                .timeout(Duration.ofSeconds(10)) // увеличили таймаут
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(numbersJson))
                .build();

        long start = System.nanoTime();
        try {
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() != 200) {
                String err = "StatusCode: " + response.statusCode() + " | Body: " + response.body();
                System.out.println("[Request-2] Error: " + err);
                ErrorLogger.log("Request-2", -1, err);
                return -1L;
            }
        } catch (IOException | InterruptedException e) {
            System.out.println("[Request-2] IO Error: " + e.toString());
            ErrorLogger.log("Request-2", -1, e.toString());
            return -1L;
        }
        return System.nanoTime() - start;
    }

    private String generateNumbersJson() {
        StringBuilder json = new StringBuilder("[");
        for (int i = 0; i < 100; i++) {
            json.append(random.nextInt(100)).append(",");
        }
        json.deleteCharAt(json.length() - 1);
        json.append("]");
        return json.toString();
    }
}
