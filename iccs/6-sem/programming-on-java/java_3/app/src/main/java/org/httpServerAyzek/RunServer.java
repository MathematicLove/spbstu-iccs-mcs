package org.httpServerAyzek;

import org.httpServerAyzek.http.HttpServer;
import org.httpServerAyzek.http.HttpRes;
import org.httpServerAyzek.http.util.HttpReqParser;
import org.jsonparser.parser.JsonParser;
import org.httpServerAyzek.http.handler.HttpHandler;

import com.google.gson.Gson;

import java.io.IOException;
import java.sql.*;
import java.util.List;
import java.util.Map;

public class RunServer {
    public static void main(String[] args) {
        String threadType = System.getProperty("threads", "classic").toLowerCase();
        String parserType = System.getProperty("parser", "own").toLowerCase();

        boolean isVirtual = threadType.equals("virtual");
        boolean useOwnParser = parserType.equals("own");

        System.out.printf("SERVER CONFIG: Threads = %s | Parser = %s%n",
                isVirtual ? "Virtual" : "Classic", useOwnParser ? "Own" : "Gson");

        HttpServer server = new HttpServer("localhost", 8080, 4, isVirtual);

        Gson gson = new Gson();

        server.addListener("/request1", "POST", (request, response) -> {
            try {
                String body = request.getBody();

                Map<String, Object> json = useOwnParser
                        ? JsonParser.parseJsonToMap(body)
                        : gson.fromJson(body, Map.class);

                String id = json.get("id").toString();
                String payload = json.get("payload").toString();

                try (Connection conn = DriverManager.getConnection("jdbc:sqlite:data.db")) {
                    Statement stmt = conn.createStatement();
                    stmt.execute("CREATE TABLE IF NOT EXISTS payloads(id TEXT PRIMARY KEY, payload TEXT)");
                    PreparedStatement insert = conn.prepareStatement("REPLACE INTO payloads(id, payload) VALUES(?, ?)");
                    insert.setString(1, id);
                    insert.setString(2, payload);
                    insert.executeUpdate();

                    PreparedStatement select = conn.prepareStatement("SELECT payload FROM payloads WHERE id=?");
                    select.setString(1, id);
                    ResultSet rs = select.executeQuery();

                    if (rs.next()) {
                        response.setStatusCode(200);
                        response.setBody(rs.getString("payload"));
                    } else {
                        response.setStatusCode(404);
                        response.setBody("Not found");
                    }
                } catch (SQLException e) {
                    response.setStatusCode(500);
                    response.setBody("Server error: " + e.getMessage());
                    e.printStackTrace();
                }
            } catch (Exception e) {
                response.setStatusCode(400);
                response.setBody("Invalid JSON: " + e.getMessage());
                e.printStackTrace();
            }
        });

        server.addListener("/request2", "POST", (request, response) -> {
            try {
                String body = request.getBody();

                List<Object> numbers = useOwnParser
                        ? JsonParser.parseJsonArray(body)
                        : gson.fromJson(body, List.class);

                double sum = numbers.stream()
                        .mapToDouble(n -> Double.parseDouble(n.toString()))
                        .sum();
                double avg = sum / numbers.size();

                response.setStatusCode(200);
                response.setBody("{\"average\":" + avg + "}");
            } catch (Exception e) {
                response.setStatusCode(400);
                response.setBody("Error parsing numbers: " + e.getMessage());
                e.printStackTrace();
            }
        });

        try {
            server.start();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
