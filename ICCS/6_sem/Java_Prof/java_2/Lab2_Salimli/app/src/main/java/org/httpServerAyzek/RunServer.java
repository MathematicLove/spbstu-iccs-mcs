package org.httpServerAyzek;

import org.httpServerAyzek.http.HttpServer;
import org.httpServerAyzek.http.handler.HttpHandler;
import org.httpServerAyzek.http.HttpRes;
import org.httpServerAyzek.http.util.HttpReqParser;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.*;

public class RunServer {
    public static void main(String[] args) {
        HttpServer server = new HttpServer("localhost", 8080, 4, false);
        server.addListener("/link1", "GET", new HttpHandler() {
            @Override
            public void handle(HttpReqParser request, HttpRes response) {
                response.setBody("GET: It work!!!");
            }
        });
        server.addListener("/link2", "POST", new HttpHandler() {
            @Override
            public void handle(HttpReqParser request, HttpRes response) {
                String body = request.getBody();
                response.setBody("POST: geted: " + (body != null ? body : "пусто"));
            }
        });
        server.addListener("/link3", "PUT", new HttpHandler() {
            @Override
            public void handle(HttpReqParser request, HttpRes response) {
                String body = request.getBody();
                response.setBody("PUT: geted: " + (body != null ? body : "пусто"));
            }
        });
        server.addListener("/link4", "PATCH", new HttpHandler() {
            @Override
            public void handle(HttpReqParser request, HttpRes response) {
                String body = request.getBody();
                response.setBody("PATCH: geted: " + (body != null ? body : "пусто"));
            }
        });
        server.addListener("/delete", "DELETE", new HttpHandler() {
            @Override
            public void handle(HttpReqParser request, HttpRes response) {
                response.setBody("DELETE: done");
            }
        });
        server.addListener("/pic", "GET", (req, res) -> {
            Path p = Paths.get("test.png");
            byte[] data = Files.readAllBytes(p);
            res.addHeader("Content-Type", "image/png");
            res.setBody(data);
        });
        server.addListener("/pic", "GET", (req, res) -> {
            try {
                BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB);
                Graphics2D g = img.createGraphics();
                g.setColor(Color.RED);
                g.fillRect(0, 0, 10, 10);
                g.dispose();
                ByteArrayOutputStream baos = new ByteArrayOutputStream();
                ImageIO.write(img, "png", baos);
                byte[] data = baos.toByteArray();
                res.addHeader("Content-Type", "image/png");
                res.setBody(data);
            } catch (Exception e) {
                res.setStatusCode(500);
                res.setBody("Error generating image");
            }
        });
        try {
            server.start();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
