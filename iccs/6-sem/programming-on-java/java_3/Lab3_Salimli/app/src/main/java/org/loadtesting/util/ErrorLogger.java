package org.loadtesting.util;

import java.io.FileWriter;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.concurrent.locks.ReentrantLock;

public class ErrorLogger {
    private static String fileName = "errors.csv";
    private static final ReentrantLock lock = new ReentrantLock();
    private static boolean initialized = false;

    public static void setFileName(String name) {
        fileName = name;
        initialized = false;
    }

    public static void log(String scenario, int index, String message) {
        lock.lock();
        try (FileWriter writer = new FileWriter(fileName, true)) {
            if (!initialized) {
                writer.write("Scenario,Index,Time,ErrorMessage\n");
                initialized = true;
            }
            writer.write(String.format(
                    "%s,%d,%s,%s\n",
                    scenario,
                    index,
                    LocalDateTime.now(),
                    message.replaceAll(",", " ")
            ));
        } catch (IOException e) {
            System.err.println("Unable to log error: " + e.getMessage());
        } finally {
            lock.unlock();
        }
    }
}
