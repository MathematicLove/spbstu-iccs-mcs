package org.loadtesting.util;

import org.loadtesting.Result;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

public class CsvWriter {
    public static void write(String filename, List<Result> results) {
        try (FileWriter writer = new FileWriter(filename)) {
            writer.write("Scenario,AvgTimeMillis,P95Millis,Errors\n");
            for (Result r : results) {
                writer.write(String.format("%s,%.2f,%.2f,%d\n",
                        r.scenario(), r.avgTimeMillis(), r.p95Millis(), r.errors()));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
