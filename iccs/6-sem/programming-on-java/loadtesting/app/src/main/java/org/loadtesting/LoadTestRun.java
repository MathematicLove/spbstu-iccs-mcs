package org.loadtesting;

import org.loadtesting.requests.RequestOne;
import org.loadtesting.requests.RequestTwo;
import org.loadtesting.util.CsvWriter;
import org.loadtesting.util.ErrorLogger;

import java.net.http.HttpClient;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

public class LoadTestRun {
    public static void main(String[] args) throws InterruptedException {
        Config config = Config.fromArgs(args);

        String threadMode = System.getProperty("threads", "classic");
        String parserMode = System.getProperty("parser", "own");

        String prefix = "results-" + capitalize(threadMode) + "-" + capitalize(parserMode);
        String resultFile = prefix + ".csv";
        String errorFile = "errors-" + capitalize(threadMode) + "-" + capitalize(parserMode) + ".csv";

        ErrorLogger.setFileName(errorFile);

        ExecutorService executor = Executors.newFixedThreadPool(config.threads());
        HttpClient client = HttpClient.newHttpClient();

        List<Result> results = new ArrayList<>();

        results.add(runScenario("Request-1", new RequestOne(config, client), executor, config));
        results.add(runScenario("Request-2", new RequestTwo(config, client), executor, config));

        CsvWriter.write(resultFile, results);
        executor.shutdown();

        results.forEach(System.out::println);
    }

    private static String capitalize(String word) {
        return word.substring(0, 1).toUpperCase() + word.substring(1).toLowerCase();
    }

    private static Result runScenario(String name, Callable<Long> task,
                                      ExecutorService executor, Config config) throws InterruptedException {
        List<Future<Long>> futures = new ArrayList<>();

        for (int i = 0; i < config.requests(); i++) {
            final int index = i;
            futures.add(executor.submit(() -> {
                try {
                    return task.call();
                } catch (Exception e) {
                    System.out.printf("[%s] #%d: %s%n", name, index, e.toString());
                    ErrorLogger.log(name, index, "EXCEPTION: " + e.toString());
                    return -1L;
                }
            }));
        }

        List<Long> times = new ArrayList<>();
        int errors = 0;

        for (int i = 0; i < futures.size(); i++) {
            Future<Long> future = futures.get(i);
            try {
                Long result = future.get();
                if (result == -1L) {
                    errors++;
                } else {
                    times.add(result);
                }
            } catch (ExecutionException e) {
                errors++;
                ErrorLogger.log(name, i, "ExecutionException: " + e.getCause());
            }
        }

        double avg = times.stream().mapToLong(Long::longValue).average().orElse(0) / 1_000_000.0;
        times.sort(Long::compare);
        double p95 = times.isEmpty() ? 0 : times.get((int)(times.size() * 0.95)) / 1_000_000.0;

        return new Result(name, avg, p95, errors);
    }
}
