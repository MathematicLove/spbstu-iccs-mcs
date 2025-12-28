package org.loadtesting;

public record Result(
        String scenario,
        double avgTimeMillis,
        double p95Millis,
        int errors
) {}
