package org.loadtesting;

public record Config(
        String host,
        int port,
        int threads,
        int requests,
        int payloadSize
) {
    public static Config fromArgs(String[] args) {
        int threads = 100;
        int requests = 10000;

        for (int i = 0; i < args.length - 1; i++) {
            if (args[i].equals("--threads")) {
                threads = Integer.parseInt(args[i + 1]);
            } else if (args[i].equals("--requests")) {
                requests = Integer.parseInt(args[i + 1]);
            }
        }

        return new Config("localhost", 8080, threads, requests, 1024);
    }

    public static Config defaultConfig() {
        return new Config("localhost", 8080, 100, 10000, 1024);
    }
}
