# Load Testing Report

## Overview  
This project measures the performance of the HTTP server from lab‑2 and the JSON parser from lab‑1 under four configurations:  
• Threading model: Classic thread pool vs Virtual threads (Project Loom)  
• JSON parser: custom implementation vs Google Gson  

Two scenarios are tested:  
1. Request‑1 (POST /request1)  
   – Body: {"id":"…","payload":"…"}  
   – Server parses JSON, stores (id, payload) in on‑disk SQLite, reads it back, returns the payload string  
2. Request‑2 (POST /request2)  
   – Body: JSON array of 100 integers  
   – Server parses JSON array, computes the average, returns {"average":…}  

Load tests are driven by a custom Java client (org.loadtesting.LoadTestRun) and a shell script (run_all.sh) that executes all four configurations in sequence and writes results into separate CSV files.

## Project Structure  
Lab3_Salimli  
  app  
    build.gradle.kts          Gradle build and application configuration  
    src/main/java  
      org.httpServerAyzek     HTTP server implementation  
      org.jsonparser          Custom JSON parser implementation  
      org.loadtesting         Load test client, Config, CsvWriter, ErrorLogger, run logic  
  run_all.sh                  Shell script to run all four configurations  
  README.txt                  This plain‑text document  

## Prerequisites  
• Java 21  
• Gradle 8.x  
• SQLite JDBC driver (org.xerial:sqlite-jdbc:3.46.0.0)  

## Building  
Run  
  ./gradlew clean build  

## Running the HTTP Server  
Choose threading model and parser via JVM system properties:  
  Classic threads + custom parser:  
    ./gradlew :app:runServer -Dthreads=classic -Dparser=own  
  Virtual threads + custom parser:  
    ./gradlew :app:runServer -Dthreads=virtual -Dparser=own  
  Classic threads + Gson parser:  
    ./gradlew :app:runServer -Dthreads=classic -Dparser=gson  
  Virtual threads + Gson parser:  
    ./gradlew :app:runServer -Dthreads=virtual -Dparser=gson  

The server listens on localhost:8080. Leave this terminal open.

## Running Load Tests  
In a separate terminal, launch the load‑test client. By default it uses 100 threads and 5000 requests per scenario:  
  ./gradlew :app:run --args="--threads 100 --requests 5000"  

To run all four configurations in sequence and collect separate CSVs:  
  chmod +x run_all.sh  
  ./run_all.sh  

After execution, you will find these files in the project root:  
  results-Classic-Own.csv  
  results-Virtual-Own.csv  
  results-Classic-Gson.csv  
  results-Virtual-Gson.csv  
  errors-Classic-Own.csv  
  errors-Virtual-Own.csv  
  errors-Classic-Gson.csv  
  errors-Virtual-Gson.csv  

Each results-*.csv has columns:  
  Scenario,AvgTimeMillis,P95Millis,Errors  

Each errors-*.csv has columns:  
  Scenario,Index,Time,ErrorMessage  

## Experiment Description  
• Request‑1: JSON → SQLite (on‑disk) → JSON  
• Request‑2: JSON array → in‑memory compute → JSON  
• Metrics: average response time (AvgTimeMillis), 95th percentile (P95Millis), error count  
• Configurations: Classic vs Virtual threads × custom parser vs Gson  
• Default run parameters: 100 concurrent threads, 5000 requests per scenario, 1 KiB JSON payload, 100‑element array  

## Hardware  
Role     CPU               RAM   OS  
Server   Apple M1 8‑core   16 GB macOS  
Client   Apple M1 8‑core   16 GB macOS  

Network: loopback (localhost)

## Parameters  
• Threads: 100 (override with --threads)  
• Requests: 5000 per scenario (override with --requests)  
• Payload size: 1024 bytes  
• Array length: 100 integers  

## Results Summary  
Scenario   | Virtual + Own         | Virtual + Gson         | Classic + Own          | Classic + Gson  
Request‑1  | 126.75 ms (p95 180.33) | 127.81 ms (p95 220.72) | 141.73 ms (p95 287.74)  | 129.65 ms (p95 229.23)  
Request‑2  | 14.07 ms  (p95 24.21)  | 10.85 ms  (p95 22.23)  | 12.90 ms  (p95 23.64)   | 14.46 ms  (p95 29.21)   

## Charts  
Performance charts (PNG/PDF) are included in the repository:  
  performance_avg.png  
  performance_grouped.png  
  detailed_performance_dualaxis.png  

They display average and 95th percentile timings for both scenarios across all four configurations.

## Conclusion  
This setup fully satisfies the assignment requirements without using an in‑memory database or external load‑testing frameworks. The methodology is reproducible and all raw data (CSV) and visualizations (PNG/PDF) are included.# Load testing
