// src/main.cpp
#include <mpi.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <unordered_map>
#include <cstring>
#include <thread>
#include <chrono>
#include <algorithm>
#include "include/timers.h"
#include "include/csv_utils.h"
#include "include/mpi_utils.h"

using namespace std;

enum Tags {
    TAG_REQUEST = 1,
    TAG_BATCH_SIZE = 11,
    TAG_BATCH_DATA = 12,
    TAG_AGG_COUNT = 100,
    TAG_AGG_DATA = 101,
    TAG_HEADER_MAP = 200
};

static void send_header_map(int dest, const vector<string> &headers) {
    // serialize header names separated by \n, send size then data
    string payload;
    for (size_t i = 0; i < headers.size(); ++i) {
        if (i) payload.push_back('\n');
        payload += headers[i];
    }
    int32_t nbytes = (int32_t)payload.size();
    MPI_Send(&nbytes, 1, MPI_INT, dest, TAG_HEADER_MAP, MPI_COMM_WORLD);
    if (nbytes > 0) MPI_Send(payload.data(), nbytes, MPI_BYTE, dest, TAG_HEADER_MAP+1, MPI_COMM_WORLD);
}

static void recv_header_map(int src, vector<string> &headers) {
    MPI_Status st;
    int32_t nbytes;
    MPI_Recv(&nbytes, 1, MPI_INT, src, TAG_HEADER_MAP, MPI_COMM_WORLD, &st);
    headers.clear();
    if (nbytes > 0) {
        string buf; buf.resize(nbytes);
        MPI_Recv(&buf[0], nbytes, MPI_BYTE, src, TAG_HEADER_MAP+1, MPI_COMM_WORLD, &st);
        // split by \n
        stringstream ss(buf);
        string line;
        while (std::getline(ss, line)) headers.push_back(line);
    }
}

// helper: serialize vector<string> lines -> buffer
static void serialize_lines(const vector<string> &lines, string &out) {
    // simple: store each line with its length (4 bytes) + data
    out.clear();
    for (const auto &s : lines) {
        uint32_t L = (uint32_t)s.size();
        out.append(reinterpret_cast<const char*>(&L), sizeof(L));
        out.append(s);
    }
}
static void deserialize_lines(const string &buf, vector<string> &out) {
    out.clear();
    size_t pos = 0;
    while (pos + sizeof(uint32_t) <= buf.size()) {
        uint32_t L;
        memcpy(&L, buf.data() + pos, sizeof(uint32_t));
        pos += sizeof(uint32_t);
        if (pos + L > buf.size()) break;
        out.emplace_back(buf.data() + pos, L);
        pos += L;
    }
}

// master merges received aggregates
static void merge_into_global(unordered_map<int64_t, AggregateRecord> &global, const vector<AggregateRecord> &v) {
    for (const auto &r : v) {
        auto it = global.find(r.bin);
        if (it == global.end()) {
            global[r.bin] = r;
        } else {
            it->second.count += r.count;
            it->second.count_neg_v7 += r.count_neg_v7;
            it->second.sum_v11 += r.sum_v11;
            it->second.total_amount += r.total_amount;
        }
    }
}

int main(int argc, char **argv) {
    MPI_Init(&argc, &argv);
    int rank, world;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &world);

    // read env
    int batch_size = atoi(getenv("BATCH_SIZE") ? getenv("BATCH_SIZE") : "20000");
    double gpu_fraction = atof(getenv("GPU_FRACTION") ? getenv("GPU_FRACTION") : "0.8");
    int omp_threads = atoi(getenv("WORKER_THREADS") ? getenv("WORKER_THREADS") : "8");
    string log_dir = getenv("LOG_DIR") ? getenv("LOG_DIR") : string("./logs");
    string csv_path = "./creditcard_2023.csv";

    // prepare logging file for master/worker
    Timer t_all;
    TimingsLogger logger;
    logger.open(log_dir + "/timings_rank_" + to_string(rank) + ".log");
    logger.log("startup", 0.0);

    if (rank == 0) {
        // MASTER
        cout << "Master starting. world=" << world << " batch_size=" << batch_size << "\n";

        // open CSV and read header
        ifstream fin(csv_path);
        if (!fin.is_open()) {
            cerr << "Master: cannot open " << csv_path << "\n";
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
        string header_line;
        if (!getline(fin, header_line)) {
            cerr << "Master: empty CSV header\n";
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
        vector<string> headers = split_csv_line(header_line);

        // send header map to all workers
        for (int dest = 1; dest < world; ++dest) {
            send_header_map(dest, headers);
        }

        // read file in streaming batches, but only send when workers request
        // We'll maintain a queue of batches (vector<string>) and respond to requests.
        // Implementation: master listens for request messages (blocking recv with MPI_ANY_SOURCE, tag=TAG_REQUEST)
        // When receives request: if more batches available, serialize next and send; else send nbytes=0 => EOF
        // We will produce batches by reading lines from file as requests arrive to avoid memory blowup.

        // Preload nothing; read on demand.
        bool eof_reached = false;
        unordered_map<int64_t, AggregateRecord> globalAgg; // bin->record
        int active_workers = world - 1;

        // Use MPI_Status to get source
        MPI_Status st;
        while (active_workers > 0) {
            // wait for request
            int32_t req_tag_int;
            MPI_Recv(&req_tag_int, 1, MPI_INT, MPI_ANY_SOURCE, TAG_REQUEST, MPI_COMM_WORLD, &st);
            int src = st.MPI_SOURCE;
            double t_recv_req = t_all.elapsed();
            logger.log("recv_request_from_rank", t_recv_req);

            // prepare next batch
            vector<string> batch_lines;
            batch_lines.reserve(batch_size);
            string line;
            int lines_read = 0;
            while (lines_read < batch_size && getline(fin, line)) {
                batch_lines.push_back(line);
                ++lines_read;
            }
            if (lines_read == 0) {
                // EOF: send nbytes=0 to signal termination
                int32_t nbytes = 0;
                MPI_Send(&nbytes, 1, MPI_INT, src, TAG_BATCH_SIZE, MPI_COMM_WORLD);
                // still expect this worker to send possibly pending aggregates later; but we'll decrement active_workers when worker signals finish by sending a special finalization message.
                // We'll define: worker, after receiving nbytes=0, will send its final aggregates and then send a special DONE_INT (int 1) with tag TAG_REQUEST to inform master it's done.
                // Wait to receive final aggregates or DONE: We'll continue listening.
            } else {
                // serialize and send
                string payload;
                serialize_lines(batch_lines, payload);
                int32_t nbytes = (int32_t)payload.size();
                // send size
                MPI_Send(&nbytes, 1, MPI_INT, src, TAG_BATCH_SIZE, MPI_COMM_WORLD);
                // send data if any
                if (nbytes > 0) {
                    MPI_Send(payload.data(), nbytes, MPI_BYTE, src, TAG_BATCH_DATA, MPI_COMM_WORLD);
                }
            }

            // After responding to request, non-blockingly probe for aggregate messages from any source
            // We will probe in a small loop to receive any aggregates available (tag TAG_AGG_COUNT)
            int flag = 0;
            MPI_Status st2;
            do {
                MPI_Iprobe(MPI_ANY_SOURCE, TAG_AGG_COUNT, MPI_COMM_WORLD, &flag, &st2);
                if (flag) {
                    // recv aggregates from st2.MPI_SOURCE
                    vector<AggregateRecord> recs;
                    recv_aggregates(st2.MPI_SOURCE, recs, MPI_COMM_WORLD);
                    merge_into_global(globalAgg, recs);
                    logger.log("recv_aggregates_and_merge", t_all.elapsed());
                }
            } while (flag);
            // Continue loop until all workers finished (we detect via separate DONE messages below)
            // To detect DONE: we will use probe for a special zero-length batch? Alternatively, worker will send final empty aggregates with count=0 and set a DONE flag via TAG_REQUEST with int 1
            // Let's probe for DONE markers
            int flag_done = 0;
            MPI_Iprobe(MPI_ANY_SOURCE, TAG_REQUEST, MPI_COMM_WORLD, &flag_done, &st2);
            if (flag_done) {
                // recv it
                int32_t marker;
                MPI_Recv(&marker, 1, MPI_INT, st2.MPI_SOURCE, TAG_REQUEST, MPI_COMM_WORLD, &st2);
                if (marker == 2) {
                    // worker informs termination (sent after receiving nbytes=0 and after sending final aggregates)
                    active_workers--;
                    logger.log("worker_terminated", t_all.elapsed());
                } else {
                    // unexpected, push back? ignore
                }
            }
        } // end while active_workers

        // all workers done, now globalAgg contains aggregated results
        // Convert to vector for selection
        struct GroupOut { int64_t bin; long double mean_v11; long double total_amount; int64_t count; int64_t neg; };
        vector<GroupOut> groups;
        groups.reserve(globalAgg.size());
        for (auto &p : globalAgg) {
            AggregateRecord rec = p.second;
            GroupOut g;
            g.bin = rec.bin;
            g.count = rec.count;
            g.neg = rec.count_neg_v7;
            g.mean_v11 = (long double)rec.sum_v11 / (long double)rec.count;
            g.total_amount = (long double)rec.total_amount;
            groups.push_back(g);
        }

        // selection logic:
        // 1) sort by neg desc (and mean_v11 as tie-breaker)
        sort(groups.begin(), groups.end(), [](const GroupOut &a, const GroupOut &b){
            if (a.neg != b.neg) return a.neg > b.neg;
            return a.mean_v11 > b.mean_v11;
        });
        // accumulate levels until >=50 candidates
        vector<GroupOut> candidates;
        size_t i = 0;
        while (i < groups.size() && candidates.size() < 50) {
            int64_t cur_neg = groups[i].neg;
            size_t j = i;
            while (j < groups.size() && groups[j].neg == cur_neg) {
                candidates.push_back(groups[j]);
                ++j;
            }
            i = j;
        }
        // choose top50 by mean_v11 desc
        sort(candidates.begin(), candidates.end(), [](const GroupOut &a, const GroupOut &b){
            if (a.mean_v11 != b.mean_v11) return a.mean_v11 > b.mean_v11;
            return a.total_amount > b.total_amount;
        });
        if (candidates.size() > 50) candidates.resize(50);
        // final sort by total_amount desc
        sort(candidates.begin(), candidates.end(), [](const GroupOut &a, const GroupOut &b){
            return a.total_amount > b.total_amount;
        });

        // write result to CSV
        ofstream fout("top50_groups.csv");
        fout << "bin_start,bin_end,bin_index,count,count_neg_v7,mean_V11,total_amount\n";
        fout.setf(std::ios::fixed);
        fout.precision(10);
        for (auto &g : candidates) {
            double start = (double)(g.bin * 0.1L);
            double end = start + 0.1;
            fout << start << "," << end << "," << g.bin << "," << g.count << "," << g.neg << "," << (double)g.mean_v11 << "," << (double)g.total_amount << "\n";
        }
        fout.close();
        cout << "Master: finished. top50 written to top50_groups.csv\n";
        logger.log("master_total", t_all.elapsed());
    } else {
        // WORKER
        // receive header map
        vector<string> headers;
        recv_header_map(0, headers);
        // map header to indices
        unordered_map<string,int> hmap;
        for (size_t i = 0; i < headers.size(); ++i) hmap[headers[i]] = (int)i;
        // check indices needed
        int idx_v1 = -1, idx_v7 = -1, idx_v11 = -1, idx_amount = -1;
        if (hmap.count("V1")) idx_v1 = hmap["V1"];
        if (hmap.count("V7")) idx_v7 = hmap["V7"];
        if (hmap.count("V11")) idx_v11 = hmap["V11"];
        if (hmap.count("Amount")) idx_amount = hmap["Amount"];
        WorkerConfig cfg;
        cfg.rank = rank;
        cfg.world_size = world;
        cfg.batch_size = batch_size;
        cfg.gpu_fraction = gpu_fraction;
        cfg.omp_threads = omp_threads;
        cfg.log_dir = log_dir;
        cfg.has_gpu = (rank >= 2); // simple mapping; adjust as needed
        cfg.idx_v1 = idx_v1; cfg.idx_v7 = idx_v7; cfg.idx_v11 = idx_v11; cfg.idx_amount = idx_amount;
        Worker worker(cfg);

        // main loop: request batches until master sends nbytes=0
        bool running = true;
        while (running) {
            // send request int to master
            int32_t req = 1;
            MPI_Send(&req, 1, MPI_INT, 0, TAG_REQUEST, MPI_COMM_WORLD);

            // receive batch size
            MPI_Status streq;
            int32_t nbytes = 0;
            MPI_Recv(&nbytes, 1, MPI_INT, 0, TAG_BATCH_SIZE, MPI_COMM_WORLD, &streq);
            if (nbytes == 0) {
                // no more work: send final aggregates then send DONE marker
                auto tosend = worker.export_aggregates_and_clear();
                // send aggregates to master (non-blocking)
                if (!tosend.empty()) {
                    MPI_Request reqs;
                    MPI_Isend((void*)&(int32_t){(int32_t)tosend.size()}, 0, MPI_BYTE, 0, TAG_AGG_COUNT, MPI_COMM_WORLD, &reqs); // dummy - keep protocol consistent
                    // use helper
                    send_aggregates_nonblocking(0, tosend, &reqs, MPI_COMM_WORLD);
                } else {
                    // still send count=0
                    int32_t zero = 0;
                    MPI_Send(&zero, 1, MPI_INT, 0, TAG_AGG_COUNT, MPI_COMM_WORLD);
                }
                // send DONE marker (int=2) on TAG_REQUEST
                int32_t done = 2;
                MPI_Send(&done, 1, MPI_INT, 0, TAG_REQUEST, MPI_COMM_WORLD);
                running = false;
                break;
            } else {
                // receive batch data
                string buf;
                buf.resize(nbytes);
                MPI_Recv(&buf[0], nbytes, MPI_BYTE, 0, TAG_BATCH_DATA, MPI_COMM_WORLD, &streq);
                vector<string> lines;
                deserialize_lines(buf, lines);
                // process lines
                worker.process_batch_lines(lines);
                // export partial aggregates and send to master (non-blocking)
                auto recs = worker.export_aggregates_and_clear();
                if (!recs.empty()) {
                    MPI_Request req;
                    // send count first
                    int32_t rec_n = (int32_t)recs.size();
                    MPI_Send(&rec_n, 1, MPI_INT, 0, TAG_AGG_COUNT, MPI_COMM_WORLD);
                    // send data
                    MPI_Isend((void*)recs.data(), recs.size() * sizeof(AggregateRecord), MPI_BYTE, 0, TAG_AGG_DATA, MPI_COMM_WORLD, &req);
                    // we don't wait here; continue to next request (master will receive when ready)
                } else {
                    int32_t zero = 0;
                    MPI_Send(&zero, 1, MPI_INT, 0, TAG_AGG_COUNT, MPI_COMM_WORLD);
                }
            }
        } // end worker loop

        logger.log("worker_total", t_all.elapsed());
    }

    MPI_Finalize();
    return 0;
}
