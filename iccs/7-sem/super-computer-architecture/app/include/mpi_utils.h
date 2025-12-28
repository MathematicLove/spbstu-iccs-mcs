// include/mpi_utils.h
#pragma once
#include <mpi.h>
#include <vector>
#include <cstdint>

// AggregateRecord: bin, count, count_neg_v7, sum_v11, total_amount
#pragma pack(push,1)
struct AggregateRecord {
    int64_t bin;
    int64_t count;
    int64_t count_neg_v7;
    double sum_v11;
    double total_amount;
};
#pragma pack(pop)

// helper send/recv using MPI_BYTE
inline void send_aggregates_nonblocking(int dest, const std::vector<AggregateRecord> &recs, MPI_Request *reqs_out = nullptr, MPI_Comm comm = MPI_COMM_WORLD) {
    int32_t n = (int32_t)recs.size();
    MPI_Send(&n, 1, MPI_INT, dest, 100, comm);
    if (n > 0) {
        MPI_Request req;
        MPI_Isend((void*)recs.data(), n * sizeof(AggregateRecord), MPI_BYTE, dest, 101, comm, &req);
        if (reqs_out) *reqs_out = req;
        else MPI_Request_free(&req); // fire-and-forget
    }
}
inline void recv_aggregates(int src, std::vector<AggregateRecord> &out, MPI_Comm comm = MPI_COMM_WORLD) {
    MPI_Status st;
    int32_t n;
    MPI_Recv(&n, 1, MPI_INT, src, 100, comm, &st);
    out.clear();
    if (n > 0) {
        out.resize(n);
        MPI_Recv((void*)out.data(), n * sizeof(AggregateRecord), MPI_BYTE, src, 101, comm, &st);
    }
}
