#pragma once

#include <sys/resource.h>

#include "graph.h"
#include "common.h"

typedef void (*StressOperation_t)(uint32_t, const char*, const char*, const char*);

struct rusage differential_traversal_measurement(GraphHandle, PersistedGraph, TraversalAlgo_t);
uint32_t stress_evaluate_runtime_complexity_helper(uint32_t, GraphBuilder_t, TraversalAlgo_t, ChronoId, char*, uint32_t);
void stress_profile_algo(uint32_t, PersistedGraph, GraphBuilder_t, TraversalAlgo_t);

void stress_runtime_complexity_all_algo(uint32_t, const char*, const char*, const char*);
void stress_profile_algo_on_graph_type(uint32_t, const char*, const char*, const char*);

