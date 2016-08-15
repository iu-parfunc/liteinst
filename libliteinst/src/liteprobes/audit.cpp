
#include "audit.hpp"

std::unordered_map<uint64_t, int64_t> g_liteprobes_layouts;

uint64_t g_activation_cost = 0;
uint64_t g_deactivation_cost = 0;

uint64_t g_activation_count = 0;
uint64_t g_deactivation_count = 0;
