#include <stdint.h>

#include "tb_client.h"

TB_STATUS hs_tb_client_init(
    tb_client_t* out_client,
    tb_uint128_t* cluster_id,
    const char* address_ptr,
    uint32_t address_len,
    uintptr_t on_completion_ctx,
    void (*on_completion)(uintptr_t, tb_client_t, tb_packet_t*, const uint8_t*, uint32_t)
);
