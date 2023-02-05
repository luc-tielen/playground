#pragma once
#include <array>
#include <cassert>
#include <cstdint>
#include <sys/mman.h>

using u8 = uint8_t;

static constexpr u8 ALIGNMENT = 16;

class PageAllocator {
    u8* allocate(size_t /* bytes */) {
        u8* memory = (u8*) mmap(
            nullptr, // TODO can we pass last allocated address + PAGE_SIZE into first argument for contiguous pages?
            PAGE_SIZE,
            PROT_READ | PROT_WRITE,
            MAP_ANON | MAP_NORESERVE, // TODO what other flags are interesting? MAP_HUGE_2MB, MAP_HUGE_1GB? requires more setup.. (hugetlbfs)
            -1,
            0
        );
        assert(memory);
        return memory;
    }

    void deallocate(u8* memory) {
        assert(memory);
        int result = munmap(memory, PAGE_SIZE);
        assert(result == 0);
    }

    static constexpr size_t PAGE_SIZE = 4096;
};

inline size_t make_aligned(size_t pos, u8 alignment) {
    return pos + (alignment - 1) & (~ 0x0F);
}

template<size_t num_bytes>
class alignas(ALIGNMENT) FixedBufferAllocator {
    u8* allocate(size_t size) {
        size_t next_pos = make_aligned(last_pos, ALIGNMENT);

        if (next_pos + size >= num_bytes) {
            return nullptr;
        }

        last_pos = next_pos + size;
        return buf.at(next_pos);
    }

    void deallocate(u8* /* memory */) {
        // nop
    }

    std::array<u8, num_bytes> buf;
    size_t last_pos = 0;
};
