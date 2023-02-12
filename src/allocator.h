#pragma once
#include <array>
#include <vector>
#include <cassert>
#include <cstdint>
#include <sys/mman.h>

// NOTE: This is only POC code, mostly intended to figure out the algorithms before porting to LLVM.

using u8 = uint8_t;

static constexpr u8 ALIGNMENT = 16;

class PageAllocator {
    u8* allocate(size_t /* bytes */) {
        u8* memory = (u8*) mmap(
            nullptr, // TODO can we pass last allocated address + PAGE_SIZE into first argument for contiguous pages?
            PAGE_SIZE, // TODO allow multiple of page size by passing in bytes here, and doing an assert on bytes % PAGE_SIZE == 0?
            PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANON | MAP_NORESERVE, // TODO what other flags are interesting? MAP_HUGE_2MB, MAP_HUGE_1GB? requires more setup.. (hugetlbfs)
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

    static constexpr size_t PAGE_SIZE = 4096; //64kB for WASM
};

inline size_t make_aligned(size_t pos, u8 alignment) {
    return pos + (alignment - 1) & (~ 0x0F);
}

// TODO: also allow "resetting", to avoid freeing a lot of memory just to reallocate it again
template <typename Alloc, size_t num_bytes>
class ArenaAllocator {
    ArenaAllocator()
        : alloc()
        , current(nullptr)
        , last_pos(0)
        , ptrs() {}

    ~ArenaAllocator() {
        for (const auto& ptr: ptrs) {
            alloc.deallocate(ptr);
        }
    }

    // TODO how to handle sizes larger than e.g. page size?
    u8* allocate(size_t size) {
        // First allocation, can we speed this up? or do it unconditionally in constructor?
        [[unlikely]]
        if (current == nullptr) {
            return allocate_chunk();
        }

        size_t next_pos = make_aligned(last_pos, ALIGNMENT);
        if (next_pos + size >= num_bytes) {
            last_pos = 0;
            return allocate_chunk();
        }

        last_pos = next_pos + size;
        return current + next_pos;
    }

    inline u8* allocate_chunk() {
        current = alloc.allocate(num_bytes);
        ptrs.push_back(current);
        return current;
    }

    void deallocate(u8* memory) {
        // nop, all memory freed in destructor
    }

    Alloc alloc;
    u8* current;
    size_t last_pos;
    std::vector<u8*> ptrs;
};

template<size_t num_bytes>
class alignas(ALIGNMENT) FixedBufferAllocator {
    u8* allocate(size_t size) {
        // NOTE: this should work, since struct itself is aligned, so the start of the array is also aligned
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

// TODO slab allocator
