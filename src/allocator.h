#pragma once
#include <cassert>
#include <sys/mman.h>

class PageAllocator {
public:
    void *allocate(size_t /* bytes */) {
        void *memory = mmap(
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

    void deallocate(void *memory) {
        assert(memory);
        int result = munmap(memory, PAGE_SIZE);
        assert(result == 0);
    }

private:
    static constexpr size_t PAGE_SIZE = 4096;
};
