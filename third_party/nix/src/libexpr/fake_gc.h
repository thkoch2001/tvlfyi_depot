#pragma once

#ifdef DISABLE_GC

#include <new>
#include <memory>

class gc {};

enum GCPlacement
{
  GC,
  NoGC,
};

// ignore the finalizer variants
inline void* operator new(size_t size, GCPlacement gcp) {
  void* obj = malloc(size);
  if (obj == nullptr) {
    throw std::bad_alloc();
  }
  memset(obj, 0xA4, size);
  return obj;
}

template <class GC_Tp>
using traceable_allocator = std::allocator<GC_Tp>;

template <class GC_Tp>
using gc_allocator = std::allocator<GC_Tp>;

#define GC_STRDUP(x) strdup(x)
#define GC_MALLOC_UNCOLLECTABLE(x) malloc(x)
#define GC_FREE(x) free(x)
#define GC_REALLOC(ptr, sz) realloc((ptr), (sz))

#define GC_word size_t

extern "C" {

void __asan_print_accumulated_stats();

}

#endif
