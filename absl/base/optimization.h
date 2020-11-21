<<<<<<< HEAD   (cce0ad chore(tazjin/emacs): Minor cleanup of unused modes and confi)
=======
//
// Copyright 2017 The Abseil Authors.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// -----------------------------------------------------------------------------
// File: optimization.h
// -----------------------------------------------------------------------------
//
// This header file defines portable macros for performance optimization.

#ifndef ABSL_BASE_OPTIMIZATION_H_
#define ABSL_BASE_OPTIMIZATION_H_

#include "absl/base/config.h"

// ABSL_BLOCK_TAIL_CALL_OPTIMIZATION
//
// Instructs the compiler to avoid optimizing tail-call recursion. Use of this
// macro is useful when you wish to preserve the existing function order within
// a stack trace for logging, debugging, or profiling purposes.
//
// Example:
//
//   int f() {
//     int result = g();
//     ABSL_BLOCK_TAIL_CALL_OPTIMIZATION();
//     return result;
//   }
#if defined(__pnacl__)
#define ABSL_BLOCK_TAIL_CALL_OPTIMIZATION() if (volatile int x = 0) { (void)x; }
#elif defined(__clang__)
// Clang will not tail call given inline volatile assembly.
#define ABSL_BLOCK_TAIL_CALL_OPTIMIZATION() __asm__ __volatile__("")
#elif defined(__GNUC__)
// GCC will not tail call given inline volatile assembly.
#define ABSL_BLOCK_TAIL_CALL_OPTIMIZATION() __asm__ __volatile__("")
#elif defined(_MSC_VER)
#include <intrin.h>
// The __nop() intrinsic blocks the optimisation.
#define ABSL_BLOCK_TAIL_CALL_OPTIMIZATION() __nop()
#else
#define ABSL_BLOCK_TAIL_CALL_OPTIMIZATION() if (volatile int x = 0) { (void)x; }
#endif

// ABSL_CACHELINE_SIZE
//
// Explicitly defines the size of the L1 cache for purposes of alignment.
// Setting the cacheline size allows you to specify that certain objects be
// aligned on a cacheline boundary with `ABSL_CACHELINE_ALIGNED` declarations.
// (See below.)
//
// NOTE: this macro should be replaced with the following C++17 features, when
// those are generally available:
//
//   * `std::hardware_constructive_interference_size`
//   * `std::hardware_destructive_interference_size`
//
// See http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0154r1.html
// for more information.
#if defined(__GNUC__)
// Cache line alignment
#if defined(__i386__) || defined(__x86_64__)
#define ABSL_CACHELINE_SIZE 64
#elif defined(__powerpc64__)
#define ABSL_CACHELINE_SIZE 128
#elif defined(__aarch64__)
// We would need to read special register ctr_el0 to find out L1 dcache size.
// This value is a good estimate based on a real aarch64 machine.
#define ABSL_CACHELINE_SIZE 64
#elif defined(__arm__)
// Cache line sizes for ARM: These values are not strictly correct since
// cache line sizes depend on implementations, not architectures.  There
// are even implementations with cache line sizes configurable at boot
// time.
#if defined(__ARM_ARCH_5T__)
#define ABSL_CACHELINE_SIZE 32
#elif defined(__ARM_ARCH_7A__)
#define ABSL_CACHELINE_SIZE 64
#endif
#endif

#ifndef ABSL_CACHELINE_SIZE
// A reasonable default guess.  Note that overestimates tend to waste more
// space, while underestimates tend to waste more time.
#define ABSL_CACHELINE_SIZE 64
#endif

// ABSL_CACHELINE_ALIGNED
//
// Indicates that the declared object be cache aligned using
// `ABSL_CACHELINE_SIZE` (see above). Cacheline aligning objects allows you to
// load a set of related objects in the L1 cache for performance improvements.
// Cacheline aligning objects properly allows constructive memory sharing and
// prevents destructive (or "false") memory sharing.
//
// NOTE: this macro should be replaced with usage of `alignas()` using
// `std::hardware_constructive_interference_size` and/or
// `std::hardware_destructive_interference_size` when available within C++17.
//
// See http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0154r1.html
// for more information.
//
// On some compilers, `ABSL_CACHELINE_ALIGNED` expands to an `__attribute__`
// or `__declspec` attribute. For compilers where this is not known to work,
// the macro expands to nothing.
//
// No further guarantees are made here. The result of applying the macro
// to variables and types is always implementation-defined.
//
// WARNING: It is easy to use this attribute incorrectly, even to the point
// of causing bugs that are difficult to diagnose, crash, etc. It does not
// of itself guarantee that objects are aligned to a cache line.
//
// NOTE: Some compilers are picky about the locations of annotations such as
// this attribute, so prefer to put it at the beginning of your declaration.
// For example,
//
//   ABSL_CACHELINE_ALIGNED static Foo* foo = ...
//
//   class ABSL_CACHELINE_ALIGNED Bar { ...
//
// Recommendations:
//
// 1) Consult compiler documentation; this comment is not kept in sync as
//    toolchains evolve.
// 2) Verify your use has the intended effect. This often requires inspecting
//    the generated machine code.
// 3) Prefer applying this attribute to individual variables. Avoid
//    applying it to types. This tends to localize the effect.
#define ABSL_CACHELINE_ALIGNED __attribute__((aligned(ABSL_CACHELINE_SIZE)))
#elif defined(_MSC_VER)
#define ABSL_CACHELINE_SIZE 64
#define ABSL_CACHELINE_ALIGNED __declspec(align(ABSL_CACHELINE_SIZE))
#else
#define ABSL_CACHELINE_SIZE 64
#define ABSL_CACHELINE_ALIGNED
#endif

// ABSL_PREDICT_TRUE, ABSL_PREDICT_FALSE
//
// Enables the compiler to prioritize compilation using static analysis for
// likely paths within a boolean branch.
//
// Example:
//
//   if (ABSL_PREDICT_TRUE(expression)) {
//     return result;                        // Faster if more likely
//   } else {
//     return 0;
//   }
//
// Compilers can use the information that a certain branch is not likely to be
// taken (for instance, a CHECK failure) to optimize for the common case in
// the absence of better information (ie. compiling gcc with `-fprofile-arcs`).
//
// Recommendation: Modern CPUs dynamically predict branch execution paths,
// typically with accuracy greater than 97%. As a result, annotating every
// branch in a codebase is likely counterproductive; however, annotating
// specific branches that are both hot and consistently mispredicted is likely
// to yield performance improvements.
#if ABSL_HAVE_BUILTIN(__builtin_expect) || \
    (defined(__GNUC__) && !defined(__clang__))
#define ABSL_PREDICT_FALSE(x) (__builtin_expect(false || (x), false))
#define ABSL_PREDICT_TRUE(x) (__builtin_expect(false || (x), true))
#else
#define ABSL_PREDICT_FALSE(x) (x)
#define ABSL_PREDICT_TRUE(x) (x)
#endif

// ABSL_INTERNAL_ASSUME(cond)
// Informs the compiler that a condition is always true and that it can assume
// it to be true for optimization purposes. The call has undefined behavior if
// the condition is false.
// In !NDEBUG mode, the condition is checked with an assert().
// NOTE: The expression must not have side effects, as it will only be evaluated
// in some compilation modes and not others.
//
// Example:
//
//   int x = ...;
//   ABSL_INTERNAL_ASSUME(x >= 0);
//   // The compiler can optimize the division to a simple right shift using the
//   // assumption specified above.
//   int y = x / 16;
//
#if !defined(NDEBUG)
#define ABSL_INTERNAL_ASSUME(cond) assert(cond)
#elif ABSL_HAVE_BUILTIN(__builtin_assume)
#define ABSL_INTERNAL_ASSUME(cond) __builtin_assume(cond)
#elif defined(__GNUC__) || ABSL_HAVE_BUILTIN(__builtin_unreachable)
#define ABSL_INTERNAL_ASSUME(cond)        \
  do {                                    \
    if (!(cond)) __builtin_unreachable(); \
  } while (0)
#elif defined(_MSC_VER)
#define ABSL_INTERNAL_ASSUME(cond) __assume(cond)
#else
#define ABSL_INTERNAL_ASSUME(cond)      \
  do {                                  \
    static_cast<void>(false && (cond)); \
  } while (0)
#endif

// ABSL_INTERNAL_UNIQUE_SMALL_NAME(cond)
// This macro forces small unique name on a static file level symbols like
// static local variables or static functions. This is intended to be used in
// macro definitions to optimize the cost of generated code. Do NOT use it on
// symbols exported from translation unit since it may casue a link time
// conflict.
//
// Example:
//
// #define MY_MACRO(txt)
// namespace {
//  char VeryVeryLongVarName[] ABSL_INTERNAL_UNIQUE_SMALL_NAME() = txt;
//  const char* VeryVeryLongFuncName() ABSL_INTERNAL_UNIQUE_SMALL_NAME();
//  const char* VeryVeryLongFuncName() { return txt; }
// }
//

#if defined(__GNUC__)
#define ABSL_INTERNAL_UNIQUE_SMALL_NAME2(x) #x
#define ABSL_INTERNAL_UNIQUE_SMALL_NAME1(x) ABSL_INTERNAL_UNIQUE_SMALL_NAME2(x)
#define ABSL_INTERNAL_UNIQUE_SMALL_NAME() \
  asm(ABSL_INTERNAL_UNIQUE_SMALL_NAME1(.absl.__COUNTER__))
#else
#define ABSL_INTERNAL_UNIQUE_SMALL_NAME()
#endif

#endif  // ABSL_BASE_OPTIMIZATION_H_
>>>>>>> BRANCH (5ccfe8 Squashed 'third_party/abseil_cpp/' changes from ccdbb5941..e)
