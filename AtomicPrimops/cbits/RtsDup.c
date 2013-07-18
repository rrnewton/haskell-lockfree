#define THREADED_RTS
#undef  KEEP_INLINES

// Stg.h will pull in SMP.h:
// #include "Stg.h"
// Also having some problems with Regs.h.
// This is probably too big a chunk to suck in.

//--------------------------------------------------------------------------------
// #define EXTERN_INLINE inline
#define EXTERN_INLINE 

#include "MachDeps.h"
#include "stg/Types.h"

// Force the GHC RTS code to provide the desired symbols:
// #define IN_STGCRUN 1

// If I pull this in I get duplicated symbols:
// #include "stg/SMP.h"
//--------------------------------------------------------------------------------

/*
 * We need to tell both the compiler AND the CPU about the barriers.
 * It's no good preventing the CPU from reordering the operations if
 * the compiler has already done so - hence the "memory" restriction
 * on each of the barriers below.
 */
EXTERN_INLINE void
DUP_write_barrier(void) {
#if i386_HOST_ARCH || x86_64_HOST_ARCH
    __asm__ __volatile__ ("" : : : "memory");
#elif powerpc_HOST_ARCH
    __asm__ __volatile__ ("lwsync" : : : "memory");
#elif sparc_HOST_ARCH
    /* Sparc in TSO mode does not require store/store barriers. */
    __asm__ __volatile__ ("" : : : "memory");
#elif arm_HOST_ARCH && defined(arm_HOST_ARCH_PRE_ARMv7)
    __asm__ __volatile__ ("" : : : "memory");
#elif arm_HOST_ARCH && !defined(arm_HOST_ARCH_PRE_ARMv7)
    __asm__ __volatile__ ("dmb  st" : : : "memory");
#elif !defined(WITHSMP)
    return;
#else
#error memory barriers unimplemented on this architecture
#endif
}

EXTERN_INLINE void
DUP_store_load_barrier(void) {
#if i386_HOST_ARCH
    __asm__ __volatile__ ("lock; addl $0,0(%%esp)" : : : "memory");
#elif x86_64_HOST_ARCH
    __asm__ __volatile__ ("lock; addq $0,0(%%rsp)" : : : "memory");
#elif powerpc_HOST_ARCH
    __asm__ __volatile__ ("sync" : : : "memory");
#elif sparc_HOST_ARCH
    __asm__ __volatile__ ("membar #StoreLoad" : : : "memory");
#elif arm_HOST_ARCH && !defined(arm_HOST_ARCH_PRE_ARMv7)
    __asm__ __volatile__ ("dmb" : : : "memory");
#elif !defined(WITHSMP)
    return;
#else
#error memory barriers unimplemented on this architecture
#endif
}

EXTERN_INLINE void
DUP_load_load_barrier(void) {
#if i386_HOST_ARCH
    __asm__ __volatile__ ("" : : : "memory");
#elif x86_64_HOST_ARCH
    __asm__ __volatile__ ("" : : : "memory");
#elif powerpc_HOST_ARCH
    __asm__ __volatile__ ("lwsync" : : : "memory");
#elif sparc_HOST_ARCH
    /* Sparc in TSO mode does not require load/load barriers. */
    __asm__ __volatile__ ("" : : : "memory");
#elif arm_HOST_ARCH && !defined(arm_HOST_ARCH_PRE_ARMv7)
    __asm__ __volatile__ ("dmb" : : : "memory");
#elif !defined(WITHSMP)
    return;
#else
#error memory barriers unimplemented on this architecture
#endif
}

// Load a pointer from a memory location that might be being modified
// concurrently.  This prevents the compiler from optimising away
// multiple loads of the memory location, as it might otherwise do in
// a busy wait loop for example.
// #define VOLATILE_LOAD(p) (*((StgVolatilePtr)(p)))
