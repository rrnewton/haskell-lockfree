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



/* 
 * CMPXCHG - the single-word atomic compare-and-exchange instruction.  Used 
 * in the STM implementation.
 */
EXTERN_INLINE StgWord
DUP_cas(StgVolatilePtr p, StgWord o, StgWord n)
{
#if i386_HOST_ARCH || x86_64_HOST_ARCH
    __asm__ __volatile__ (
 	  "lock\ncmpxchg %3,%1"
          :"=a"(o), "=m" (*(volatile unsigned int *)p) 
          :"0" (o), "r" (n));
    return o;
#elif powerpc_HOST_ARCH
    StgWord result;
    __asm__ __volatile__ (
        "1:     lwarx     %0, 0, %3\n"
        "       cmpw      %0, %1\n"
        "       bne       2f\n"
        "       stwcx.    %2, 0, %3\n"
        "       bne-      1b\n"
        "2:"
        :"=&r" (result)
        :"r" (o), "r" (n), "r" (p)
        :"cc", "memory"
    );
    return result;
#elif sparc_HOST_ARCH
    __asm__ __volatile__ (
	"cas [%1], %2, %0"
	: "+r" (n)
	: "r" (p), "r" (o)
	: "memory"
    );
    return n;
#elif arm_HOST_ARCH && defined(arm_HOST_ARCH_PRE_ARMv6)
    StgWord r;
    arm_atomic_spin_lock();
    r  = *p; 
    if (r == o) { *p = n; } 
    arm_atomic_spin_unlock();
    return r;
#elif arm_HOST_ARCH && !defined(arm_HOST_ARCH_PRE_ARMv6)
    StgWord result,tmp;

    __asm__ __volatile__(
        "1:     ldrex   %1, [%2]\n"
        "       mov     %0, #0\n"
        "       teq     %1, %3\n"
        "       it      eq\n"
        "       strexeq %0, %4, [%2]\n"
        "       teq     %0, #1\n"
        "       it      eq\n"
        "       beq     1b\n"
#if !defined(arm_HOST_ARCH_PRE_ARMv7)
        "       dmb\n"
#endif
                : "=&r"(tmp), "=&r"(result)
                : "r"(p), "r"(o), "r"(n)
                : "cc","memory");

    return result;
#elif !defined(WITHSMP)
    StgWord result;
    result = *p;
    if (result == o) {
        *p = n;
    }
    return result;
#else
#error cas() unimplemented on this architecture
#endif
}



// Copied from atomic_inc in the GHC RTS, except tweaked to allow
// arbitrary increments (other than 1).
EXTERN_INLINE StgWord
atomic_inc_with(StgWord incr, StgVolatilePtr p)
{
#if defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH)
    StgWord r;
    r = incr;
    __asm__ __volatile__ (
        "lock\nxadd %0,%1":
            "+r" (r), "+m" (*p):
    );
    return r + incr;
#else
    StgWord old, new;
    do {
        old = *p;
        new = old + incr;
    } while (DUP_cas(p, old, new) != old);
    return new;
#endif
}
