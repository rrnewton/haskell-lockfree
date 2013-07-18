#define THREADED_RTS
#undef  KEEP_INLINES

// Stg.h will pull in SMP.h:
// #include "Stg.h"
// Also having some problems with Regs.h.

//--------------------------------------------------------------------------------
#define EXTERN_INLINE inline

#include "MachDeps.h"
#include "stg/Types.h"

// Force the GHC RTS code to provide the desired symbols:
// #define IN_STGCRUN 1

// If I pull this in I get duplicated symbols:
#include "stg/SMP.h"
//--------------------------------------------------------------------------------


StgWord DUP_cas(StgVolatilePtr p, StgWord o, StgWord n)
{ 
  return cas(p,o,n);
}

void DUP_store_load_barrier() { store_load_barrier(); }
void DUP_load_load_barrier () { load_load_barrier(); }
void DUP_write_barrier     () { write_barrier(); }
