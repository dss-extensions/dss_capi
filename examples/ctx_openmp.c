/*

This is a simple example using the DSSContext ctx_* functions from DSS C-API with OpenMP.

On Linux, compile with:

    gcc -fopenmp -O2 -I../include -L../lib/linux_x64 -ldss_capi -lklusolvex ctx_openmp.c -o ctx_openmp

Assuming electricdss-tst was cloned in the same folder as dss_capi, you could run it with something like:

    OMP_NUM_THREADS=6 LD_LIBRARY_PATH=../lib/linux_x64 time ./ctx_openmp

We also recommend testing other memory allocators such as jemalloc and mimalloc for better performance 
(varies from task to task):

    OMP_NUM_THREADS=6 LD_LIBRARY_PATH=../lib/linux_x64 LD_PRELOAD=/usr/lib64/libjemalloc.so.2 time ./ctx_openmp

For actual large scale simulations, we recommend reusing the circuit instead of reloading everything.
We reload here for conciseness.

*/
#include <stdint.h>
#include <stdio.h>
#include <omp.h>
#include "dss_capi_ctx.h"

int main(void)
{
    const int NUM_THREADS = omp_get_max_threads();
    const int NUM_TASKS = 40;
    void *contexts[NUM_THREADS];
    double kWh_losses = 0;
    int32_t num_errors = 0;
    int i;

    printf("Starting...\n");
    DSS_Start(0);
    DSS_Set_AllowChangeDir(0);
    for (i = 0; i < NUM_THREADS; ++i)
    {
        contexts[i] = ctx_New();
    }
    printf("Running tasks...\n");

    // Run a loop for LoadMults ranging from 0.1 to 2.5,
    // and sum all kWh losses for a meter.
    #pragma omp parallel for reduction(+: kWh_losses, num_errors)
    for (i = 0; i < NUM_TASKS; ++i)
    {
        // Each thread get a DSS context to work with
        void *ctx = contexts[omp_get_thread_num()];

        ctx_Text_Set_Command(ctx, "redirect ../../electricdss-tst/Version8/Distrib/EPRITestCircuits/ckt5/Master_ckt5.dss");
        ctx_Solution_Set_LoadMult(ctx, 0.1 + (2.4 * i) / (NUM_TASKS - 1));
        ctx_Text_Set_Command(ctx, "solve mode=daily number=250");
        if (ctx_Error_Get_Number(ctx) || !ctx_Solution_Get_Converged(ctx))
        {
            // Remember to check for errors, especially after loading and solving systems
            // (this is cumbersome in plain C)
            ++num_errors;
        }
        
        // Get total for the register "Zone Losses kWh" (index 12)
        int32_t count[2] = {0, 0};
        double *totals = NULL;
        ctx_Meters_Get_First(ctx);
        ctx_Meters_Get_Totals(ctx, &totals, count);
        kWh_losses += totals[12];
        DSS_Dispose_PDouble(&totals);
    }
    kWh_losses /= (NUM_TASKS - num_errors);
    printf("Done!\n\n");

    printf("Average zone losses from %d tasks: %g kWh\n", NUM_TASKS, kWh_losses);
    printf("Number of solution errors: %d\n\n", num_errors);

    for (i = 0; i < NUM_THREADS; ++i)
    {
        ctx_Dispose(contexts[i]);
    }

    return 0;
}
