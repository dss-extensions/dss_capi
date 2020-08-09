#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "dss_capi.h"

int main(void)
{
    // For numVoltages, first `int` is the current count, 
    // the second `int` is the allocated capacity.
    int64_t numVoltages[2] = {0, 0}; 
    double *voltages = NULL;
    int numNodes;
    int i;
    int32_t* errorPtr;

    DSS_Start(0);
    errorPtr = Error_Get_NumberPtr();
    
    Text_Set_Command("compile master.dss");
    // Ideally you should check for errors for every significant API call.
    // For conciseness, we'll only check it here. Run in a folder without
    // a "master.dss" to see it working
    if (*errorPtr)
    {
        printf(Error_Get_Description());
        printf("\n");
        return EXIT_FAILURE;
    }
    
    Solution_Solve();
    Circuit_Get_AllBusVolts(&voltages, numVoltages);

    if (numVoltages[0] == 0)
    {
        return -1;
    }
    numNodes = numVoltages[0] / 2;
    
    for (i = 0; i < numNodes; ++i)
    {
        printf("node %d: %f + j%f\n", i, voltages[2*i], voltages[2*i + 1]);
    }

    // Before v0.10.0, if you needed to recall Circuit_Get_AllBusVolts,
    // you would need to call
    
    // DSS_Dispose_PDouble(&voltages);
    
    // Since v0.10.0, even if the circuit changes, you can still use the same
    // pointer.
    
    // If the current count of doubles return by AllBusVolts can fit the 
    // previous allocated capacity, no allocations are needed!
    
    Solution_Solve();
    Circuit_Get_AllBusVolts(&voltages, numVoltages);
   
    // As the memory for voltages is allocated in Pascal, we need to
    // free it there, hence the call to DSS_Dispose_PDouble
    DSS_Dispose_PDouble(&voltages);
    
    return EXIT_SUCCESS;
}
