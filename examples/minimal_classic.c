#include <stdint.h>
#include <stdio.h>
#include "dss_capi.h"

int main(void)
{
    // For numVoltages, first `int` is the current count, 
    // the second `int` is the allocated capacity.
    int numVoltages[2] = {0, 0}; 
    double *voltages = NULL;
    int numNodes;
    int i;

    DSS_Start(0);
    Text_Set_Command("compile master.dss");
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
    
    return 0;
}
