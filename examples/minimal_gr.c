#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "dss_capi.h"

int main(void)
{
    float** data_PSingle;
    double** data_PDouble;
    int32_t** data_PInteger;
    int8_t** data_PByte;
    int64_t* count_PSingle;
    int64_t* count_PDouble;
    int64_t* count_PInteger;
    int64_t* count_PByte;
    
    double* voltages;
    int numNodes;
    int i;
    int32_t *errorPtr;
    
    DSS_Start(0);
    DSS_GetGRPointers(
        &data_PSingle,
        &data_PDouble,
        &data_PInteger,
        &data_PByte,
        &count_PSingle,
        &count_PDouble,
        &count_PInteger,
        &count_PByte
    );
    
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
    Circuit_Get_AllBusVolts_GR();
    
    // The result for Circuit_Get_AllBusVolts is now in 
    // dataPtr_PDouble[0] and countPtr_PDouble
    
    // Copy just the pointer for convenience, the GR mechanism
    // in Pascal is responsible for the allocated memory.
    voltages = data_PDouble[0];
    
    numNodes = count_PDouble[0]/2;
    if (numNodes == 0)
    {
        return -1;
    }
    for (i = 0; i < numNodes; ++i)
    {
        printf("node %d: %f + j%f\n", i, voltages[2*i], voltages[2*i + 1]);
    }
    
    return 0;
}
