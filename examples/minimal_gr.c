#include <stdint.h>
#include <stdio.h>
#include "dss_capi.h"

int main(void)
{
    char*** data_PPAnsiChar;
    double** data_PDouble;
    int32_t** data_PInteger;
    int8_t** data_PByte;
    int32_t* count_PPAnsiChar;
    int32_t* count_PDouble;
    int32_t* count_PInteger;
    int32_t* count_PByte;
    
    double* voltages;
    int numNodes;
    int i;
    
    DSS_Start(0);
    DSS_GetGRPointers(
        &data_PPAnsiChar,
        &data_PDouble,
        &data_PInteger,
        &data_PByte,
        &count_PPAnsiChar,
        &count_PDouble,
        &count_PInteger,
        &count_PByte
    );
    
    Text_Set_Command("compile master.dss");
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
