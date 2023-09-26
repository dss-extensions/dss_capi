#include <stdint.h>
#include <stdio.h>
#include "dss_capi.h"

int main(void)
{
    DSS_Start(0);
    const char* schema = DSS_ExtractSchema(NULL);
    puts(schema);
    return 0;
}
