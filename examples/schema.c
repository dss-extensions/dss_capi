#include <stdint.h>
#include <stdio.h>
#include "altdss/capi/dss.h"

int main(void)
{
    DSS_Start(0);
    const char* schema = DSS_ExtractSchema(NULL);
    puts(schema);
    return 0;
}
