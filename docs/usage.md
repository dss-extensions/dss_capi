# Using DSS C-API

Basic knowledge of OpenDSS is assumed for using DSS C-API, especially since we will reference the official COM interface. Currently, in both COM and C-API, OpenDSS is exposed as a global instance. Most of the element iteration affects the state of this global instance, usually meaning that one element of a certain class can be activated and accessed at a point in time.

In both interfaces, there are three main classes of functions:
- Simple property access
- Array property access
- General functions

## Simple properties

All simple properties are implemented as a pair of functions. Some properties only have one function, indicating they are either read-only or write-only. 

```c
    // An example of property which uses the `double` type
    double Loads_Get_kva();
    void Loads_Set_kva(double Value);
    
    // An example of property which uses the `int32_t` type
    int32_t Loads_Get_idx(void);
    void Loads_Set_idx(int32_t Value);
    
    // An example of property which uses the `char*` type, strings
    char* Loads_Get_Name(void);
    void Loads_Set_Name(char* Value);
    
    // An example of property which uses the `uint16_t` type, 
    // which represents a boolean values
    void Loads_Set_IsDelta(uint16_t Value);
    uint16_t Loads_Get_IsDelta(void);
```

For floating point and integer numbers, the API is trivial and there is no need to worry about memory. For strings, `char*`, the getter uses a global string buffer at Pascal side to keep the memory allocated (you can reset the buffer with `DSS_ResetStringBuffer()`). The caller of the string setters is responsible for the memory of the string parameters.

## Array properties

For array property access, while COM uses [`VARIANT` structures](https://docs.microsoft.com/en-us/windows/desktop/WinAuto/variant-structure), DSS C-API uses a simpler pointer convention. This convention was changed slightly in version 0.10.0.

Here, there are three functions instead of two:
```c
    void Loads_Set_ZIPV(double* ValuePtr, int32_t ValueCount);
    void Loads_Get_ZIPV(double** ResultPtr, int32_t* ResultCount);
    void Loads_Get_ZIPV_GR(void);
```

Just like for strings, the caller is responsible for the memory of the setter parameters. The pointer should be the address of the first element of the array, and the count parameter is the number of elements of the array.

The getters present two interfaces: a immediate result (classic) and a global result mode.

### Immediate Result (IR, classic, updated in v0.10.0)

The immediate/direct result getters use two parameters. Its usage is like the follow snippet:

```c
    int numZipv[2] = {0, 0}; 
    double *zipv = NULL;
    
    Loads_Get_ZIPV(&zipv, numZipv);
    
    // do something with zipv and numZipv
    // ...
    
    // Finally free the memory of the zipv result
    DSS_Dispose_PDouble(&zipv);
```

Until 0.9.8, `numZipv` here would be a simple integer:

```
    // VALID ONLY FOR DSS C-API 0.9.8
    int numZipv = 0; 
    double *zipv = NULL;
    
    Loads_Get_ZIPV(&zipv, &numZipv);
    
    // do something with zipv and numZipv
    // ...
    
    // Finally free the memory of the zipv result
    DSS_Dispose_PDouble(&zipv);
```

Since version 0.10.0, the parameter become a 2-element array. The first element of this array represents the actual count of elements in the result, and the second represents the allocated capacity of the array. This means that we can reuse the same pointer. In the interface code, if the result doesn't fit the current pointer, it will reallocate it to a larger memory block. Since most functions have predictible memory requirements, it is expected that the allocated size stabilizes quickly.

On v0.10.0, the memory is allocated as instructed. That is, when it grows, the new pointer will contain exactly the required amount of memory. We can still investigate more optimal reallocation strategies in the future (such as over-allocated to avoid constant resizing), although there is no expectation that it will help much for most of DSS interfaces.


```c
    int numZipv[2] = {0, 0}; 
    double *zipv = NULL;
    int idx = 1;
    
    // ...
    Loads_Get_ZIPV(&zipv, numZipv);
    // do something with zipv and numZipv
    // ...
    
    idx = Loads_Get_First();
    while (idx)
    {
        Loads_Get_ZIPV(&zipv, numZipv);
        
        // do something with zipv and numZipv
        // ...
            
        idx = Loads_Get_Next();
    }
    
    // Finally free the memory of the zipv result
    DSS_Dispose_PDouble(&zipv);
```

As a side note, it is recommended to avoid using `Circuit_Get_SystemY` (and `Circuit_Get_SystemY_GR`) since the dense Ybus matrix can be really memory intensive. If you need the matrix, use `YMatrix_GetCompressedYMatrix` to get its sparse CSC representation, which is much more memory efficient.

### Global Result (GR, new in v0.10.0)

The global result interface uses a single set of global pointers to pass the results. It is useful for double and integer arrays, and tight loops. The GR interface was created to address a couple of concerns:
    - It reduces the memory handling burden (at the cost of a loss of explicit names for the pointers)
    - It removes two arguments of the function call -- it may seem 
    
It is important to note that the GR mode can be used in conjunction with the IR mode, so you can update you code to use the GR mode where it excels.

Its usage is simple: call `DSS_GetGRPointers()` once to get the pointer references. There pointers passed as parameters will then be updated with pointers to the global result strucutures created in the Pascal code.
```c
    char*** data_PPAnsiChar;
    double** data_PDouble;
    int32_t** data_PInteger;
    int8_t** data_PByte;
    int32_t* count_PPAnsiChar;
    int32_t* count_PDouble;
    int32_t* count_PInteger;
    int32_t* count_PByte;

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
```

Then, instead of calling the IR functions, call the GR flavor. For example, the ZIPV call would be:

```c
    // ...
    Loads_Get_ZIPV_GR();
    // do something with data_PDouble and count_PDouble
    // ...
    
    idx = Loads_Get_First();
    while (idx)
    {
        Loads_Get_ZIPV_GR();
        
        // do something with data_PDouble and count_PDouble
        // ...
            
        idx = Loads_Get_Next();
    }
```

Remember to be careful to use or copy the data before another GR call of the same result type.

## General functions

The general functions use combinations of the previous concepts. For example, if there is a pair of parameters akin to `double** ResultPtr, int32_t* ResultCount`, the same convention of the immediate result mode is used. 

Some special functions like `YMatrix_getIpointer` and `YMatrix_getVpointer` provide internal pointers that should not be deallocated by the user. These functions should stand out since they don't folow the IR convention (pointer to data pointer + pointer to counts) and are not provided by the COM interface.

## Memory management functions

These functions are implemented in `CAPI_Utils.pas` and represent direct memory management at the API level.

- `void DSS_ResetStringBuffer(void)`: Used to reset the global string buffer. It can be useful to call this after reading a very large string from the API.

- General memory deallocation, used to free results from the immediate result API:
    - `void DSS_Dispose_PByte(int8_t** p)`
    - `void DSS_Dispose_PDouble(double** p)`
    - `void DSS_Dispose_PInteger(int32_t** p)`
    - `void DSS_Dispose_PPAnsiChar(char ***p, int32_t cnt)`

- `void DSS_DisposeGRData(void)`: Resets the global result pointers, deallocated the data pointers and resetting the count and capacity values to zero. 
    
- `DSS_GetGRPointers`: Get references to the global result (GR) pointers, used in the `*_GR` variations of most getter functions. The returned values in the DataPtrs will contain pointers to the global variables that contains the actual pointers, hence all the indirections here. Full signature:
```c
void DSS_GetGRPointers(
    char**** DataPtr_PPAnsiChar,
    double*** DataPtr_PDouble,
    int32_t*** DataPtr_PInteger,
    int8_t*** DataPtr_PByte,
    int32_t** CountPtr_PPAnsiChar,
    int32_t** CountPtr_PDouble,
    int32_t** CountPtr_PInteger,
    int32_t** CountPtr_PByte
);
```
