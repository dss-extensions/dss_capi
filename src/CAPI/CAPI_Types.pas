unit CAPI_Types;

{$mode objfpc}

interface

type
    PointerArray0 = array[0..$effffff] of Pointer;
    PPointerArray0 = ^PointerArray0;

    DoubleArray0 = array[0..$effffff] of Double;
    PDoubleArray0 = ^DoubleArray0;

    IntegerArray0 = array[0..$effffff] of Integer;
    PIntegerArray0 = ^IntegerArray0;

    PAnsiCharArray0 = array[0..$effffff] of PAnsiChar;
    PPAnsiCharArray0 = ^PAnsiCharArray0;

    PPDouble = ^PDouble;
    PPInteger = ^PInteger;
    PPByte = ^PByte;
    PPPAnsiChar = ^PPAnsiChar;

    Float32 = Single;
    Float32Array0 = array[0..$effffff] of Float32;
    PFloat32Array0 = ^Float32Array0;
    PFloat32 = ^Float32;
    PPFloat32 = ^PFloat32;

    SingleArray0 = Float32Array0;
    PSingleArray0 = PFloat32Array0;
    PSingle = PFloat32;
    PPSingle = PPFloat32;
    
    // TODO: for 0.13, update to Int64 and Boolean
    TAPISize = Int32;
    PAPISize = ^Int32;
    TAPIBoolean = WordBool;

implementation

end.