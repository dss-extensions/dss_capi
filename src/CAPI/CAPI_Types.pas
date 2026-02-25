unit CAPI_Types;

{$mode objfpc}

interface

uses
    UComplex;

type
{$PUSH}
{$SCOPEDENUMS ON}
{$Z4} // keep enums as int32 values
    TGeneralConnection = (
        Wye = 0, // wye, star, line-neutral connection
        // Y = 0, // wye, star, line-neutral connection
        // LN = 0, // wye, star, line-neutral connection
        Delta = 1 // delta, line-line connection
        // LL = 1 // delta, line-line connection
    );
{$SCOPEDENUMS OFF}
{$POP}

    TCBuffer24 = array[1..24] of Complex;
    PCBuffer24 = ^TCBuffer24;


// Define arrays with dummy dimension of 100 so we can hard code
// constants for accessing small order elements;  Otherwise, always
// allocate arrays of these types before using
    pSmallIntArray = ^SmallIntArray;
    SmallIntArray = array[1..100] of Smallint;
    pIntegerArray = ^LongIntArray;
    pLongIntArray = ^LongIntArray;
    LongIntArray = array[1..100] of Longint;
    pDoubleArray = ^DoubleArray;
    DoubleArray = array[1..100] of Double;
    pSingleArray = ^SingleArray;
    SingleArray = array[1..100] of Single;
    pPointerArray = ^PointerArray;
    PointerArray = array[1..100] of Pointer;
    pStringArray = ^StringArray;
    StringArray = array[1..100] of AnsiString;

    pPtrIntArray = ^PtrIntArray;
    PtrIntArray = array[1..100] of PtrInt;

    ArrayOfDouble = Array of Double;
    ArrayOfInteger = Array of Integer;
    ArrayOfString = Array of AnsiString;
    ArrayOfPointer = Array of Pointer;
    PArrayOfDouble = ^ArrayOfDouble;
    PArrayOfInteger = ^ArrayOfInteger;
    PArrayOfString = ^ArrayOfString;
    PArrayOfPointer = ^ArrayOfPointer;


    BooleanArray = Array[1..100] of Boolean;
    pBooleanArray = ^BooleanArray;


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
    // PSingle = PFloat32;
    PPSingle = PPFloat32;
    
    // TODO: for 0.15?, update to Int64 and Boolean (at least on 64-bit platforms?)
    TAPISize = Int32;
    PAPISize = ^Int32;
    TAPIBoolean = WordBool;
    TAltAPIBoolean = LongBool;

{$SCOPEDENUMS ON}
{$PUSH}
{$PACKSET 4} // keep sets as int32

    TDSSPropertySetterFlag = (
        // Most array properties depend on sizes defined by other properties.
        // Using this flag, many properties allow users to skip setting the other property
        // directly, allowing the engine to use the size of the provided array to
        // initialize the other property.
        ImplicitSizes = 0,

        // Some components like Loads don't need to update YPrim for every change, e.g. setting
        // "`load.a_load.kW=1`" if was "kW" previously 2 should not force a YPrim update, but it does
        // force an update by default.
        // Using this flag will reproduce what the classic OpenDSS API for Loads (DSS.ActiveCircuit.Loads)
        // does, but removes a lot of duplicated code. Besides that, we can extend the feature 
        // for other components if we think it fits.
        AvoidFullRecalc = 1,

        // For batch operations, skip NA values -- values of NaN for float64, INT32_MAX (0x7fffffff) for int32, null pointers for strings.
        SkipNA = 2,

        // Use internally to handle setting single elements in arrays
        SingleElement = 3,

        // Broadcast a scalar value to all elements in an array
        Broadcast = 4,

        Reserved5 = 5,
        Reserved6 = 6,
        Reserved7 = 7,
        Reserved8 = 8,
        Reserved9 = 9,
        Reserved10 = 10,
        Reserved11 = 11,
        Reserved12 = 12,
        Reserved13 = 13,
        Reserved14 = 14,
        Reserved15 = 15,
        Reserved16 = 16,
        Reserved17 = 17,
        Reserved18 = 18,
        Reserved19 = 19,
        Reserved20 = 20,
        Reserved21 = 21,
        Reserved22 = 22,
        Reserved23 = 23,
        Reserved24 = 24,
        Reserved25 = 25,
        Reserved27 = 27,
        StrictSize = 28,
        FixedMaxSize = 29
        
        // REMOVED: Used internally for the "Wires" property ("Conductors").
        // AllowAllConductors = 30 -- removed since there is now a dedicated Conductors property

        // Leave the last bit alone; it creates some issues elsewhere.
    );
    TSetterFlag = TDSSPropertySetterFlag;
    TDSSPropertySetterFlags = set of TDSSPropertySetterFlag;
{$POP}
{$SCOPEDENUMS OFF}

function AllocStringArray(Size: Integer): pStringArray;
procedure FreeStringArray(var pS: pStringArray; Size: Integer);

implementation

uses
    SysUtils;

function AllocStringArray(Size: Integer): pStringArray;
// Allocates a string array initialized with nil values
begin
    Result := AllocMem(SizeOf(String) * Size);
end;

procedure FreeStringArray(var pS: pStringArray; Size: Integer);
var
    i: Integer;
begin
    if Assigned(ps) then
    begin
        for i := 1 to Size do
        begin
            pS^[i] := '';  // decrement counter in string
        end;
        Reallocmem(ps, 0);  // Throw it away and set it to NIL
    end;
end;

end.
