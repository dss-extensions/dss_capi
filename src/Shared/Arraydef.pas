unit Arraydef;

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    CAPI_Types,
    UComplex;

type

{$PUSH}
{$SCOPEDENUMS ON}
{$Z4} // keep enums as int32 values
    TGeneralConnection = (
        Wye = 0, // wye, star, line-neutral connection
        Y = 0, // wye, star, line-neutral connection
        LN = 0, // wye, star, line-neutral connection
        Delta = 1, // delta, line-line connection
        LL = 1 // delta, line-line connection
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
    StringArray = array[1..100] of String;

    pPtrIntArray = ^PtrIntArray;
    PtrIntArray = array[1..100] of PtrInt;

    PDoubleArray0 = CAPI_Types.PDoubleArray0;
    PSingleArray0 = CAPI_Types.PSingleArray0;


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
