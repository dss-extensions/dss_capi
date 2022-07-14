unit Arraydef;
 {
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Type
    TRatingsArray   = Array of Double;
{ Define arrays with dummy dimension of 100 so we can hard code
  constants for accessing small order elements;  Otherwise, always
  allocate arrays of these types before using}
    SmallIntArray   = Array[1..100] of SmallInt;
    pSmallIntArray  = ^SmallIntArray;
    pIntegerArray   = ^LongIntArray;
    LongIntArray    = Array[1..100] of LongInt;
    pLongIntArray   = ^LongIntArray;
    DoubleArray     = Array[1..100] of Double;
    pDoubleArray    = ^DoubleArray;
    SingleArray     = Array[1..100] of Single;
    pSingleArray    = ^SingleArray;
    PointerArray    = Array[1..100] of Pointer;
    pPointerArray   = ^PointerArray;
    StringArray     = Array[1..100] of String;
    pStringArray    = ^StringArray;
    DynStringArray  = Array of string;
    pDynStringArray = ^DynStringArray;


    pDouble = ^Double;
    pSingle = ^Single;
    pSmallInt = ^SmallInt;
    pLongInt = ^LongInt;

    DynSlot         = array [0..1] of double; // dynamic memory slot, just 1 (z-1) slots for now
    pDynSlot        = ^DynSlot;

Function AllocStringArray(Size:Integer):pStringArray;
Procedure FreeStringArray(var pS:pStringArray; Size:Integer);
{--------------------------------------------------------------------------}

implementation

uses SysUtils;

Function AllocStringArray(Size:Integer):pStringArray;
// Allocates a string array initialized with nil values
Begin
  {$IFDEF FPC}initialize(Result);{$ENDIF}
  Result := AllocMem(SizeOf(Result^[1])*Size);
End;

Procedure FreeStringArray(var pS:pStringArray; Size:Integer);
Var i:Integer;
Begin
    IF Assigned(ps) Then Begin
        For i := 1 to Size Do Begin
            pS^[i] := '';  // decrement counter in string
        End;
        Reallocmem(ps, 0);  // Throw it away and set it to NIL
    End;
End;

end.
