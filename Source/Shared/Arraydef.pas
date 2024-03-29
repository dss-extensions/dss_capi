unit Arraydef;

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

type
    TRatingsArray = array of Double;
{ Define arrays with dummy dimension of 100 so we can hard code
  constants for accessing small order elements;  Otherwise, always
  allocate arrays of these types before using}
    SmallIntArray = array[1..100] of Smallint;
    pSmallIntArray = ^SmallIntArray;
    pIntegerArray = ^LongIntArray;
    LongIntArray = array[1..100] of Longint;
    pLongIntArray = ^LongIntArray;
    DoubleArray = array[1..100] of Double;
    pDoubleArray = ^DoubleArray;
    SingleArray = array[1..100] of Single;
    pSingleArray = ^SingleArray;
    PointerArray = array[1..100] of Pointer;
    pPointerArray = ^PointerArray;
    StringArray = array[1..100] of String;
    pStringArray = ^StringArray;
    DynStringArray = array of String;
    pDynStringArray = ^DynStringArray;


    pDouble = ^Double;
    pSingle = ^Single;
    pSmallInt = ^Smallint;
    pLongInt = ^Longint;

    DynSlot = array [0..1] of Double; // dynamic memory slot, just 1 (z-1) slots for now
    pDynSlot = ^DynSlot;

function AllocStringArray(Size: Integer): pStringArray;
procedure FreeStringArray(var pS: pStringArray; Size: Integer);
{--------------------------------------------------------------------------}

implementation

uses
    SysUtils;

function AllocStringArray(Size: Integer): pStringArray;
// Allocates a string array initialized with nil values
begin
  {$IFDEF FPC}
    initialize(Result);
{$ENDIF}
    Result := AllocMem(SizeOf(Result^[1]) * Size);
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
