unit Arraydef;

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

type

{ Define arrays with dummy dimension of 100 so we can hard code
  constants for accessing small order elements;  Otherwise, always
  allocate arrays of these types before using}
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

    pDouble = ^Double;
    pSingle = ^Single;
    pSmallInt = ^Smallint;
    pLongInt = ^Longint;

function AllocStringArray(Size: Integer): pStringArray;
procedure FreeStringArray(var pS: pStringArray; Size: Integer);
{--------------------------------------------------------------------------}

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
