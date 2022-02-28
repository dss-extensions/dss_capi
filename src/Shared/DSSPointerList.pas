unit DSSPointerList;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Arraydef,
    SysUtils;

type
    TDSSPointerList = class(TObject)
    PRIVATE
        NumInList: Integer;
        MaxAllocated: Integer;
        ActiveItem: Integer;
        List: pPointerArray;
        IncrementSize: Integer;

        function Get_First: Pointer;
        function Get_Next: Pointer;
        function Get_Active: Pointer;
    PUBLIC
        constructor Create(Size: Integer);
        destructor Destroy; OVERRIDE;

        procedure Clear;

        function Add(p: Pointer): Integer;  // Returns index of item
        function Get(i: Integer): Pointer; // Changes active item
        function At(i: Integer): Pointer; // Does not change the active item
        property First: Pointer READ Get_First;
        property Next: Pointer READ Get_Next;
        property Count: Integer READ NumInList;
        property Active: Pointer READ Get_Active;
        property ActiveIndex: Integer READ ActiveItem;

        property InternalPointer: pPointerArray read List;
    end;

implementation

constructor TDSSPointerList.Create(Size: Integer);
begin
    inherited Create;

    MaxAllocated := Size;
    if MaxAllocated <= 0 then
        MaxAllocated := 10;    // Default Size & Increment
    List := AllocMem(SizeOf(Pointer) * MaxAllocated);
    NumInList := 0;
    ActiveItem := 0;
    IncrementSize := MaxAllocated;  // Increment is equal to original allocation
end;

destructor TDSSPointerList.Destroy;
begin
    Freemem(List, Sizeof(Pointer) * MaxAllocated);
    inherited Destroy;
end;

function TDSSPointerList.Add(p: Pointer): Integer;
begin
    Inc(NumInList);
    if NumInList > MaxAllocated then
    begin
        MaxAllocated := MaxAllocated + IncrementSize;
        ReallocMem(List, SizeOf(List^[1]) * MaxAllocated);
    end;
    List^[NumInList] := p;
    Result := NumInList;
    ActiveItem := Result;
end;

function TDSSPointerList.Get_Active: Pointer;
begin
    if (ActiveItem > 0) and (ActiveItem <= NumInList) then
        Result := Get(ActiveItem)
    else
        Result := NIL;
end;

function TDSSPointerList.Get_First: Pointer;
begin
    if NumInList > 0 then
    begin
        ActiveItem := 1;
        Result := List^[ActiveItem];
    end
    else
    begin
        ActiveItem := 0;
        Result := NIL;
    end;
end;

function TDSSPointerList.Get_Next: Pointer;
begin
    if NumInList > 0 then
    begin
        Inc(ActiveItem);
        if ActiveItem > NumInList then
        begin
            ActiveItem := NumInList;
            Result := NIL;
        end
        else
            Result := List^[ActiveItem];
    end
    else
    begin
        ActiveItem := 0;
        Result := NIL;
    end;
end;

function TDSSPointerList.Get(i: Integer): Pointer;
begin
    if (i < 1) or (i > NumInList) then
        Result := NIL
    else
    begin
        Result := List^[i];
        ActiveItem := i;
    end;
end;

function TDSSPointerList.At(i: Integer): Pointer;
begin
    if (i < 1) or (i > NumInList) then
        Result := NIL
    else
    begin
        Result := List^[i];
    end;
end;

procedure TDSSPointerList.Clear;
begin
    ActiveItem := 0;
    NumInList := 0;
end;

end.
