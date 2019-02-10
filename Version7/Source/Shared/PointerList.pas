unit PointerList;

{$M+}
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
    TPointerList = class(TObject)
    PRIVATE
        NumInList: Integer;
        MaxAllocated: Integer;
        ActiveItem: Integer;
        List: pPointerArray;
        IncrementSize: Integer;

        function Get_First: Pointer;
        function Get_Next: Pointer;
        function Get_Active: Pointer;
        procedure Set_New(value: Pointer);

    PUBLIC
        constructor Create(Size: Integer);
        destructor Destroy; OVERRIDE;

        procedure Clear;

        function Add(p: Pointer): Integer;  // Returns index of item
        function Get(i: Integer): Pointer;

        property First: Pointer READ Get_First;
        property Next: Pointer READ Get_Next;
        property ListSize: Integer READ NumInList;
        property New: Pointer WRITE Set_New;
        property Active: Pointer READ Get_Active;
        property ActiveIndex: Integer READ ActiveItem;

    PUBLISHED

    end;


implementation

constructor TPointerList.Create(Size: Integer);
//-------------------------------------------------------------------------
begin
    inherited Create;

    MaxAllocated := Size;
    if MaxAllocated <= 0 then
        MaxAllocated := 10;    // Default Size & Increment
    List := AllocMem(SizeOf(List^[1]) * MaxAllocated);
    NumInList := 0;
    ActiveItem := 0;
    IncrementSize := MaxAllocated;  // Increment is equal to original allocation
end;

//-------------------------------------------------------------------------
destructor TPointerList.Destroy;
begin
    Freemem(List, Sizeof(List^[1]) * MaxAllocated);
    inherited Destroy;
end;

//-------------------------------------------------------------------------
function TPointerList.Add(p: Pointer): Integer;
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

//-------------------------------------------------------------------------
procedure TPointerList.Set_New(value: Pointer);
begin
    Add(Value);
end;

//-------------------------------------------------------------------------
function TPointerList.Get_Active: Pointer;
begin
    if (ActiveItem > 0) and (ActiveItem <= NumInList) then
        Result := Get(ActiveItem)
    else
        Result := NIL;
end;

//-------------------------------------------------------------------------
function TPointerList.Get_First: Pointer;
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

//-------------------------------------------------------------------------
function TPointerList.Get_Next: Pointer;
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

//-------------------------------------------------------------------------
function TPointerList.Get(i: Integer): Pointer;
begin
    if (i < 1) or (i > NumInList) then
        Result := NIL
    else
    begin
        Result := List^[i];
        ActiveItem := i;
    end;
end;

procedure TPointerList.Clear;
begin
    ActiveItem := 0;
    NumInList := 0;
end;

end.
