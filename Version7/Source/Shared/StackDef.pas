unit StackDef;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    ArrayDef;

type
    TStackBase = class(TObject)
    PRIVATE

    PROTECTED
        NumItems,
        Increment,
        MaxItems: Integer;

    PUBLIC
        constructor Create(initSize: Integer);
        destructor Destroy; OVERRIDE;
        procedure Clear;
        function Size: Integer;
    end;


    TPstack = class(TStackBase)  // simple pointer stack
    PRIVATE
        Items: pPointerArray;
    PUBLIC
        constructor Create(initSize: Integer);
        destructor Destroy; OVERRIDE;

        procedure Push(p: Pointer);
        function Pop: Pointer;
    end;

    TiStack = class(TStackBase)  // simple integer stack
    PRIVATE
        Items: pIntegerArray;
    PUBLIC
        constructor Create(initSize: Integer);
        destructor Destroy; OVERRIDE;

        procedure Push(p: Integer);
        function Pop: Integer;
    end;


implementation

uses
    Sysutils;

constructor TStackBase.Create(initSize: Integer);
begin
    inherited Create;
    MaxItems := InitSize;
    Increment := InitSize;
    NumItems := 0;
end;

destructor TStackBase.Destroy;
begin
    inherited Destroy;
end;

procedure TStackBase.Clear;
begin
    NumItems := 0;
end;

function TStackBase.Size: Integer;
begin
    Result := NumItems;
end;


constructor TPstack.Create(initSize: Integer);
begin
    inherited Create(InitSize);
    Items := AllocMem(SizeOf(Items^[1]) * MaxItems);
end;

destructor TPstack.Destroy;
begin
    Reallocmem(Items, 0);
    inherited Destroy;
end;

procedure TPstack.Push(p: Pointer);
begin
    Inc(NumItems);
    if NumItems > MaxItems then
    begin
        Inc(MaxItems, Increment);
        Reallocmem(Items, SizeOf(Items^[1]) * MaxItems);
    end;
    Items^[NumItems] := p;
end;

function TPstack.Pop: Pointer;
begin
    if NumItems > 0 then
    begin
        Result := Items^[NumItems];
        Dec(NumItems);
    end
    else
        Result := NIL;
end;


constructor TiStack.Create(initSize: Integer);
begin
    inherited Create(initSize);
    Items := AllocMem(SizeOf(Items^[1]) * MaxItems);
end;

destructor TiStack.Destroy;
begin
    Reallocmem(Items, 0);
    inherited Destroy;
end;

procedure TiStack.Push(p: Integer);
begin
    Inc(NumItems);
    if NumItems > MaxItems then
    begin
        Inc(MaxItems, Increment);
        Reallocmem(Items, SizeOf(Items^[1]) * MaxItems);
    end;
    Items^[NumItems] := p;
end;

function TiStack.Pop: Integer;
begin
    if NumItems > 0 then
    begin
        Result := Items^[NumItems];
        Dec(NumItems);
    end
    else
        Result := 0;
end;


end.
