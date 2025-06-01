unit DSSPointerList;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    Arraydef,
    SysUtils;

type
    TDSSPointerList = class;

    TDSSPointerEnumerator = class
    private
        lst: TDSSPointerList;
        currentPtr: Pointer;
        currentIdx: Integer;
        function Get_Current(): Pointer;
    public
        constructor Create(alst: TDSSPointerList); 
        function MoveNext(): Boolean;
        property Current: Pointer READ Get_Current;
    end;

    TDSSPointerList = class(TObject)
    PRIVATE
        MaxAllocated: Integer;
        IncrementSize: Integer;

    PUBLIC
        count: Integer;
        activeIndex: Integer;
        listPtr: pPointerArray;

        constructor Create(Size: Integer);
        destructor Destroy; OVERRIDE;

        procedure Clear();

        function Add(p: Pointer): Integer;  // Returns index of item
        function Get(i: Integer): Pointer; // Changes active item
        function At(i: Integer): Pointer; // Does not change the active item
        function First(): Pointer;
        function Next(): Pointer;
        function Active(): Pointer;
        procedure ResetActive();

        function GetEnumerator(): TDSSPointerEnumerator;
    end;

implementation

constructor TDSSPointerList.Create(Size: Integer);
begin
    inherited Create;

    MaxAllocated := Size;
    if MaxAllocated <= 0 then
        MaxAllocated := 10;    // Default Size & Increment
    listPtr := AllocMem(SizeOf(Pointer) * MaxAllocated);
    count := 0;
    activeIndex := 0;
    IncrementSize := MaxAllocated;  // Increment is equal to original allocation
end;

destructor TDSSPointerList.Destroy;
begin
    Freemem(listPtr);
    inherited Destroy;
end;

function TDSSPointerList.Add(p: Pointer): Integer;
begin
    Inc(count);
    if count > MaxAllocated then
    begin
        MaxAllocated := MaxAllocated + IncrementSize;
        ReallocMem(listPtr, SizeOf(listPtr[1]) * MaxAllocated);
    end;
    listPtr[count] := p;
    Result := count;
    activeIndex := Result;
end;

function TDSSPointerList.Active(): Pointer;
begin
    if (activeIndex > 0) and (activeIndex <= count) then
        Result := Get(activeIndex)
    else
        Result := NIL;
end;

function TDSSPointerList.First(): Pointer;
begin
    if count > 0 then
    begin
        activeIndex := 1;
        Result := listPtr[activeIndex];
    end
    else
    begin
        activeIndex := 0;
        Result := NIL;
    end;
end;

function TDSSPointerList.Next(): Pointer;
begin
    if count > 0 then
    begin
        Inc(activeIndex);
        if activeIndex > count then
        begin
            activeIndex := count;
            Result := NIL;
        end
        else
            Result := listPtr[activeIndex];
    end
    else
    begin
        activeIndex := 0;
        Result := NIL;
    end;
end;

function TDSSPointerList.Get(i: Integer): Pointer;
begin
    if (i < 1) or (i > count) then
        Result := NIL
    else
    begin
        Result := listPtr[i];
        activeIndex := i;
    end;
end;

function TDSSPointerList.At(i: Integer): Pointer;
begin
    if (i < 1) or (i > count) then
        Result := NIL
    else
    begin
        Result := listPtr[i];
    end;
end;

procedure TDSSPointerList.Clear();
begin
    activeIndex := 0;
    count := 0;
end;

procedure TDSSPointerList.ResetActive();
begin
    activeIndex := 0;
end;

function TDSSPointerList.GetEnumerator(): TDSSPointerEnumerator;
begin
    Result := TDSSPointerEnumerator.Create(self);
end;

function TDSSPointerEnumerator.Get_Current(): Pointer;
begin
    Result := currentPtr;
end;

function TDSSPointerEnumerator.MoveNext(): Boolean;
begin
    if lst.Count > 0 then
    begin
        Inc(currentIdx);
        if currentIdx > lst.Count then
        begin
            currentIdx := lst.Count;
            currentPtr := NIL;
            lst.activeIndex := currentIdx; // for backwards compatibility
        end
        else
            currentPtr := lst.listPtr[currentIdx];
    end
    else
    begin
        currentIdx := 0;
        currentPtr := NIL;
    end;
    Result := currentPtr <> NIL;
end;

constructor TDSSPointerEnumerator.Create(alst: TDSSPointerList);
begin
    lst := alst;
    // lst.ResetActive(); // this could be required for backwards compat.
    currentPtr := NIL;
    currentIdx := 0;
end;

end.
