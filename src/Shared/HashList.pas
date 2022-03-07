unit HashList;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

//  This Hash list module is designed to make searches on arrays of strings more
//  efficient.  The list actually consists of several short linear lists.  When a string
//  is added, it is hashed and placed at the end of one of the lists.
//
//  The list may by searched by string or by index.  When by string, the string
//  is hashed and the search is restricted to the resulting linear list.  When by
//  index, it simply goes to that index in the array of pointers that points to the
//  individual strings.
//
//  All strings are saved in lower case and tested with case sensitivity.  This
//  actually makes the search insensitive to case because everything is lower case.

interface

uses
    {$IFDEF DSS_CAPI_HASHLIST}Contnrs,{$ENDIF}
    ArrayDef;

type
    TSubList = record
        Nelem: Cardinal;
        NAllocated: Cardinal;
        Str: pStringArray;
        Idx: pLongIntArray;
    end;

    pSubListArray = ^SubListArray;
    SubListArray = array[1..100] of TSubList;

    THashList = class(TObject)
    PRIVATE
        NumElementsAllocated: Cardinal;
        NumLists: Cardinal;
        NumElements: Cardinal;
        StringPtr: pStringArray;
        ListPtr: pSubListArray;
        AllocationInc: Cardinal;
        LastFind: Cardinal;
        LastHash: Cardinal;
        LastSearchString: String;

        procedure ResizeSubList(var SubList: TSubList);
        function Hash(const S: String): Cardinal;
        procedure ResizeStrPtr;
    PUBLIC
        InitialAllocation: Cardinal;
        constructor Create(Nelements: Cardinal);
        destructor Destroy; OVERRIDE;
        function Add(const S: String): Integer;
        function Find(const S: String): Integer;
        function FindNext: Integer;  //  repeat find for duplicate string in same hash list
        function FindAbbrev(const S: String): Integer;
        function NameOfIndex(i: Cardinal): String;
        // procedure Expand(NewSize: Cardinal);   {Expands number of elements}
        procedure DumpToFile(const fname: String);
        procedure Clear;
        property Count: Cardinal READ NumElements;
    end;

{$IFDEF DSS_CAPI_HASHLIST}
    TAltHashList = class (TFPHashList)
    PUBLIC
        constructor Create(const Nelements: Integer);
        destructor Destroy; OVERRIDE;
        function Add(const S: String): Integer; inline;
        function Find(const S: String): Integer; inline;
        function FindAbbrev(const S: String): Integer;
        procedure DumpToFile(const fname: String);
        function NameOfIndex(i: Integer): String; inline;
    end;
{$ENDIF}


    TBusHashListType = {$IFDEF DSS_CAPI_HASHLIST}TAltHashList;{$ELSE}THashList;{$ENDIF}
    TCommandHashListType = {$IFDEF DSS_CAPI_HASHLIST}TAltHashList;{$ELSE}THashList;{$ENDIF}
    TClassNamesHashListType = {$IFDEF DSS_CAPI_HASHLIST}TAltHashList;{$ELSE}THashList;{$ENDIF}

implementation

uses
    BufStream,
    Classes,
    Utilities,
    Sysutils,
    math;

constructor THashList.Create(Nelements: Cardinal);
var
    i: Integer;
    Elementsperlist: Cardinal;
begin
    inherited Create;
    NumElements := 0;
    InitialAllocation := Nelements;
    StringPtr := NIL;  // Allocmem(Sizeof(StringPtr^[1]) * Nelements);

    NumLists := round(sqrt(Nelements));
    if NumLists < 1 then
        NumLists := 1;  // make sure at least one list
    ElementsPerList := Nelements div NumLists + 1;
    AllocationInc := ElementsPerList;

    Getmem(ListPtr, Sizeof(Listptr^[1]) * NumLists);
    for i := 1 to NumLists do
    begin
         {Allocate initial Sublists to zero; alocated on demand}
        with ListPtr^[i] do
        begin
            Str := NIL;
            Idx := NIL;
            Nallocated := 0;
            Nelem := 0;
        end;
    end;
    NumElementsAllocated := 0;
    LastFind := 0;
    LastHash := 0;
    LastSearchString := '';
end;

destructor THashList.Destroy;
var
    i, j: Integer;
begin
    for i := 1 to NumLists do
    begin
         {DeAllocated  Sublists}
        with ListPtr^[i] do
        begin
            for j := 1 to Nallocated do
                Str^[j] := ''; // decrement ref count on string
            Freemem(Str, SizeOf(Str^[1]) * Nallocated);
            Freemem(Idx, SizeOf(Idx^[1]) * Nallocated);
        end;
    end;

    Freemem(ListPtr, Sizeof(Listptr^[1]) * NumLists);

    for i := 1 to NumElementsAllocated do
        StringPtr^[i] := ''; // get rid of string storage

    Freemem(StringPtr, Sizeof(StringPtr^[1]) * NumElementsAllocated);

    inherited Destroy;
end;

procedure ReallocStr(var S: pStringArray; oldSize, NewSize: Integer);
// Make a bigger block to hold the pointers to the strings
var
    X: pStringArray;
begin
    X := Allocmem(NewSize);   // Zero fills new string pointer array (important!)
    if OldSize > 0 then
    begin
        Move(S^, X^, OldSize);
        Freemem(S, Oldsize);
    end;
    S := X;
end;

procedure THashList.ResizeSubList(var SubList: TSubList);
var
    OldAllocation: Cardinal;
begin
    // resize by reasonable amount

    with SubList do
    begin
        OldAllocation := Nallocated;
        Nallocated := OldAllocation + AllocationInc;
        ReallocStr(Str, Sizeof(Str^[1]) * OldAllocation, Sizeof(Str^[1]) * Nallocated);
        Reallocmem(Idx, Sizeof(Idx^[1]) * Nallocated);
    end;
end;

(*   New supposedly fast hash method      *)
function THashList.Hash(const S: String): Cardinal;
var
    Hashvalue: Cardinal;
    i: Integer;

  {per Stackoverflow.com}
begin
    HashValue := 0;
    for i := 1 to Length(S) do
    begin
        HashValue := ((HashValue shl 5) or (HashValue shr 27)) xor Cardinal(S[i]);
    end;
    Result := (Hashvalue mod NumLists) + 1;
end;


procedure THashList.ResizeStrPtr;

// make linear string list larger

var
    OldAllocation: Cardinal;
    NewPointer: pStringArray;
begin
    OldAllocation := NumelementsAllocated;
    NumelementsAllocated := OldAllocation + AllocationInc * NumLists;

   // Allocate and zero fill new string space (important!)
    NewPointer := Allocmem(SizeOf(StringPtr^[1]) * NumElementsAllocated);

   //move old stuff to new location then dispose of old stuff
    if OldAllocation > 0 then
    begin
        Move(StringPtr^[1], NewPointer^[1], SizeOf(StringPtr^[1]) * OldAllocation);
        Freemem(StringPtr, SizeOf(StringPtr^[1]) * OldAllocation);
    end;
    StringPtr := NewPointer;
end;

function THashList.Add(const S: String): Integer;
var
    HashNum: Cardinal;
    SS: String;
begin
    SS := AnsiLowerCase(S);
    HashNum := Hash(SS);

    Inc(NumElements);
    if NumElements > NumElementsAllocated then
        ResizeStrPtr;
    Result := NumElements;

    with ListPtr^[hashNum] do
    begin
        Inc(Nelem);
        if Nelem > Nallocated then
            ResizeSubList(ListPtr^[HashNum]);
    end;

    with ListPtr^[hashNum] do
    begin
        Str^[Nelem] := SS;   // make copy of whole string, lower case
        StringPtr^[NumElements] := SS;   // increments count to string
        Idx^[Nelem] := NumElements;
    end;
end;


function THashList.Find(const S: String): Integer;
var
    i: Integer;
begin
    LastSearchString := AnsiLowerCase(S);
    LastHash := Hash(LastSearchString);
    Result := 0;
    LastFind := 0;

    with ListPtr^[LastHash] do
    begin
        for i := 1 to Nelem do
        begin
            if CompareStr(LastSearchString, Str^[i]) = 0 then
            begin
                Result := Idx^[i];    // Return the index of this element
                LastFind := i;
                Break;
            end;
        end;
    end;
end;

function THashList.FindNext: Integer;

// Begin search in same list as last
var
    i: Integer;
begin
    Result := 0;  // Default return
    Inc(LastFind); // Start with next item in hash list

    if (LastHash > 0) and (LastHash <= NumLists) then
        with ListPtr^[LastHash] do
        begin
            for i := LastFind to Nelem do
            begin
                if CompareStr(LastSearchString, Str^[i]) = 0 then
                begin
                    Result := Idx^[i];    // Return the index of this element
                    LastFind := i;
                    Break;
                end;
            end;
        end;
end;


function THashList.FindAbbrev(const S: String): Integer;
{Just make a linear search and test each string until a string is found that
 matches all the characters entered in S}
var
    Test1, Test2: String;
    i: Integer;
begin
    Result := 0;
    if Length(S) = 0 then
        Exit;
    
    Test1 := AnsiLowerCase(S);

    for i := 1 to NumElements do
    begin
        Test2 := Copy(StringPtr^[i], 1, Length(Test1));
        if CompareStr(Test1, Test2) = 0 then
        begin
            Result := i;
            Break;
        end;
    end;
end;


function THashList.NameOfIndex(i: Cardinal): String;
begin
    if (i > 0) and (i <= NumElements) then
        Result := StringPtr^[i]
    else
        Result := '';
end;


procedure THashList.DumpToFile(const fname: String);
var
    F: TFileStream;
    i, j: Integer;
    sout: String;
begin
    F := TBufferedFileStream.Create(fname, fmCreate);
    FSWriteln(F, Format('Number of Hash Lists = %d, Number of Elements = %d', [NumLists, NumElements]));

    FSWriteln(F);
    FSWriteln(F, 'Hash List Distribution');
    for i := 1 to NumLists do
    begin
        with ListPtr^[i] do
        begin
            FSWriteln(F, Format('List = %d, Number of elements = %d', [i, Nelem]));
        end;
    end;
    FSWriteln(F);

    for i := 1 to NumLists do
    begin
        with ListPtr^[i] do
        begin
            FSWriteln(F, Format('List = %d, Number of elements = %d', [i, Nelem]));
            for j := 1 to Nelem do
            begin
                WriteStr(sout, '"', Str^[j], '"  Idx= ', Idx^[j]: 0);
                FSWriteln(F, sout);
            end;
        end;
        FSWriteln(F);
    end;
    FSWriteln(F, 'LINEAR LISTING...');
    for i := 1 to NumElements do
    begin
        WriteStr(sout, i: 3, ' = "', Stringptr^[i], '"');
        FSWriteln(F, sout);
    end;
    FreeAndNil(F);
end;

procedure THashList.Clear;
var
    i, j: Integer;
begin
    for i := 1 to NumLists do
    begin
        with ListPtr^[i] do
        begin
            Nelem := 0;
            for j := 1 to Nallocated do
                ListPtr^[i].Str^[j] := '';
        end;
    end;

    for i := 1 to NumElementsAllocated do
        StringPtr^[i] := '';

    NumElements := 0;
end;

{$IFDEF DSS_CAPI_HASHLIST}
constructor TAltHashList.Create(const Nelements: Integer);
begin
    inherited Create();
    Capacity := Nelements;
end;
destructor TAltHashList.Destroy;
begin
    inherited;
end;
function TAltHashList.Add(const S: String): Integer; inline;
begin
    inherited Add(AnsiLowerCase(s), Pointer(self.Count + 1));
    Result := self.Count;
end;
function TAltHashList.Find(const S: String): Integer; inline;
begin
    Result := Integer(inherited Find(AnsiLowerCase(s)));
end;
function TAltHashList.NameOfIndex(i: Integer): String; inline;
begin
    Result := inherited NameOfIndex(i - 1);
end;
procedure TAltHashList.DumpToFile(const fname: String);
var
    F: TextFile;
    i: Integer;
begin
    AssignFile(F, fname);
    Rewrite(F);
    Writeln(F, 'LINEAR LISTING...');
    for i := 1 to Count do
    begin
        Writeln(F, i: 3, ' = "', NameOfIndex(i), '"');
    end;
    CloseFile(F);
end;

function TAltHashList.FindAbbrev(const S: String): Integer;
var
    Test1, Test2: String;
    i: Integer;
begin
    Result := 0;
    if Length(S) = 0 then
        Exit;
    
    Test1 := AnsiLowerCase(S);

    for i := 1 to Count do
    begin
        Test2 := Copy(NameOfIndex(i), 1, Length(Test1));
        if CompareStr(Test1, Test2) = 0 then
        begin
            Result := i;
            break;
        end;
    end;
end;
{$ENDIF}

end.
