unit HashList;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
  This Hash list module is designed to make searches on arrays of strings more
  efficient.  The list actually consists of several short linear lists.  When a string
  is added, it is hashed and placed at the end of one of the lists.

  The list may by searched by string or by index.  When by string, the string
  is hashed and the search is restricted to the resulting linear list.  When by
  index, it simply goes to that index in the array of pointers that points to the
  individual strings.

  All strings are saved in lower case and tested with case sensitivity.  This
  actually makes the search insensitive to case because everything is lower case.

  Modified 4/18/98 to allocate on demand.  That way, you can create it for a certain
  number of hash lists, without caring about how many actual there will be.

}

{$M+}

interface

uses
    ArrayDef;

type

   //pStringArray = ^StringArray;
  // StringArray = Array[1..100] of String;

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
    PROTECTED

    PUBLIC
        InitialAllocation: Cardinal;
        constructor Create(Nelements: Cardinal);
        destructor Destroy; OVERRIDE;
        function Add(const S: String): Integer;
        function Find(const S: String): Integer;
        function FindNext: Integer;  //  repeat find for duplicate string in same hash list
        function FindAbbrev(const S: String): Integer;
        function Get(i: Cardinal): String;
        procedure Expand(NewSize: Cardinal);   {Expands number of elements}
        procedure DumpToFile(const fname: String);
        procedure Clear;
        property ListSize: Cardinal READ NumElements;
    PUBLISHED

    end;


implementation

uses
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

(*   This one was for AnsiStrings and just moved up to 8 bytes into an integer
Function THashList.Hash(Const S:String):Integer;

VAR
    Hashvalue:UInt64;

BEGIN
   HashValue := Uint64(0);
 // Only hash first 8 characters

   Move(S[1], HashValue, min(8, Length(S)));
   Result := (Hashvalue mod NumLists) + 1;
END;
*)

(* OLD HASH FUNCTION -- only hashes 1st 8 chars

Function THashList.Hash(Const S:String):Cardinal;

VAR
    Hashvalue:Word;
    i:Integer;

    {Use Delphi Math function: it is declared as inline}
    {Function Min(const a,b:Integer):Integer;
    BEGIN If a<b Then result := a else result := b; END;  }
BEGIN
   HashValue := 0;
 // Only hash first 8 characters
   FOR i := 1 to min(8,Length(S)) DO HashValue := HashValue*2 + ord(S[i]);
   Result := (Hashvalue mod NumLists) + 1;
END;
*)

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
    SS := LowerCase(S);
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

    LastSearchString := LowerCase(S);
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
    if Length(S) > 0 then
    begin
        Test1 := LowerCase(S);

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

end;


function THashList.Get(i: Cardinal): String;
begin
    if (i > 0) and (i <= NumElements) then
        Result := StringPtr^[i]
    else
        Result := '';
end;

procedure THashList.Expand(NewSize: Cardinal);

{
  This procedure creates a new set of string lists and copies the
  old strings into the new, hashing for the new number of lists.

}

var
    NewStringPtr: pStringArray;
    NewNumLists: Cardinal;
    ElementsPerList: Cardinal;
    NewListPtr: pSubListArray;
    HashNum: Cardinal;
    S: String;
    OldNumLists: Cardinal;
    i, j: Integer;

begin
    if NewSize > NumElementsAllocated then
    begin

        OldNumLists := NumLists;

        NewStringPtr := AllocMem(SizeOf(String) * NewSize);
        NewNumLists := round(sqrt(NewSize));
        ElementsPerList := NewSize div NewNumLists + 1;
        if NewNumLists < 1 then
            NewNumLists := 1;  // make sure at least one list
        Getmem(NewListPtr, Sizeof(TSubList) * NewNumLists);
        for i := 1 to NumLists do
        begin
         {Allocate initial Sublists}
            with NewListPtr^[i] do
            begin
                Str := Allocmem(SizeOf(Str^[1]) * ElementsPerList);
                Idx := Allocmem(SizeOf(Idx^[1]) * ElementsPerList);
                Nallocated := ElementsPerList;
                Nelem := 0;
            end;
        end;

        NumLists := NewNumLists;  // Has to be set so Hash function will work

{Add elements from old Hash List to New Hash List}


        for i := 1 to NumElements do
        begin
            S := StringPtr^[i];
            HashNum := Hash(S);

            with NewListPtr^[hashNum] do
            begin
                Inc(Nelem);
                if Nelem > Nallocated then
                    ResizeSubList(NewListPtr^[HashNum]);
            end;

            with NewListPtr^[hashNum] do
            begin
                Str^[Nelem] := S;
                NewStringPtr^[NumElements] := Str^[Nelem];
                Idx^[Nelem] := i;
            end;

        end;

{Dump the old StringPtr and ListPtr storage}

        for i := 1 to OldNumLists do
        begin
         {DeAllocate  Sublists}
            with ListPtr^[i] do
            begin
                for j := 1 to Nelem do
                    Str^[j] := ''; // decrement ref count on string
                Freemem(Str, SizeOf(Str^[1]) * Nallocated);
                Freemem(Idx, SizeOf(Idx^[1]) * Nallocated);
            end;
        end;
        Freemem(ListPtr, Sizeof(Listptr^[1]) * OldNumLists);
        Freemem(StringPtr, Sizeof(StringPtr^[1]) * NumElementsAllocated);

{Assign new String and List Pointers}

        StringPtr := NewStringPtr;
        ListPtr := NewListPtr;
        NumElementsAllocated := NewSize;

    end;
end;

procedure THashList.DumpToFile(const fname: String);
var
    F: TextFile;
    i, j: Integer;
begin

    AssignFile(F, fname);
    Rewrite(F);
    Writeln(F, Format('Number of Hash Lists = %d, Number of Elements = %d', [NumLists, NumElements]));

    Writeln(F);
    Writeln(F, 'Hash List Distribution');
    for i := 1 to NumLists do
    begin
        with ListPtr^[i] do
        begin
            Writeln(F, Format('List = %d, Number of elements = %d', [i, Nelem]));
        end;
    end;
    Writeln(F);

    for i := 1 to NumLists do
    begin
        with ListPtr^[i] do
        begin
            Writeln(F, Format('List = %d, Number of elements = %d', [i, Nelem]));
            for j := 1 to Nelem do
                Writeln(F, '"', Str^[j], '"  Idx= ', Idx^[j]: 0);
        end;
        Writeln(F);
    end;
    Writeln(F, 'LINEAR LISTING...');
    for i := 1 to NumElements do
    begin
        Writeln(F, i: 3, ' = "', Stringptr^[i], '"');
    end;
    CloseFile(F);

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

end.
