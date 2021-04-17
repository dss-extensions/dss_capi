unit DSSObject;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Arraydef,
    DSSClass,
    NamedObject;

type

    TDSSObject = class(TNamedObject)
    PRIVATE
        function Get_PropertyValue(Index: Integer): String;
        procedure Set_PropertyValue(Index: Integer; const Value: String);
        function Get_Name: String;
        procedure Set_Name(const Value: String);

    PROTECTED

        PropSeqCount: Integer;
        FPropertyValue: pStringArray;
        PrpSequence: pIntegerArray;

        function GetNextPropertySet(idx: Integer): Integer;

    PUBLIC

        DSSObjType: Integer; // PD, PC, Monitor, CondCode, etc.
        ParentClass: TDSSClass;
        ClassIndex: Integer;    // Index into the class collection list

        HasBeenSaved: Boolean;
        Flag: Boolean;  // General purpose Flag for each object  don't assume inited

        constructor Create(ParClass: TDSSClass);
        destructor Destroy; OVERRIDE;

        function Edit: Integer;  // Allow Calls to edit from object itself

      {Get actual values of properties}
        function GetPropertyValue(Index: Integer): String; VIRTUAL;  // Use dssclass.propertyindex to get index by name
        procedure InitPropertyValues(ArrayOffset: Integer); VIRTUAL;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); VIRTUAL;
        procedure SaveWrite(var F: TextFile); VIRTUAL;

        procedure ClearPropSeqArray;

        property PropertyValue[Index: Integer]: String READ Get_PropertyValue WRITE Set_PropertyValue;

        property Name: String READ Get_Name WRITE Set_Name;
    end;


implementation

uses
    Sysutils,
    Utilities;

procedure TDSSObject.ClearPropSeqArray;
var
    i: Integer;
begin
    PropSeqCount := 0;
    for i := 1 to ParentClass.NumProperties do
        PrpSequence^[i] := 0;

end;

constructor TDSSObject.Create(ParClass: TDSSClass);
begin
    inherited Create(ParClass.Name);
    DSSObjType := 0;
    PropSeqCount := 0;
    ParentClass := ParClass;
    FPropertyValue := Allocmem(SizeOf(FPropertyValue^[1]) * ParentClass.NumProperties);

   // init'd to zero when allocated
    PrpSequence := Allocmem(SizeOf(PrpSequence^[1]) * ParentClass.NumProperties);

    HasBeenSaved := FALSE;

end;

destructor TDSSObject.Destroy;

var
    i: Integer;

begin
    for i := 1 to ParentClass.NumProperties do
        FPropertyValue^[i] := '';
    Reallocmem(FPropertyValue, 0);
    Reallocmem(PrpSequence, 0);

    inherited Destroy;
end;


procedure TDSSObject.DumpProperties(var F: TextFile; Complete: Boolean);
begin
    Writeln(F);
    Writeln(F, 'New ', DSSClassName, '.', Name);
end;

function TDSSObject.Edit: Integer;
begin
    ParentClass.Active := ClassIndex;
    Result := ParentClass.Edit;
end;

function TDSSObject.GetPropertyValue(Index: Integer): String;
begin
    Result := FPropertyValue^[Index];  // Default Behavior   for all DSS Objects
end;

function TDSSObject.Get_PropertyValue(Index: Integer): String;
begin
    Result := GetPropertyValue(Index);  // This is virtual function that may call routine
end;

procedure TDSSObject.InitPropertyValues(ArrayOffset: Integer);
begin
    PropertyValue[ArrayOffset + 1] := ''; //Like   Property

     // Clear propertySequence Array  after initialization
    ClearPropSeqArray;

end;

procedure TDSSObject.SaveWrite(var F: TextFile);
var
    iprop: Integer;
    str: String;
begin
   {Write only properties that were explicitly set in the
   final order they were actually set}
    iProp := GetNextPropertySet(0); // Works on ActiveDSSObject
    while iProp > 0 do
    begin
        str := trim(PropertyValue[iProp]);
        if Comparetext(str, '----') = 0 then
            str := ''; // set to ignore this property
        if Length(str) > 0 then
        begin
            with ParentClass do
                Write(F, ' ', PropertyName^[RevPropertyIdxMap[iProp]]);
            Write(F, '=', CheckForBlanks(str));
        end;
        iProp := GetNextPropertySet(iProp);
    end;
end;

function TDSSObject.GetNextPropertySet(idx: Integer): Integer;
// Find next larger property sequence number
// return 0 if none found

var
    i, smallest: Integer;
begin

    Smallest := 9999999; // some big number
    Result := 0;

    if idx > 0 then
        idx := PrpSequence^[idx];
    for i := 1 to ParentClass.NumProperties do
    begin
        if PrpSequence^[i] > idx then
            if PrpSequence^[i] < Smallest then
            begin
                Smallest := PrpSequence^[i];
                Result := i;
            end;
    end;

end;

procedure TDSSObject.Set_Name(const Value: String);
begin
// If renamed, then let someone know so hash list can be updated;
    if Length(LocalName) > 0 then
        ParentClass.ElementNamesOutOfSynch := TRUE;
    LocalName := Value;
end;

function TDSSObject.Get_Name: String;
begin
    Result := LocalName;
end;

procedure TDSSObject.Set_PropertyValue(Index: Integer;
    const Value: String);
begin
    FPropertyValue^[Index] := Value;

    // Keep track of the order in which this property was accessed for Save Command
    Inc(PropSeqCount);
    PrpSequence^[Index] := PropSeqCount;
end;

end.
