unit DSSObject;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Classes,
    ParserDel,
    Arraydef,
    DSSClass,
    NamedObject,
    CAPI_Types;

type
    TDSSObjectPtr = ^TDSSObject;

    TDSSObject = class(TNamedObject)
    PRIVATE
        procedure Set_Name(const Value: String);

    PUBLIC
        DSS: TDSSContext;

        PrpSequence: pIntegerArray0;

        function GetNextPropertySet(idx: Integer): Integer;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); virtual;

        // DSSContext convenience functions
        procedure DoErrorMsg(Const S, Emsg, ProbCause: String; ErrNum: Integer);inline;
        procedure DoSimpleMsg(Const S: String; ErrNum:Integer);inline;overload;
        procedure DoSimpleMsg(Const S: String; fmtArgs: Array of Const; ErrNum:Integer);overload;inline;

        procedure AppendToEventLog(const opdev: String; const action: String);inline;

    PUBLIC
        DSSObjType: Integer; // PD, PC, Monitor, LineCode, etc.
        ParentClass: TDSSClass;
        ClassIndex: Integer;    // Index into the class collection list

        Flags: TDSSObjectFlags;

        constructor Create(ParClass: TDSSClass);
        destructor Destroy; OVERRIDE;

        function Edit(Parser: TDSSParser): Integer;  // Allow Calls to edit from object itself
        procedure MakeLike(OtherPtr: Pointer); virtual;

        procedure SetAsNextSeq(Index: Integer); inline;

        function GetPropertyValue(Index: Integer): String; VIRTUAL;  // Use dssclass.propertyindex to get index by name
        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); VIRTUAL;
        procedure SaveWrite(F: TFileStream); VIRTUAL;
        procedure CustomSetRaw(Idx: Integer; Value: String); virtual;

        property PropertyValue[Index: Integer]: String READ GetPropertyValue;

        property Name: String READ LName WRITE Set_Name;
        function FullName: String;
        function DSSClassName: String;
    end;

implementation

uses
    Sysutils,
    Utilities,
    LoadShape,
    DSSGlobals,
    UComplex, DSSUcomplex,
    DSSObjectHelper;


procedure TDSSObject.CustomSetRaw(Idx: Integer; Value: String);
begin
    DoSimpleMsg('Error: base CustomSetRaw reached', 8754);
end;

constructor TDSSObject.Create(ParClass: TDSSClass);
begin
    inherited Create(ParClass.Name);
    DSS := ParClass.DSS;

    DSSObjType := 0;
    ParentClass := ParClass;
    // init'd to zero when allocated
    PrpSequence := Allocmem(SizeOf(Integer) * (ParentClass.NumProperties + 1));

    Flags := [];
end;

destructor TDSSObject.Destroy;
begin
    if DSS = nil then
    begin
        inherited Destroy;
        exit;
    end;
    Reallocmem(PrpSequence, 0);

    inherited Destroy;
end;

procedure TDSSObject.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
var
    i: Integer;
begin
    FSWriteln(F);
    FSWriteln(F, 'New ' + EncloseQuotes(FullName));
    if Leaf then
    begin
        with ParentClass do
            for i := 1 to NumProperties do
                FSWriteLn(F, '~ ' + PropertyName^[i] + '=' + GetPropertyValue(i));
        
        if Complete then
            FSWriteln(F);        
    end;
end;

function TDSSObject.Edit(Parser: TDSSParser): Integer;
begin
    ParentClass.Active := ClassIndex;
    Result := ParentClass.Edit(Parser);
end;

procedure TDSSObject.MakeLike(OtherPtr: Pointer);
var
    other: TDSSObject;
begin
    other := TDSSObject(OtherPtr);
    Move(other.PrpSequence[0], PrpSequence[0], SizeOf(Integer) * (ParentClass.NumProperties + 1));
end;

function TDSSObject.GetPropertyValue(Index: Integer): String;
begin
    ParentClass.GetObjPropertyValue(self, Index, Result);
end;

procedure TDSSObject.SaveWrite(F: TFileStream);
var
    iprop: Integer;
    str: String;
begin
    // Write only properties that were explicitly set in the
    // final order they were actually set
    iProp := GetNextPropertySet(-9999999);
    while iProp > 0 do
    begin
        str := trim(PropertyValue[iProp]);
        if Comparetext(str, '----') = 0 then
            str := ''; // set to ignore this property
        if Length(str) > 0 then
        begin
            with ParentClass do
                FSWrite(F, ' ' + PropertyName[iProp]);
            FSWrite(F, '=' + CheckForBlanks(str));
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
        if (PrpSequence^[i] <> 0) and (PrpSequence^[i] > idx) then
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

procedure TDSSObject.SetAsNextSeq(Index: Integer); inline;
begin
    // Keep track of the order in which this property was accessed for Save Command
    Inc(PrpSequence[0]);
    PrpSequence[Index] := PrpSequence[0];
end;

procedure TDSSObject.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
end;

procedure TDSSObject.DoErrorMsg(Const S, Emsg, ProbCause: String; ErrNum: Integer);
begin
    DSSGlobals.DoErrorMsg(DSS, S, Emsg, ProbCause, ErrNum)
end;

procedure TDSSObject.DoSimpleMsg(Const S: String; ErrNum:Integer);
begin
    DSSGlobals.DoSimpleMsg(DSS, S, ErrNum)
end;
          
procedure TDSSObject.DoSimpleMsg(Const S: String; fmtArgs: Array of Const; ErrNum:Integer);
begin
    DSSGlobals.DoSimpleMsg(DSS, DSSTranslate(S), fmtArgs, ErrNum)
end;

procedure TDSSObject.AppendToEventLog(const opdev: String; const action: String);
begin
    Utilities.AppendtoEventLog(DSS, opdev, action);
end;

function TDSSObject.FullName: String;
begin
    Result := ParentClass.Name + '.' + Name;
end;

function TDSSObject.DSSClassName: String;
begin
    Result := ParentClass.Name;
end;

end.
