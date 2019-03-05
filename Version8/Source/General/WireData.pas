unit WireData;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{Used for overhead line impedances.
}

uses
    Command,
    DSSClass,
    DSSObject,
    ConductorData;

type
    TWireData = class(TConductorData)
    PRIVATE
        function Get_Code: String;  // Returns active line code string
        procedure Set_Code(const Value: String);  // sets the  active WireData
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const WireName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

      // Set this property to point ActiveWireDataObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;
    end;

    TWireDataObj = class(TConductorDataObj)
    PUBLIC
        constructor Create(ParClass: TDSSClass; const WireDataName: String);
        destructor Destroy; OVERRIDE;

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
    end;

implementation

uses
    ParserDel,
    DSSGlobals,
    DSSClassDefs,
    Sysutils,
    Ucomplex,
    Arraydef,
    LineUNits;

const
    NumPropsThisClass = 0; // because they were all moved to ConductorData

constructor TWireData.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'WireData';
    DSSClassType := DSS_OBJECT;
    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

destructor TWireData.Destroy;
begin
    inherited Destroy;
end;

procedure TWireData.DefineProperties;
begin
    NumProperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list
end;

function TWireData.NewObject(const ObjName: String): Integer;
begin
  // create a new object of this class and add to list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveDSSObject[ActiveActor] := TWireDataObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

function TWireData.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveConductorDataObj := ElementList.Active;
    ActiveDSSObject[ActorID] := ActiveConductorDataObj;
    with ActiveConductorDataObj do
    begin
        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 101);
            else
        // Inherited parameters
                ClassEdit(ActiveConductorDataObj, ParamPointer - NumPropsThisClass)
            end;
            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;
    end;
end;

function TWireData.MakeLike(const WireName: String): Integer;
var
    OtherWireData: TWireDataObj;
    i: Integer;
begin
    Result := 0;
    OtherWireData := Find(WireName);
    if OtherWireData <> NIL then
        with ActiveConductorDataObj do
        begin
            ClassMakeLike(OtherWireData);
            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherWireData.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in Wire MakeLike: "' + WireName + '" Not Found.', 102);
end;

function TWireData.Init(Handle: Integer; ActorID: Integer): Integer;
begin
    DoSimpleMsg('Need to implement TWireData.Init', -1);
    REsult := 0;
end;

function TWireData.Get_Code: String;  // Returns active line code string
begin
    Result := TWireDataObj(ElementList.Active).Name;
end;

procedure TWireData.Set_Code(const Value: String);  // sets the  active WireData
var
    WireDataObj: TWireDataObj;
begin
    ActiveConductorDataObj := NIL;
    WireDataObj := ElementList.First;
    while WireDataObj <> NIL do
    begin
        if CompareText(WireDataObj.Name, Value) = 0 then
        begin
            ActiveConductorDataObj := WireDataObj;
            Exit;
        end;
        WireDataObj := ElementList.Next;
    end;
    DoSimpleMsg('WireData: "' + Value + '" not Found.', 103);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TWireData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TWireDataObj.Create(ParClass: TDSSClass; const WireDataName: String);
begin
    inherited Create(ParClass, WireDataName);
    Name := LowerCase(WireDataName);
    DSSObjType := ParClass.DSSClassType;
    InitPropertyValues(0);
end;

destructor TWireDataObj.Destroy;
begin
    inherited destroy;
end;

procedure TWireDataObj.DumpProperties(var F: TextFile; Complete: Boolean);
begin
    inherited DumpProperties(F, Complete);
end;

procedure TWireDataObj.InitPropertyValues(ArrayOffset: Integer);
begin
    inherited InitPropertyValues(ArrayOffset + NumPropsThisClass);
end;

end.
