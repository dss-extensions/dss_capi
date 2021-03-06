unit WireData;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2020, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{Used for overhead line impedances.
}

uses
    Classes,
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
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

      // Set this property to point ActiveWireDataObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;
    end;

    TWireDataObj = class(TConductorDataObj)
    PUBLIC
        constructor Create(ParClass: TDSSClass; const WireDataName: String);
        destructor Destroy; OVERRIDE;

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(F: TFileStream; Complete: Boolean); OVERRIDE;
    end;

implementation

uses
    ParserDel,
    DSSGlobals,
    DSSClassDefs,
    Sysutils,
    Ucomplex,
    Arraydef,
    LineUNits,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

const
    NumPropsThisClass = 0; // because they were all moved to ConductorData

constructor TWireData.Create(dssContext: TDSSContext);  // Creates superstructure for all Line objects
begin
    inherited Create(dssContext);
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
    DSS.ActiveDSSObject := TWireDataObj.Create(Self, ObjName);
    Result := AddObjectToList(DSS.ActiveDSSObject);
end;

function TWireData.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
begin
    Result := 0;
  // continue parsing with contents of Parser
    DSS.ActiveConductorDataObj := ElementList.Active;
    DSS.ActiveDSSObject := DSS.ActiveConductorDataObj;
    with DSS.ActiveConductorDataObj do
    begin
        ParamPointer := 0;
        ParamName := Parser.NextParam;
        Param := Parser.StrValue;
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
                ClassEdit(DSS.ActiveConductorDataObj, ParamPointer - NumPropsThisClass)
            end;
            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
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
        with DSS.ActiveConductorDataObj do
        begin
            ClassMakeLike(OtherWireData);
            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherWireData.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in Wire MakeLike: "' + WireName + '" Not Found.', 102);
end;

function TWireData.Get_Code: String;  // Returns active line code string
begin
    Result := TWireDataObj(ElementList.Active).Name;
end;

procedure TWireData.Set_Code(const Value: String);  // sets the  active WireData
var
    WireDataObj: TWireDataObj;
begin
    DSS.ActiveConductorDataObj := NIL;
    WireDataObj := ElementList.First;
    while WireDataObj <> NIL do
    begin
        if CompareText(WireDataObj.Name, Value) = 0 then
        begin
            DSS.ActiveConductorDataObj := WireDataObj;
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

procedure TWireDataObj.DumpProperties(F: TFileStream; Complete: Boolean);
begin
    inherited DumpProperties(F, Complete);
end;

procedure TWireDataObj.InitPropertyValues(ArrayOffset: Integer);
begin
    inherited InitPropertyValues(ArrayOffset + NumPropsThisClass);
end;

end.
