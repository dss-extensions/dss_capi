unit TSData;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    CableData,
    ConductorData;

type
    TTSData = class(TCableData)
    PRIVATE
        function Get_Code: String;
        procedure Set_Code(const Value: String);
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const TSName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

       // Set this property to point ActiveTSDataObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;
    end;

    TTSDataObj = class(TCableDataObj)
{$IFDEF DSS_CAPI}
    PUBLIC
{$ELSE}
    PRIVATE
{$ENDIF}
        FDiaShield: Double;
        FTapeLayer: Double;
        FTapeLap: Double;
    PUBLIC

        constructor Create(ParClass: TDSSClass; const TSDataName: String);
        destructor Destroy; OVERRIDE;

        property DiaShield: Double READ FDiaShield;
        property TapeLayer: Double READ FTapeLayer;
        property TapeLap: Double READ FTapeLap;

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
    LineUnits,
    Utilities;

const
    NumPropsThisClass = 3;

constructor TTSData.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'TSData';
    DSSClassType := DSS_OBJECT;
    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

destructor TTSData.Destroy;
begin
    inherited Destroy;
end;

procedure TTSData.DefineProperties;
begin
    NumProperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

    PropertyName[1] := 'DiaShield';
    PropertyName[2] := 'TapeLayer';
    PropertyName[3] := 'TapeLap';

    PropertyHelp[1] := 'Diameter over tape shield; same units as radius; no default.';
    PropertyHelp[2] := 'Tape shield thickness; same units as radius; no default.';
    PropertyHelp[3] := 'Tape Lap in percent; default 20.0';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list
end;

function TTSData.NewObject(const ObjName: String): Integer;
begin
    with ActiveCircuit do
    begin
        ActiveDSSObject := TTSDataObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

function TTSData.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveConductorDataObj := ElementList.Active;
    ActiveDSSObject := ActiveConductorDataObj;
    with TTSDataObj(ActiveConductorDataObj) do
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
                1:
                    FDiaShield := Parser.DblValue;
                2:
                    FTapeLayer := Parser.DblValue;
                3:
                    FTapeLap := Parser.DblValue;
            else
        // Inherited parameters
                ClassEdit(ActiveConductorDataObj, ParamPointer - NumPropsThisClass)
            end;

      {Check for critical errors}
            case ParamPointer of
                1:
                    if (FDiaShield <= 0.0) then
                        DoSimpleMsg('Error: Diameter over shield must be positive for TapeShieldData ' + Name, 999);
                2:
                    if (FTapeLayer <= 0.0) then
                        DoSimpleMsg('Error: Tape shield thickness must be positive for TapeShieldData ' + Name, 999);
                3:
                    if ((FTapeLap < 0.0) or (FTapeLap > 100.0)) then
                        DoSimpleMsg('Error: Tap lap must range from 0 to 100 for TapeShieldData ' + Name, 999);
            end;
            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;
    end;
end;

function TTSData.MakeLike(const TSName: String): Integer;
var
    OtherData: TTSDataObj;
    i: Integer;
begin
    Result := 0;
    OtherData := Find(TSName);
    if OtherData <> NIL then
        with TTSDataObj(ActiveConductorDataObj) do
        begin
            FDiaShield := OtherData.FDiaShield;
            FTapeLayer := OtherData.FTapeLayer;
            FTapeLap := OtherData.FTapeLap;
            ClassMakeLike(OtherData);
            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherData.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in TapeShield MakeLike: "' + TSName + '" Not Found.', 102);
end;

function TTSData.Get_Code: String;  // Returns active line code string
begin
    Result := TTSDataObj(ElementList.Active).Name;
end;

procedure TTSData.Set_Code(const Value: String);  // sets the  active TSData
var
    TSDataObj: TTSDataObj;
begin
    ActiveConductorDataObj := NIL;
    TSDataObj := ElementList.First;
    while TSDataObj <> NIL do
    begin
        if CompareText(TSDataObj.Name, Value) = 0 then
        begin
            ActiveConductorDataObj := TSDataObj;
            Exit;
        end;
        TSDataObj := ElementList.Next;
    end;
    DoSimpleMsg('TSData: "' + Value + '" not Found.', 103);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TTSData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TTSDataObj.Create(ParClass: TDSSClass; const TSDataName: String);
begin
    inherited Create(ParClass, TSDataName);
    Name := LowerCase(TSDataName);
    DSSObjType := ParClass.DSSClassType;
    FDiaShield := -1.0;
    FTapeLayer := -1.0;
    FTapeLap := 20.0;
    InitPropertyValues(0);
end;

destructor TTSDataObj.Destroy;
begin
    inherited destroy;
end;

procedure TTSDataObj.DumpProperties(F: TFileStream; Complete: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);
    with ParentClass do
    begin
        for i := 1 to NumProperties do
        begin
            FSWrite(F, '~ ' + PropertyName^[i] + '=');
            case i of
                1:
                    FSWriteln(F, Format('%.6g', [FDiaShield]));
                2:
                    FSWriteln(F, Format('%.6g', [FTapeLayer]));
                3:
                    FSWriteln(F, Format('%.2g', [FTapeLap]));
            end;
        end;
    end;
end;

procedure TTSDataObj.InitPropertyValues(ArrayOffset: Integer);
begin
    PropertyValue[1] := '-1';
    PropertyValue[2] := '-1';
    PropertyValue[3] := '20.0';
    inherited InitPropertyValues(ArrayOffset + NumPropsThisClass);
end;

end.
