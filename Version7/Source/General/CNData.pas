unit CNData;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses
    Command,
    DSSClass,
    DSSObject,
    ConductorData,
    CableData;

type
    TCNData = class(TCableData)
    PRIVATE
        function Get_Code: String;  // Returns active line code string
        procedure Set_Code(const Value: String);  // sets the  active CNData
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const CNName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create(dss: TDSS);
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

      // Set this property to point ActiveCNDataObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;
    end;

    TCNDataObj = class(TCableDataObj)
{$IFDEF DSS_CAPI}
    PUBLIC
{$ELSE}
    PRIVATE
{$ENDIF}
        FkStrand: Integer;
        FDiaStrand: Double;
        FGmrStrand: Double;
        FRStrand: Double;
    PUBLIC

        constructor Create(ParClass: TDSSClass; const CNDataName: String);
        destructor Destroy; OVERRIDE;

        property NStrand: Integer READ FkStrand;
        property DiaStrand: Double READ FDiaStrand;
        property GmrStrand: Double READ FGmrStrand;
        property RStrand: Double READ FRStrand;

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
    LineUNits,
    DSSHelper;

const
    NumPropsThisClass = 4;

constructor TCNData.Create(dss: TDSS);  // Creates superstructure for all Line objects
begin
    inherited Create(dss);
    Class_Name := 'CNData';
    DSSClassType := DSS_OBJECT;
    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

destructor TCNData.Destroy;
begin
    inherited Destroy;
end;

procedure TCNData.DefineProperties;
begin
    NumProperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

    PropertyName[1] := 'k';
    PropertyName[2] := 'DiaStrand';
    PropertyName[3] := 'GmrStrand';
    PropertyName[4] := 'Rstrand';

    PropertyHelp[1] := 'Number of concentric neutral strands; default is 2';
    PropertyHelp[2] := 'Diameter of a concentric neutral strand; same units as core conductor radius; no default.';
    PropertyHelp[3] := 'Geometric mean radius of a concentric neutral strand; same units as core conductor GMR; defaults to 0.7788 * CN strand radius.';
    PropertyHelp[4] := 'AC resistance of a concentric neutral strand; same units as core conductor resistance; no default.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list
end;

function TCNData.NewObject(const ObjName: String): Integer;
begin
    with DSS.ActiveCircuit do
    begin
        DSS.ActiveDSSObject := TCNDataObj.Create(Self, ObjName);
        Result := AddObjectToList(DSS.ActiveDSSObject);
    end;
end;

function TCNData.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
begin
    Result := 0;
  // continue parsing with contents of Parser
    DSS.ActiveConductorDataObj := ElementList.Active;
    DSS.ActiveDSSObject := DSS.ActiveConductorDataObj;
    with TCNDataObj(DSS.ActiveConductorDataObj) do
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
                    FkStrand := Parser.IntValue;
                2:
                    FDiaStrand := Parser.DblValue;
                3:
                    FGmrStrand := Parser.DblValue;
                4:
                    FRStrand := Parser.DblValue;
            else
        // Inherited parameters
                ClassEdit(DSS.ActiveConductorDataObj, ParamPointer - NumPropsThisClass)
            end;

      {Set defaults}
            case ParamPointer of
                2:
                    if FGmrStrand <= 0.0 then
                        FGmrStrand := 0.7788 * 0.5 * FDiaStrand;
            end;

      {Check for critical errors}
            case ParamPointer of
                1:
                    if (FkStrand < 2) then
                        DoSimpleMsg('Error: Must have at least 2 concentric neutral strands for CNData ' + Name, 999);
                2:
                    if (FDiaStrand <= 0.0) then
                        DoSimpleMsg('Error: Neutral strand diameter must be positive for CNData ' + Name, 999);
                3:
                    if (FGmrStrand <= 0.0) then
                        DoSimpleMsg('Error: Neutral strand GMR must be positive for CNData ' + Name, 999);
            end;
            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;
    end;
end;

function TCNData.MakeLike(const CNName: String): Integer;
var
    OtherData: TCNDataObj;
    i: Integer;
begin
    Result := 0;
    OtherData := Find(CNName);
    if OtherData <> NIL then
        with TCNDataObj(DSS.ActiveConductorDataObj) do
        begin
            FkStrand := OtherData.FkStrand;
            FDiaStrand := OtherData.FDiaStrand;
            FGmrStrand := OtherData.FGmrStrand;
            FRStrand := OtherData.FRStrand;
            ClassMakeLike(OtherData);
            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherData.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in Concentric Neutral MakeLike: "' + CNName + '" Not Found.', 102);
end;

function TCNData.Init(Handle: Integer): Integer;
begin
    DoSimpleMsg('Need to implement TCNData.Init', -1);
    Result := 0;
end;

function TCNData.Get_Code: String;  // Returns active line code string
begin
    Result := TCNDataObj(ElementList.Active).Name;
end;

procedure TCNData.Set_Code(const Value: String);  // sets the  active CNData
var
    CNDataObj: TCNDataObj;
begin
    DSS.ActiveConductorDataObj := NIL;
    CNDataObj := ElementList.First;
    while CNDataObj <> NIL do
    begin
        // TODO: fast lookup
        if CompareText(CNDataObj.Name, Value) = 0 then
        begin
            DSS.ActiveConductorDataObj := CNDataObj;
            Exit;
        end;
        CNDataObj := ElementList.Next;
    end;
    DoSimpleMsg('CNData: "' + Value + '" not Found.', 103);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TCNData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCNDataObj.Create(ParClass: TDSSClass; const CNDataName: String);
begin
    inherited Create(ParClass, CNDataName);
    Name := LowerCase(CNDataName);
    DSSObjType := ParClass.DSSClassType;
    FkStrand := 2;
    FDiaStrand := -1.0;
    FGmrStrand := -1.0;
    FRStrand := -1.0;
    InitPropertyValues(0);
end;

destructor TCNDataObj.Destroy;
begin
    inherited destroy;
end;

procedure TCNDataObj.DumpProperties(var F: TextFile; Complete: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);
    with ParentClass do
    begin
        for i := 1 to NumProperties do
        begin
            Write(F, '~ ', PropertyName^[i], '=');
            case i of
                1:
                    Writeln(F, Format('%d', [FkStrand]));
                2:
                    Writeln(F, Format('%.6g', [FDiaStrand]));
                3:
                    Writeln(F, Format('%.6g', [FGmrStrand]));
                4:
                    Writeln(F, Format('%.6g', [FRStrand]));
            end;
        end;
    end;
end;

procedure TCNDataObj.InitPropertyValues(ArrayOffset: Integer);
begin
    PropertyValue[1] := '2';
    PropertyValue[2] := '-1';
    PropertyValue[3] := '-1';
    PropertyValue[4] := '-1';
    inherited InitPropertyValues(ArrayOffset + NumPropsThisClass);
end;

end.
