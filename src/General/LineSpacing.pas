unit LineSpacing;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Classes,
    Sysutils,
    Arraydef,
    Command,
    DSSClass,
    DSSObject;

type
    SpcParmChoice = (X, H);

    TLineSpacing = class(TDSSClass)
    PRIVATE

        function Get_Code: String;  // Returns active line code string
        procedure Set_Code(const Value: String);  // sets the  active LineSpacing
        procedure InterpretArray(const S: String; which: SpcParmChoice);

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const LineName: String): Integer; OVERRIDE;
    PUBLIC

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;


       // Set this property to point ActiveLineSpacingObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;

    end;


    TLineSpacingObj = class(TDSSObject)
    PUBLIC
        FX: pDoubleArray;
        FY: pDoubleArray;
    PRIVATE
        FNConds: Integer;
        FNPhases: Integer;
        FUnits: Integer;
{$IFNDEF DSS_CAPI}
        DataChanged: Boolean;
{$ENDIF}

        procedure set_Nwires(const Value: Integer);

        // CIM Accessors
        function Get_FX(i: Integer): Double;
        function Get_FY(i: Integer): Double;
{$IFDEF DSS_CAPI}
        procedure Set_FX(i: Integer; Value: Double);
        procedure Set_FY(i: Integer; Value: Double);
        procedure Set_FUnits(Value: Integer);
{$ENDIF}
    PUBLIC
{$IFDEF DSS_CAPI}
        DataChanged: Boolean;
{$ENDIF}
        constructor Create(ParClass: TDSSClass; const LineSpacingName: String);
        destructor Destroy; OVERRIDE;

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(F: TFileStream; Complete: Boolean); OVERRIDE;

        // CIM XML accessors
        property Xcoord[i: Integer]: Double READ Get_FX
{$IFDEF DSS_CAPI}
            WRITE Set_FX
{$ENDIF}
        ;
        property Ycoord[i: Integer]: Double READ Get_FY
{$IFDEF DSS_CAPI}
            WRITE Set_FY
{$ENDIF}
        ;
        property NWires: Integer READ FNConds WRITE set_Nwires;
        property NPhases: Integer READ FNPhases
{$IFDEF DSS_CAPI}
            WRITE FNPhases
{$ENDIF}
        ;
        property Units: Integer READ FUnits
{$IFDEF DSS_CAPI}
            WRITE Set_FUnits
{$ENDIF}
        ;
    end;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Ucomplex,
    Utilities,
    LineUnits,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

const
    NumPropsThisClass = 5;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLineSpacing.Create(dssContext: TDSSContext);  // Creates superstructure for all Line objects
begin
    inherited Create(dssContext);
    Class_Name := 'LineSpacing';
    DSSClassType := DSS_OBJECT;
    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLineSpacing.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLineSpacing.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


    PropertyName[1] := 'nconds';
    PropertyName[2] := 'nphases';
    PropertyName[3] := 'x';
    PropertyName[4] := 'h';
    PropertyName[5] := 'units';


    PropertyHelp[1] := 'Number of wires in this geometry. Default is 3. Triggers memory allocations. Define first!';
    PropertyHelp[2] := 'Number of retained phase conductors. If less than the number of wires, list the retained phase coordinates first.';
    PropertyHelp[3] := 'Array of wire X coordinates.';
    PropertyHelp[4] := 'Array of wire Heights.';
    PropertyHelp[5] := 'Units for x and h: {mi|kft|km|m|Ft|in|cm } Initial default is "ft", but defaults to last unit defined';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLineSpacing.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    DSS.ActiveDSSObject := TLineSpacingObj.Create(Self, ObjName);
    Result := AddObjectToList(DSS.ActiveDSSObject);
end;

procedure TLineSpacing.InterpretArray(const S: String; which: SpcParmChoice);
var
    Str: String;
    i: Integer;
begin
    AuxParser.CmdString := S;
    with DSS.ActiveLineSpacingObj do
    begin
        for i := 1 to NWires do
        begin
            AuxParser.NextParam; // ignore any parameter name  not expecting any
            Str := AuxParser.StrValue;
            if Length(Str) > 0 then
                case which of
                    X:
                        FX^[i] := AuxParser.Dblvalue;
                    H:
                        FY^[i] := AuxParser.Dblvalue;
                end;
        end;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLineSpacing.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin
    Result := 0;
  // continue parsing with contents of Parser
    DSS.ActiveLineSpacingObj := ElementList.Active;
    DSS.ActiveDSSObject := DSS.ActiveLineSpacingObj;

    with DSS.ActiveLineSpacingObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 10101);
                1:
                    NWires := Parser.IntValue;  // Use property value to force reallocations
                2:
                    FNPhases := Parser.IntValue;
                3:
                    InterpretArray(Param, X);
                4:
                    InterpretArray(Param, H);
                5:
                    FUnits := GetUnitsCode(Param);
            else
                // Inherited parameters
                ClassEdit(DSS.ActiveLineSpacingObj, Parampointer - NumPropsThisClass)
            end;

            case ParamPointer of
                1..5:
                    DataChanged := TRUE;
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLineSpacing.MakeLike(const LineName: String): Integer;
var
    OtherLineSpacing: TLineSpacingObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this line code in the present collection}
    OtherLineSpacing := Find(LineName);
    if OtherLineSpacing <> NIL then
        with DSS.ActiveLineSpacingObj do
        begin

            NWires := OtherLineSpacing.NWires;   // allocates
            FNPhases := OtherLineSpacing.NPhases;
            for i := 1 to FNConds do
                FX^[i] := OtherLineSpacing.FX^[i];
            for i := 1 to FNConds do
                FY^[i] := OtherLineSpacing.FY^[i];
            FUnits := OtherLineSpacing.FUnits;
            DataChanged := TRUE;
            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherLineSpacing.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in LineSpacing MakeLike: "' + LineName + '" Not Found.', 102);


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLineSpacing.Get_Code: String;  // Returns active line code string
begin
    Result := TLineSpacingObj(ElementList.Active).Name;
end;

procedure TLineSpacing.Set_Code(const Value: String);  // sets the  active LineSpacing
var
    LineSpacingObj: TLineSpacingObj;
begin

    DSS.ActiveLineSpacingObj := NIL;
    LineSpacingObj := ElementList.First;
    while LineSpacingObj <> NIL do
    begin

        if CompareText(LineSpacingObj.Name, Value) = 0 then
        begin
            DSS.ActiveLineSpacingObj := LineSpacingObj;
            Exit;
        end;

        LineSpacingObj := ElementList.Next;
    end;

    DoSimpleMsg('LineSpacing: "' + Value + '" not Found.', 103);

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLineSpacing Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


constructor TLineSpacingObj.Create(ParClass: TDSSClass; const LineSpacingName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(LineSpacingName);
    DSSObjType := ParClass.DSSClassType;

    DataChanged := TRUE;
    FX := NIL;
    FY := NIL;
    Funits := UNITS_FT;
    NWires := 3;  // Allocates terminals
    FNPhases := 3;

    InitPropertyValues(0);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLineSpacingObj.Destroy;
begin

    Reallocmem(FY, 0);
    Reallocmem(FX, 0);

    inherited destroy;

end;


procedure TLineSpacingObj.DumpProperties(F: TFileStream; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
    begin
        for i := 1 to 5 do
        begin
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + GetPropertyValue(i));
        end;
    end;

end;

function ArrayString(pF: pDoubleArray; N: Integer): String;
var
    i: Integer;
    r: String;
begin
    r := '[';
    if N > 0 then
        r := r + Format('%-g', [pF^[1]]);
    for i := 2 to N do
        r := r + Format(',%-g', [pF^[i]]);
    Result := r + ']';
end;

function TLineSpacingObj.GetPropertyValue(Index: Integer): String;

{Return Property Value for Active index}

begin

    case Index of
        3:
            Result := ArrayString(FX, FNConds);
        4:
            Result := ArrayString(FY, FNConds);
        5:
            Result := LineUnitsStr(FUnits);
    else
     // Inherited parameters
        Result := inherited GetPropertyValue(Index);
    end;

end;

function TLineSpacingObj.Get_FX(i: Integer): Double;
begin
    if i <= FNConds then
        Result := FX^[i]
    else
        Result := 0.0;
end;

function TLineSpacingObj.Get_FY(i: Integer): Double;
begin
    if i <= FNConds then
        Result := FY^[i]
    else
        Result := 0.0;
end;


{$IFDEF DSS_CAPI}
procedure TLineSpacingObj.Set_FX(i: Integer; Value: Double);
begin
    if (i > 0) and (i <= FNConds) then
        FX^[i] := Value;
end;

procedure TLineSpacingObj.Set_FY(i: Integer; Value: Double);
begin
    if (i > 0) and (i <= FNConds) then
        FY^[i] := Value;
end;

procedure TLineSpacingObj.Set_FUnits(Value: Integer);
begin
    FUnits := Value;
end;

{$ENDIF}

procedure TLineSpacingObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '3';
    PropertyValue[2] := '3';
    PropertyValue[3] := '0';
    PropertyValue[4] := '32';
    PropertyValue[5] := 'ft';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TLineSpacingObj.set_NWires(const Value: Integer);
begin
    FNconds := Value;
    FX := Allocmem(Sizeof(FX^[1]) * FNconds);
    FY := Allocmem(Sizeof(FY^[1]) * FNconds);
    FUnits := UNITS_FT;
end;

end.
