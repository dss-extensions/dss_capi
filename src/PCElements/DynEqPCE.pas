unit DynEqPCE;

// DynEqPCE is an abstract class for grouping data used in components that 
// use DynamicExp. In the upstream OpenDSS, this is included in the base 
// PCElement, but it feels better to create a dedicated class, even in the
// restrictive environment of the Object Pascal language.

interface

uses
    DSSClass,
    PCClass,
    PCElement,
    DynamicExp,
    ParserDel;

type
    TDynSlot = array [0..1] of Double;

    TDynEqPCEClass = class(TPCClass)
    PUBLIC
        constructor Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
        destructor Destroy; OVERRIDE;
    end;

    TDynEqPCE = class(TPCElement)
    PUBLIC
        DynamicEqObj: TDynamicExpObj; // Reference to the local Dynamic equation (if any)
        DynamicEqVals: TDynSlotArray; // Memory space for the variable values in time
        DynOut: Array of Integer; // Memory space for referencing the output values
        DynamicEqPair: Array of Integer; // Memory space for assigning calculated values to vars

        constructor Create(ParClass: TDSSClass);
        destructor Destroy; OVERRIDE;
        //TODO? procedure MakeLike(OtherObj: Pointer); override;

        function ParseDynVar(Parser: TDSSParser; variable: String): Boolean; OVERRIDE; // was CheckIfDynVar upstream
        procedure SetDynOutput(variable: String);
        function GetDynOutputStr(): String;

        function NumVariables: Integer; OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    Classes,
    SysUtils,
    TypInfo;


constructor TDynEqPCEClass.Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
begin
    inherited Create(dssContext, DSSClsType, DSSClsName);
end;

destructor TDynEqPCEClass.Destroy;
begin
    inherited Destroy;
end;

constructor TDynEqPCE.Create(ParClass: TDSSClass);
begin
    inherited Create(ParClass);
    DynamicEqObj := NIL;
end;

destructor TDynEqPCE.Destroy;
begin
    inherited Destroy;
end;

function TDynEqPCE.ParseDynVar(Parser: TDSSParser; variable: String): Boolean;
// Evaluates if the value provided corresponds to a constant value or to an operand
// for calculating the value using the simulation results
var
    op: Integer; // Operator found
    varValue: String; // Value entered by the user
    varIdx: Integer;
begin
    Result := False;
    if DynamicEqObj = NIL then
        Exit;
    varIdx := DynamicEqObj.Get_Var_Idx(variable);
    if (varIdx < 0) or (varIdx >= 50000) then
        Exit;

    varValue := Parser.StrValue;
    if (DynamicEqObj.Check_If_CalcValue(varValue, op)) then
    begin
        // Adds the pair (var index + operand index)
        SetLength(DynamicEqPair, Length(DynamicEqPair) + 2);
        DynamicEqPair[High(DynamicEqPair) - 1] := varIdx;
        DynamicEqPair[High(DynamicEqPair)] := op;
    end
    else 
        // Otherwise, move the value to the values array
        DynamicEqVals[varIdx][0] := Parser.DblValue;

    Result := True;
end;

function TDynEqPCE.GetDynOutputStr(): String;
// Returns the names of the variables to be used as outputs for the dynamic expression
var
    idx: Integer;
begin
    if DynamicEqObj = NIL then
    begin
        Result := '[]';
        Exit;
    end;

    Result := '[';
    for idx := 0 to High(DynOut) do
        Result := Result + DynamicEqObj.Get_VarName(DynOut[idx]) + ',';
    Result := Result + ']';
end;

procedure TDynEqPCE.SetDynOutput(variable: String);
// Obtains the indexes of the given variables to use them as reference for setting
// the dynamic output for the generator
var
    VarIdx,
    idx: Integer;
    strList: TStringList = NIL;
begin
    if DynamicEqObj = NIL then
    begin
        // Making sure we have a dynamic eq linked
        DoSimpleMsg('A DynamicExp object needs to be assigned to this element before this declaration: DynOut = [%s]', [variable], 50007);
        Exit;
    end;

    // First, set the Length for the index array, 2 variables in this case
    SetLength(DynOut, 2);
    strList := TStringList.Create;
    InterpretTStringListArray(DSS, variable, strList);
    // ensuring they are lower case
    for idx := 0 to (strList.Count - 1) do
    begin
        strList[idx] := AnsiLowerCase(strList[idx]);
        VarIdx := DynamicEqObj.Get_Out_Idx(strList[idx]);
        if (VarIdx < 0) then
            // Being here means that the given name doesn't exist or is a constant
            DoSimpleMsg('DynamicExp variable "%s" not found or not defined as an output.', [strList[idx]], 50008)
        else
            DynOut[idx] := VarIdx;
    end;
    strList.Free;
end;

function TDynEqPCE.NumVariables: Integer;
begin
    Result := 0;
    if DynamicEqObj = NIL then
        Exit;

    Result := DynamicEqObj.NVariables * Length(DynamicEqVals[0]);
end;

function TDynEqPCE.VariableName(i: Integer): String;
begin
    if (DynamicEqObj = NIL) or (i < 0) or (i > (DynamicEqObj.NVariables * Length(DynamicEqVals[0]))) then
    begin
        Result := '';
        Exit;
    end;
    
    Result := DynamicEqObj.Get_VarName(i - 1);
end;

end.