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
    ParserDel,
    Classes,
    fpjson;

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
        UserDynInit: TJSONObject;

        constructor Create(ParClass: TDSSClass);
        destructor Destroy; OVERRIDE;
        //TODO? procedure MakeLike(OtherObj: Pointer); override;
        procedure SaveWrite(F: TStream); override; // override to add user-defined dyn init expressions

        function ParseDynVar(Parser: TDSSParser; variable: String): Boolean; OVERRIDE; // was CheckIfDynVar upstream
        procedure SetDynOutputNames(variables: TStringList);
        function GetDynOutputNames(): TStringList;
        function SetDynVars(Parser: TDSSParser; dynInit: TJSONObject): Boolean;

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
    UserDynInit := NIL;
end;

destructor TDynEqPCE.Destroy;
begin
    inherited Destroy;
    if UserDynInit <> NIL then
        UserDynInit.Free;
end;

function TDynEqPCE.SetDynVars(Parser: TDSSParser; dynInit: TJSONObject): Boolean;
// Evaluates if the value provided corresponds to a constant value or to an operand
// for calculating the value using the simulation results
var
    op: Integer; // Operator found
    variable, varValueStr: String; // Value entered by the user
    varValue: TJSONData;
    varIdx: Integer;
    i: Integer;
begin
    Result := False;
    if DynamicEqObj = NIL then
        Exit;

    if UserDynInit <> NIL then
        UserDynInit.Free();
    UserDynInit := dynInit;

    for i := 0 to UserDynInit.Count - 1 do
    begin
        variable := AnsiLowerCase(UserDynInit.Names[i]);
        varIdx := DynamicEqObj.Get_Var_Idx(variable);
        if (varIdx < 0) or (varIdx >= 50000) then
            continue;

        varValue := UserDynInit.Elements[variable];
        if varValue is TJSONNumber then
        begin
            DynamicEqVals[varIdx][0] := varValue.AsFloat;
            continue;
        end;

        varValueStr := varValue.AsString;
        
        if (DynamicEqObj.Check_If_CalcValue(varValueStr, op)) then
        begin
            // Adds the pair (var index + operand index)
            SetLength(DynamicEqPair, Length(DynamicEqPair) + 2);
            DynamicEqPair[High(DynamicEqPair) - 1] := varIdx;
            DynamicEqPair[High(DynamicEqPair)] := op;
        end
        else 
        begin
            // Otherwise, move the value to the values array
            Parser.CmdString := '[' + varValueStr + ']';
            Parser.NextParam();
            DynamicEqVals[varIdx][0] := Parser.DblValue;
        end;
    end;
    Result := True;
end;

function TDynEqPCE.ParseDynVar(Parser: TDSSParser; variable: String): Boolean;
// Evaluates if the value provided corresponds to a constant value or to an operand
// for calculating the value using the simulation results
var
    op: Integer; // Operator found
    varValue: String; // Value entered by the user
    varIdx: Integer;
    varDbl: Double;
    requiredRPN: Boolean = false;
begin
    variable := AnsiLowerCase(variable);
    Result := False;
    if DynamicEqObj = NIL then
        Exit;
    varIdx := DynamicEqObj.Get_Var_Idx(variable);
    if (varIdx < 0) or (varIdx >= 50000) then
        Exit;

    if UserDynInit = NIL then
        UserDynInit := TJSONObject.Create();

    varValue := Parser.StrValue;

    UserDynInit.Delete(variable);
    if (DynamicEqObj.Check_If_CalcValue(varValue, op)) then
    begin
        // Adds the pair (var index + operand index)
        SetLength(DynamicEqPair, Length(DynamicEqPair) + 2);
        DynamicEqPair[High(DynamicEqPair) - 1] := varIdx;
        DynamicEqPair[High(DynamicEqPair)] := op;
        UserDynInit.Add(variable, varValue);
    end
    else 
    begin
        // Otherwise, move the value to the values array
        varDbl := Parser.MakeDouble(@requiredRPN);
        DynamicEqVals[varIdx][0] := varDbl;
        if requiredRPN then
            UserDynInit.Add(variable, varValue)
        else
            UserDynInit.Add(variable, varDbl);
    end;
    Result := True;
end;

function TDynEqPCE.GetDynOutputNames(): TStringList;
// Returns the names of the variables to be used as outputs for the dynamic expression
var
    idx: Integer;
begin
    Result := TStringList.Create();
    if DynamicEqObj = NIL then
    begin
        Exit;
    end;

    for idx := 0 to High(DynOut) do
        Result.Add(DynamicEqObj.Get_VarName(DynOut[idx] * 2));
end;

procedure TDynEqPCE.SetDynOutputNames(variables: TStringList);
// Obtains the indexes of the given variables to use them as reference for setting
// the dynamic output for the generator
var
    VarIdx,
    idx: Integer;
    variablesStr: String;
    varStr: String;
begin
    if DynamicEqObj = NIL then
    begin
        // Making sure we have a dynamic eq linked
        variablesStr := '';
        for idx := 0 to variables.Count - 1 do
            variablesStr := variablesStr + variables[idx] + ',';
        DoSimpleMsg('A DynamicExp object needs to be assigned to this element before this declaration: DynOut = [%s]', [variablesStr], 50007);
        Exit;
    end;

    // First, set the Length for the index array, 2 variables in this case
    SetLength(DynOut, 2);
    // ensuring they are lower case
    for idx := 0 to variables.Count - 1 do
    begin
        varStr := AnsiLowerCase(variables[idx]);
        VarIdx := DynamicEqObj.Get_Out_Idx(variables[idx]);
        if (VarIdx < 0) then
            // Being here means that the given name doesn't exist or is a constant
            DoSimpleMsg('DynamicExp variable "%s" not found or not defined as an output.', [varStr], 50008)
        else
            DynOut[idx] := VarIdx;
    end;
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

procedure TDynEqPCE.SaveWrite(F: TStream);
var
    i: Integer;
    variable: String;
    varValue: TJSONData;
begin
    inherited SaveWrite(F);
    if UserDynInit = NIL then
        Exit;

    for i := 0 to UserDynInit.Count - 1 do
    begin
        variable := UserDynInit.Names[i];
        FSWrite(F, ' ' + variable);
        varValue := UserDynInit.Elements[variable];
        if varValue is TJSONNumber then
            FSWrite(F, '=' + FloatToStr(varValue.AsFloat))
        else
            FSWrite(F, '=' + varValue.AsString);
    end;
end;

end.