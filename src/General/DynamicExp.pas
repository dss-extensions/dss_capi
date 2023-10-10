unit DynamicExp;
// ----------------------------------------------------------
// Copyright (c) 2022-2023, Paulo Meira, DSS-Extensions contributors
// Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------
interface

uses
    Command,
    DSSClass,
    DSSObject,
    UcMatrix,
    Arraydef,
    Classes;

const
    DYN_SLOT_LENGTH = 2;
type
{$SCOPEDENUMS ON}
    TDynamicExpProp = (
        INVALID = 0,
        NVariables = 1,
        VarNames = 2,
        vr = 3, // var
        VarIdx = 4,
        Expression = 5,
        Domain = 6
    );
    TDynamicExpPropLegacy = TDynamicExpProp;
{$PUSH}
{$Z4} // keep enums as int32 values
    TDynDomain = (
        Time = 0,
        dq = 1
    );
{$POP}
{$SCOPEDENUMS OFF}

    TDynSlot = Array [0..(DYN_SLOT_LENGTH - 1)] of Double; // dynamic memory slot, just 1 (z-1) slots for now
    TDynSlotArray = Array of TDynSlot;

    TDynamicExp = class(TDSSClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function Find(const ObjName: String; const ChangeActive: Boolean=True): Pointer; OVERRIDE;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TDynamicExpObj = class(TDSSObject)
    PUBLIC
        VarIdx: Integer;
        VarNames: TStringList; // List containing the state variable names
        VarConsts: Array of Double; // Array containing the numeric constants of the expression
        Cmds: Array of Integer; // Sequence of commands that implement the expression
        ActiveVar: String; // Name of the active variable
        Expression: String; // Differential equation in RPN format
        NVariables: Integer; // Number of state variables
        Domain: TDynDomain;

        constructor Create(ParClass: TDSSClass; const ObjName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); OVERRIDE;
        procedure MakeLike(OtherPtr: Pointer); OVERRIDE;

        procedure InterpretDiffEq();
        function Get_Closer_Op(expr: String; var opCode: String; var opNum: Integer): Integer;
        function Get_Var_Idx(varName: String): Integer;
        function Check_If_CalcValue(valueStr: String; var opNum: Integer): Boolean;
        function Get_Out_Idx(varName: String): Integer;  // gets the index for the given variable if it is an output
        procedure SolveEq(var MemSpace: TDynSlotArray); // Equation solver
        function IsInitVal(code: Integer): Boolean; // returns true if the given code is for an initialization value
        function Get_DynamicEqVal(idx: Integer; var MemSpace: TDynSlotArray): Double;
        function Get_VarName(idx: Integer): String;
    end;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    SysUtils,
    Ucomplex,
    Utilities,
    LineUnits,
    math,
    RPN,
    DSSHelper;

type
    TObj = TDynamicExpObj;
    TProp = TDynamicExpProp;
    TPropLegacy = TDynamicExpPropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
    opCodes: array [0..28] of String = (
        'dt', '=', '+', '-', '*', '/', '(', ')', ';', '[', ']',
        'sqr', 'sqrt', 'inv', 'ln', 'exp', 'log10', 'sin', 'cos',
        'tan', 'asin', 'acos', 'atan', 'atan2', 'rollup', 'rolldn',
        'swap', 'pi', '^'
    );
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;
    DomainEnum: TDSSEnum;

constructor TDynamicExp.Create;
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
        DomainEnum := TDSSEnum.Create('DynamicExp: Domain', True, 1, 1, 
            ['Time', 'dq'], [Ord(TDynDomain.Time), Ord(TDynDomain.dq)]
        );
        DomainEnum.DefaultValue := Ord(TDynDomain.dq); // default when parsing, per upstream code
    end;

    inherited Create(dssContext, DSS_OBJECT, 'DynamicExp');
end;

destructor TDynamicExp.Destroy;
begin
    inherited Destroy;
end;

procedure TDynamicExp.DefineProperties;
var
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    // integer properties
    PropertyType[ord(TProp.NVariables)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.NVariables)] := ptruint(@obj.NVariables);

    PropertyType[ord(TProp.VarIdx)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.VarIdx)] := ptruint(@obj.VarIdx);
    PropertyFlags[ord(TProp.VarIdx)] := [TPropertyFlag.SilentReadOnly];

    // enums
    PropertyType[ord(TProp.Domain)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Domain)] := ptruint(@obj.Domain);
    PropertyOffset2[ord(TProp.Domain)] := PtrInt(DomainEnum);

    // strings
    PropertyType[ord(TProp.Expression)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.Expression)] := ptruint(@obj.Expression);
    PropertyFlags[ord(TProp.Expression)] := [TPropertyFlag.Required];

    PropertyType[ord(TProp.vr)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.vr)] := ptruint(@obj.ActiveVar);
    PropertyFlags[ord(TProp.vr)] := [TPropertyFlag.Transform_LowerCase];

    // string lists
    PropertyType[ord(TProp.VarNames)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.VarNames)] := ptruint(@obj.VarNames);
    PropertyFlags[ord(TProp.VarNames)] := [TPropertyFlag.Transform_LowerCase];

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

procedure TDynamicExpObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    if (Idx > 0) and (Idx <= NumPropsThisClass) then
        case Idx of
            // ord(TProp.NVariables): Not really used, ignore.
            ord(TProp.Expression):
                InterpretDiffEq();
            ord(TProp.vr):
            begin
                VarIdx := VarNames.IndexOf(ActiveVar);
                if VarIdx < 0 then
                begin
                    // Being here means that the given name doesn't exist
                    DoSimpleMsg('DynamicExp variable "%s" not found.', [ActiveVar], 50001);
                    ActiveVar := '';
                end;
            end;
        end;

    inherited PropertySideEffects(Idx, previousIntVal);        
end;

function TDynamicExp.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

function TDynamicExp.Find(const ObjName: String; const ChangeActive: Boolean): Pointer;
begin
    if (Length(ObjName) = 0) or (CompareText(ObjName, 'none') = 0) then
        Result := NIL
    else
        Result := inherited Find(ObjName, ChangeActive);
end;

constructor TDynamicExpObj.Create(ParClass: TDSSClass; const ObjName: String);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(ObjName);
    DSSObjType := ParClass.DSSClassType;
    NVariables := 0;
    ActiveVar := '';
    VarIdx := -1;
    VarNames := TStringList.Create();
    Domain := TDynDomain.Time;
end;

function TDynamicExpObj.Check_If_CalcValue(valueStr: String; var opNum: Integer): Boolean;
// Checks if the given string is a value calculated by the element using the eq model
var
    found: Boolean;
    Val: String;
    idx: Integer;
const
    ValNames: array [0..11] of String =
        ('p', 'q', 'vmag', 'vang', 'imag', 'iang', 's', 'p0', 'q0', 'edp', 'kvdc', 'mod');
begin
    opNum := -1;
    found := FALSE;
    //NOTE: there is a function for this (AnsiIndexText), but we'll leave as-is for upstream compat.
    Val := AnsiLowerCase(valueStr);
    for idx := 0 to High(ValNames) do
        if Val = ValNames[idx] then
        begin
            opNum := idx;
            Found := TRUE;
            break;
        end;

    Result := found;
end;

destructor TDynamicExpObj.Destroy;
begin
    FreeAndNil(VarNames);
    inherited destroy;
end;

function TDynamicExpObj.Get_Closer_Op(expr: String; var opCode: String; var opNum: Integer): Integer;
var
    SetMark: Boolean;  // For validating special chars
    opPos: Integer;
    idx: Integer;
begin
    Result := 10000;
    for idx := 0 to High(opCodes) do
    begin
        opPos := Pos(opCodes[idx], expr);
        if (opPos >= Result) or (opPos <= 0) then
            continue;

        SetMark := TRUE;
        if opCodes[idx] = '-' then    // Verify in case it is a negative number
            if expr[opPos + 1] <> ' ' then
                SetMark := FALSE;

        if not SetMark then
            continue;

        Result := opPos;
        opCode := opCodes[idx];
        opNum := idx;
    end;
end;

function TDynamicExpObj.Get_Var_Idx(varName: String): Integer;
// Returns the index of the variable if it exists in the state variable list,
// otherwise, it returns 50001 if the string entered is a numeric constant (dbl)
// or -1 if the string entered is neither a numeric constant or state varaible
var
    // dblval: Double;
    idx: Integer;
begin
    // dblval := 0.0;
    Result := -1;   // error
    for idx := 0 to (VarNames.Count - 1) do
        if AnsiLowercase(varName) = VarNames[idx] then
        begin
            Result := idx;
            break;
        end;

    if Result < 0 then
    begin
      // so, it's not a state variable, maybe a constant
        try
            // dblval :=
            strtofloat(varName);
            Result := 50001;  // returns this code to indicate that it is a constant
        except
            Result := -1;  // it's not a number
        end;
    end;
end;

function TDynamicExpObj.Get_Out_Idx(varName: String): Integer;
// Returns the index of the variable if it exists in the state variable list,
// and if it is an output (-50 in the next cell ot the Cmds automation array)
var
    CmdIdx: Integer;
    idx: Integer;
begin
    Result := -1; // error
    for idx := 0 to (VarNames.Count - 1) do
        if AnsiLowercase(varName) = VarNames[idx] then
        begin
            // now, check if the index corresponds to an output
            for CmdIdx := 0 to High(Cmds) do
            begin
                if (idx = Cmds[CmdIdx]) and (CmdIdx < High(Cmds)) and (Cmds[CmdIdx + 1] = -50) then
                begin
                    // Means that the variable found is an output, we can leave
                    Result := idx;
                    break;
                end;
            end;
        end;
end;

function TDynamicExpObj.Get_VarName(idx: Integer): String;
var
    diffstr: String;
    len, col, row: Integer;
begin
    len := DYN_SLOT_LENGTH;
    row := idx div len;
    col := idx - (row * len);
    diffstr := '';
    if col > 0 then
    begin
        diffstr := 'd';
        if col > 1 then
            diffstr := diffstr + inttostr(col);
    end;
    Result := diffstr + VarNames[row];
end;

function TDynamicExpObj.Get_DynamicEqVal(idx: Integer; var MemSpace: TDynSlotArray): Double;
var
    len, col, row: Integer;
begin
    len := DYN_SLOT_LENGTH;
    row := idx div len;
    col := idx - (row * len);
    Result := MemSpace[row][col];
end;

function TDynamicExpObj.IsInitVal(code: Integer): Boolean;
begin
    Result := FALSE;
    case code of
        7, 8, 9:
            Result := TRUE;
    end;
end;

procedure TDynamicExpObj.SolveEq(var MemSpace: TDynSlotArray);
var
    OutIdx,
    idx: Integer;
    RPN: TRPNCalc;
begin
    RPN := TRPNCalc.Create();
    OutIdx := -1;
    for idx := 0 to High(Cmds) do
    begin
        if (Cmds[idx + 1] = -50) or (Cmds[idx] = -50) then // it's the begining of an equation
        begin
            if (Cmds[idx] <> -50) then // The index
            begin
                if OutIdx >= 0 then // It's not the first equation
                    MemSpace[OutIdx][1] := RPN.X; // Uploads value into memory space
                OutIdx := Cmds[idx];
            end;
        end
        else
        case Cmds[idx] of
            -2: //Add
                RPN.Add;
            -3: //Sub
                RPN.Subtract;
            -4: //Mult
                RPN.Multiply;
            -5: //Div
                RPN.Divide;
            -11: //Sqr
                RPN.Square;
            -12: //Sqrt
                RPN.Sqrt;
            -13: //Inv
                RPN.Inv;
            -14: //ln
                RPN.NatLog;
            -15: //exp
                RPN.etothex;
            -16: //log10
                RPN.TenLog;
            -17: //Sin
                RPN.Sindeg;
            -18: //Cos
                RPN.Cosdeg;
            -19: //Tan
                RPN.Tandeg;
            -20: //ASin
                RPN.aSindeg;
            -21: //ACos
                RPN.aCosdeg;
            -22: //ATan
                RPN.aTandeg;
            -23: //ATan2
                RPN.aTan2deg;
            -24: //RollUp
                RPN.RollUp;
            -25: //RollDn
                RPN.RollDn;
            -26: //Swap
                RPN.SwapXY;
            -27: //Pi
                RPN.EnterPi;
            -28: //^
                RPN.YToTheXPower;
        else
            begin
                if Cmds[idx] >= 50000 then
                    RPN.X := VarConsts[Cmds[idx] - 50000]  // It's a constant
                else
                    RPN.X := MemSpace[Cmds[idx]][0];       // It's a variable
            end;
        end;
    end;
    MemSpace[OutIdx][1] := RPN.X; // Uploads value into memory space
    RPN.Free(); // Destroy RPN calculator
end;

procedure TDynamicExpObj.InterpretDiffEq();
// Builds the list of commands required for operating the equation declared, this
// automation is intended to acelerate the calculation in run time.
// Notation:
//   Positive integers represent the index to a variable slot (dbl)
//   If the integer is a value >= 50000, it means that it is the index to a
//   numeric constant that can be located at VarConsts
//   If is a negative integer, represents one of the following operations:
//     -2 = Add
//     -3 = Subtraction
//     -4 = Mult
//     -5 = Div .... etc. For details, check opCodes array defined above
//   If the negative integer is -50, it means the begining of a new equation
var
    Idx,
    OpCode,
    OpIdx: Integer;
    ErrorSrc,
    SubXp,
    Op: String;
    vars: TStringList;
    Expr: String;
    Result: Boolean = FALSE;
begin
    ErrorSrc := '';
    vars := TStringList.Create;
    vars.Clear;
    SetLength(Cmds, 0);
    Expr := '[' + AnsiLowercase(Expression) + ']';
    try
        while Expr.Length > 0 do
        begin
            OpIdx := Get_Closer_Op(Expr, Op, OpCode);

            if OpIdx = 10000 then
            begin
                Expr := ''   // we're done
            end
            else
            begin
                SubXp := Expr.Substring(0, OpIdx - 1);
                if Op.Length > 1 then
                    OpIdx := OpIdx + Op.Length;
                Expr := Expr.Substring(OpIdx, Expr.Length);
                InterpretTStringListArray(DSS, SubXp, vars);
                case OpCode of
                    0:
                    begin
                        SetLength(Cmds, length(Cmds) + 2);
                        OpIdx := Get_Var_Idx(vars[0]);     // the result is always placed at the begin
                        if OpIdx = 50001 then
                        begin
                            DoSimpleMsg('DynamicExp: the expression preceeding the "dt" operand has to be a state variable.', 50006);
                            ErrorSrc := 'preceeding differential output';
                            Exit;
                        end
                        else
                        begin
                            if OpIdx < 0 then
                                ErrorSrc := vars[0]
                            else
                            begin
                                Cmds[High(Cmds) - 1] := OpIdx;
                                Cmds[High(Cmds)] := -50;   // denotes the begining of an equation
                            end;
                        end;
                    end;
                    1, 6, 7, 8, 9:
                    begin
                        // Do nothing, it's just for notation reference at the user side
                    end;
                else   // it is one of the basic operations or end of the diff eq
                begin
                    for Idx := 0 to (vars.Count - 1) do
                    begin
                        SetLength(Cmds, Length(Cmds) + 1);
                        OpIdx := Get_Var_Idx(vars[idx]);
                        if OpIdx = 50001 then
                        begin
                            SetLength(VarConsts, Length(VarConsts) + 1);
                            VarConsts[High(VarConsts)] := strtofloat(vars[idx]);
                            Cmds[High(Cmds)] := 50000 + High(VarConsts);
                        end
                        else
                        begin
                            if OpIdx < 0 then
                                ErrorSrc := '"' + vars[0] + '"'
                            else
                                Cmds[High(Cmds)] := Get_Var_Idx(vars[idx]);
                        end;
                    end;
                    if OpCode <> 10 then
                    begin
                        SetLength(Cmds, Length(Cmds) + 1);
                        Cmds[High(Cmds)] := -1 * OpCode;    // assings the operator -> + - * /
                    end;
                end;

                end;
            end;
            if ErrorSrc <> '' then
            begin
                DoSimpleMsg('DynamicExp: Variable "%s" not Found.', [ErrorSrc], 50005);
                Expr := '';
                Result := TRUE;
            end;
        end;

        if not Result then
            Expression := Expr
        else
            DoSimpleMsg('There are errors in the differential equation.', 50003);
    finally
        vars.Free();
    end;
end;

procedure TDynamicExpObj.MakeLike(OtherPtr: Pointer);
// var
//     Other: TObj;
begin
    DoSimpleMsg('"Like" is not implemented for DynamicExp.', 50099);
    // inherited MakeLike(OtherPtr);
    // Other := TObj(OtherPtr);
end;


finalization
    DomainEnum.Free;    
end.
