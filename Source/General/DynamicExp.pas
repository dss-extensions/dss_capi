unit DynamicExp;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$IFDEF FPC}{$MODE Delphi}{$ENDIF}
interface

{The dynamics experssion object implements differential equations
and state variables that can be called by other objects to represent
their dynamic behavior when operating in dynamics simulation mode.

Last update by Davis Montenegro 04/13/2022
}

uses
    Command,
    DSSClass,
    DSSObject,
    UcMatrix,
    Arraydef,
    Classes;

type

    TDynamicExp = class(TDSSClass)
    PRIVATE
        SymComponentsChanged: Boolean;
        MatrixChanged: Boolean;

        function Get_Code: String;  // Returns active line code string
        procedure Set_Code(const Value: String);  // sets the  active linecode

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const LineName: String): Integer; OVERRIDE;
    PUBLIC

        constructor Create;
        destructor Destroy; OVERRIDE;

        function Find(const ObjName: String): Pointer; OVERRIDE;
        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

       // Set this property to point ActiveLineCodeObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;

    end;

    TDynamicExpObj = class(TDSSObject)
    PRIVATE
        FVarIdx,
        FNumvars: Integer;              // Number of state variables
        FvarNames: TStringList;          // List containing the state variable names
        FVarConst: array of Double;      // Array containing the numeric constants of the expression
        FCmd: array of Integer;     // Sequence of commands that implement the expression
        FActiveVar,                                 // Name of the active variable
        FXpression: String;               // Differential equiation in RPN format

    PUBLIC
        BaseFrequency: Double;
        myDomain: Integer;
        constructor Create(ParClass: TDSSClass; const LineCodeName: String);
        destructor Destroy; OVERRIDE;

        function InterpretDiffEq(Exp: String): Boolean;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function Get_Closer_Op(myExp: String; var myOp: String; var OpCode: Integer): Integer;
        function Get_Var_Idx(myVar: String): Integer;
        function Check_If_CalcValue(myVal: String; var myOp: Integer): Boolean;
        function Get_Out_Idx(myVar: String): Integer;           // gets the index for the given variable if it is an output
        procedure SolveEq(var MemSpace: array of DynSlot);       // Equation solver
        function IsInitVal(myCode: Integer): Boolean;            // returns true if the given code is for an initial value
        function Get_DynamicEqVal(myIdx: Integer; var MemSpace: array of DynSlot): Double;
        function Get_VarName(myIdx: Integer): String;

        property NumVars: Integer READ FNumVars WRITE FNumVars;


    end;

const
    myOps: array [0..28] of String =
        ('dt', '=', '+', '-', '*', '/', '(', ')', ';', '[', ']',
        'sqr', 'sqrt', 'inv', 'ln', 'exp', 'log10', 'sin', 'cos',
        'tan', 'asin', 'acos', 'atan', 'atan2', 'rollup', 'rolldn',
        'swap', 'pi', '^');

var
    DynamicExpClass: TDynamicExp;
    ActiveDynamicExpObj: TDynamicExpObj;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Ucomplex,
    Utilities,
    LineUnits,
    math,
    RPN;

const
    NumPropsThisClass = 6;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TDynamicExp.Create;  // Creates superstructure for all DynamicExp objects
begin
    inherited Create;
    Class_Name := 'DynamicExp';
    DSSClassType := DSS_OBJECT;
    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(PropertyName, NumProperties);
    CommandList.Abbrev := TRUE;

    DynamicExpClass := Self;
end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TDynamicExp.Destroy;

begin
      // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDynamicExp.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


    PropertyName^[1] := 'nvariables';
    PropertyName^[2] := 'varnames';
    PropertyName^[3] := 'var';
    PropertyName^[4] := 'varidx';
    PropertyName^[5] := 'expression';
    PropertyName^[6] := 'Domain';

    PropertyHelp^[1] := '(Int) Number of state variables to be considered in the differential equation.';
    PropertyHelp^[2] := '([String]) Array of strings with the names of the state variables.';
    PropertyHelp^[3] := '(String) Activates the state variable using the given name.';
    PropertyHelp^[4] := '(Int) read-only, returns the index of the active state variable.';
    PropertyHelp^[5] := 'It is the differential expression using OpenDSS RPN syntax. The expression must be contained within brackets in case of having multiple equations, for example:' + CRLF + CRLF +
        'expression = "[w dt = 1 M / (P_m D*w - P_e -) *]"';
    PropertyHelp^[6] := 'It is the domain for which the equation is defined, it can be one of [time*, dq]. By deafult, dynamic epxressions are defined in the time domain.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDynamicExp.NewObject(const ObjName: String): Integer;
begin
     // create a new object of this class and add to list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveDSSObject[ActiveActor] := TDynamicExpObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function TDynamicExp.Find(const ObjName: String): Pointer;
begin
    if (Length(ObjName) = 0) or (CompareText(ObjName, 'none') = 0) then
        Result := NIL
    else
        Result := inherited Find(ObjName);
end;


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDynamicExp.Edit(ActorID: Integer): Integer;
var
    idx,
    ParamPointer: Integer;
    ParamName,
    Param: String;

begin
    Result := 0;
    // continue parsing with contents of Parser
    ActiveDynamicExpObj := ElementList.Active;
    ActiveDSSObject[ActorID] := ActiveDynamicExpObj;
    SymComponentsChanged := FALSE;
    MatrixChanged := FALSE;

    with ActiveDynamicExpObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 50001);
                1:
                begin
                    FNumVars := Parser[ActorID].IntValue;  // Use property value to force reallocations
                end;
                2:
                begin
                    InterpretTStringListArray(Param, FVarNames);
                  // ensuring they are lower case
                    for idx := 0 to (FVarNames.Count - 1) do
                        FVarNames[idx] := LowerCase(FVarNames[idx]);
                end;
                3:
                begin
                    FActiveVar := LowerCase(Parser[ActorID].StrValue);
                    FVarIdx := FVarNames.IndexOf(FActiveVar);
                    if FVarIdx < 0 then
                    begin
                    // Being here means that the given name doesn't exist
                        DoSimpleMsg('DynamicExp "' + FActiveVar + '" not found.', 50001);
                        FActiveVar := '';
                    end;
                end;
                5:
                begin
                    if (InterpretDiffEq(Parser[ActorID].StrValue)) then
                        DoSimpleMsg('There are errors in the differential equation.', 50003);
                end;
                6:
                begin
                    if Parser[ActorID].StrValue = 'time' then
                        myDomain := 0
                    else
                        myDomain := 1;
                end
            else
                ClassEdit(ActiveDynamicExpObj, Parampointer - NumPropsThisClass)
            end;
{
           CASE ParamPointer OF
               1 : ;
           END;

}
            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

    end;

end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDynamicExp.MakeLike(const LineName: String): Integer;
  //VAR
  //   OtherDynExpCode:TDynamicExpObj;
begin
    Result := 0;
     {See if we can find this DynamicExp code in the present collection}


end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDynamicExp.Init(Handle: Integer; ActorID: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TDynamicExp.Init', -1);
    REsult := 0;
end;

function TDynamicExp.Get_Code: String;  // Returns active line code string

begin

    Result := '';

end;

procedure TDynamicExp.Set_Code(const Value: String);  // sets the  active linecode
var
    DynExpCodeObj: TDynamicExpObj;
begin

    ActiveDynamicExpObj := NIL;
    DynExpCodeObj := ElementList.First;
    while DynExpCodeObj <> NIL do
    begin

        if CompareText(DynExpCodeObj.Name, Value) = 0 then
        begin
            ActiveDynamicExpObj := DynExpCodeObj;
            Exit;
        end;

        DynExpCodeObj := ElementList.Next;
    end;

    DoSimpleMsg('DynamicExp: "' + Value + '" not Found.', 50004);

end;


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  //      TDynamicExp Obj
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TDynamicExpObj.Create(ParClass: TDSSClass; const LineCodeName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(LineCodeName);
    DSSObjType := ParClass.DSSClassType;
    FVarNames := NIL;
    FNumVars := 20;
    FActiveVar := '';
    FVarIdx := -1;
    FVarNames := TStringList.create;
    FVarNames.Clear;
    setlength(FCmd, 0);
    setlength(FVarConst, 0);
    myDomain := 0;

    InitPropertyValues(0);
end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  {Checks if the given string is a value calculated by the element using the eq model}
function TDynamicExpObj.Check_If_CalcValue(myVal: String; var myOp: Integer): Boolean;
var
    found: Boolean;
    Val: String;
    idx: Integer;
const
    ValNames: array [0..11] of String =
        ('p', 'q', 'vmag', 'vang', 'imag', 'iang', 's', 'p0', 'q0', 'edp', 'kvdc', 'mod');

begin
    myOp := -1;
    found := FALSE;
    Val := LowerCase(myVal);
    for idx := 0 to High(ValNames) do
    begin
        if Val = ValNames[idx] then
        begin
            myOp := idx;
            Found := TRUE;
            break;
        end;
    end;

    Result := found;
end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TDynamicExpObj.Destroy;
begin
    FVarNames.Clear;
    setlength(FVarConst, 0);
    inherited destroy;
end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function TDynamicExpObj.Get_Closer_Op(myExp: String; var myOp: String; var OpCode: Integer): Integer;
var
    SetMark: Boolean;  // For validating special chars
    myPos,
    idx: Integer;
begin
    Result := 10000;
    for idx := 0 to High(myOps) do
    begin
        myPos := Pos(myOps[idx], myExp);
        if (myPos < Result) and (myPos > 0) then
        begin
            SetMark := TRUE;
            if myOps[idx] = '-' then    // Verify in case it is a negative number
                if myExp[myPos + 1] <> ' ' then
                    SetMark := FALSE;
            if SetMark then
            begin
                Result := myPos;
                myOp := myOps[idx];
                OpCode := idx;
            end;
        end;
    end;
end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  {returns the index of the variable if it exists in the state variable list,
  otherwise, it returns 50001 if the string entered is a numeric constant (dbl)
  or -1 if the string entered is neither a numeric constant or state varaible}
function TDynamicExpObj.Get_Var_Idx(myVar: String): Integer;
var
    idx: Integer;
begin

    Result := -1;   // error
    for idx := 0 to (FVarNames.Count - 1) do
        if Lowercase(myVar) = FVarNames[idx] then
        begin
            Result := idx;
            break;
        end;

    if Result < 0 then
    begin
      // so, it's not a state variable, maybe a constant
        try
            Result := 50001;  // returns this code to indicate that it is a constant
        except
            Result := -1;  // it's not a number
        end;
    end;

end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function TDynamicExpObj.Get_Out_Idx(myVar: String): Integer;
  {returns the index of the variable if it exists in the state variable list,
  and if it is an output (-50 in the next cell ot the FCmd automation array)}
var
    CmdIdx,
    idx: Integer;
begin

    Result := -1;   // error
    for idx := 0 to (FVarNames.Count - 1) do
        if Lowercase(myVar) = FVarNames[idx] then
        begin
        // now, check if the index corresponds to an output
            for CmdIdx := 0 to High(FCmd) do
            begin
                if (idx = FCmd[CmdIdx]) and (CmdIdx < High(FCmd)) then
                    if FCmd[CmdIdx + 1] = -50 then
                    begin
              // Means that the variable found is an output, we can leave
                        Result := idx;
                        break;
                    end;
            end;

        end;

end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function TDynamicExpObj.Get_VarName(myIdx: Integer): String;
var
    diffstr: String;
    myProt: DynSlot;
    mylen,
    myCol,
    myRow: Integer;
begin
    mylen := length(myProt);
{$IFDEF FPC}
    myRow := myIdx div mylen;
{$ELSE}
    myRow := floor(Double(myIdx) / Double(mylen));
{$ENDIF}
    myCol := myIdx - (myRow * mylen);
    diffstr := '';
    if myCol > 0 then
    begin
        diffstr := 'd';
        if myCol > 1 then
            diffstr := diffstr + inttostr(myCol);
    end;

    Result := diffstr + FVarNames[myRow];

end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function TDynamicExpObj.Get_DynamicEqVal(myIdx: Integer; var MemSpace: array of DynSlot): Double;
var
    mylen,
    myCol,
    myRow: Integer;
begin
    mylen := length(MemSpace[0]);
{$IFDEF FPC}
    myRow := myIdx div mylen;
{$ELSE}
    myRow := floor(Double(myIdx) / Double(mylen));
{$ENDIF}
    myCol := myIdx - (myRow * mylen);
    Result := MemSpace[myRow][myCol];
end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function TDynamicExpObj.IsInitVal(myCode: Integer): Boolean;
begin
    Result := FALSE;
    case myCode of
        7, 8, 9:
            Result := TRUE;
    end;
end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TDynamicExpObj.SolveEq(var MemSpace: array of DynSlot);
var
    OutIdx,
    idx: Integer;
    RPN: TRPNCalc;
begin

    RPN := TRPNCalc.Create;
    OutIdx := -1;
    for idx := 0 to High(FCmd) do
    begin
        if (FCmd[idx + 1] = -50) or (FCmd[idx] = -50) then    // it's the begining of an equation
        begin
            if (FCmd[idx] <> -50) then                            // The index
            begin
                if OutIdx >= 0 then                                   // It's not the first equation
                    MemSpace[OutIdx][1] := RPN.X;                     // Uploads value into memory space
                OutIdx := FCmd[idx];
            end;
        end
        else
        begin
            case FCmd[idx] of
                -2:
                    RPN.Add;             //Add
                -3:
                    RPN.Subtract;        //Sub
                -4:
                    RPN.Multiply;        //Mult
                -5:
                    RPN.Divide;          //Div
                -11:
                    RPN.Square;          //Sqr
                -12:
                    RPN.Sqrt;            //Sqrt
                -13:
                    RPN.Inv;             //Inv
                -14:
                    RPN.NatLog;          //ln
                -15:
                    RPN.etothex;         //exp
                -16:
                    RPN.TenLog;          //log10
                -17:
                    RPN.Sindeg;          //Sin
                -18:
                    RPN.Cosdeg;          //Cos
                -19:
                    RPN.Tandeg;          //Tan
                -20:
                    RPN.aSindeg;         //ASin
                -21:
                    RPN.aCosdeg;         //ACos
                -22:
                    RPN.aTandeg;         //ATan
                -23:
                    RPN.aTan2deg;        //ATan2
                -24:
                    RPN.RollUp;          //RollUp
                -25:
                    RPN.RollDn;          //RollDn
                -26:
                    RPN.SwapXY;          //Swap
                -27:
                    RPN.EnterPi;         //Pi
                -28:
                    RPN.YToTheXPower;    //^
            else
            begin
                if FCmd[idx] >= 50000 then
                    RPN.X := FVarConst[FCmd[idx] - 50000]  // It's a constant
                else
                    RPN.X := MemSpace[FCmd[idx]][0];       // It's a variable
            end;

            end;

        end;

    end;
    MemSpace[OutIdx][1] := RPN.X;               // Uploads value into memory space
    RPN.Free;                                     // Destroy RPN calculator
end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function TDynamicExpObj.InterpretDiffEq(Exp: String): Boolean;
  {Builds the list of commands required for operating the equation declared, this
  automation is intended to acelerate the calculation in run time.
  Notation:
    Positive integers represent the index to a variable slot (dbl)
    If the integer is a value >= 50000, it means that it is the index to a
    numeric constant that can be located at FVarConst
    If is a negative integer, represents one of the following operations:
      -2 = Add
      -3 = Subtraction
      -4 = Mult
      -5 = Div .... etc. For details, check myOps array defined above
    If the negative integer is -50, it means the begining of a new equation}
var
    Idx,
    OpCode,
    OpIdx: Integer;
    ErrorSrc,
    SubXp,
    Op: String;
    myVars: TStringList;
begin
    Result := FALSE;
    ErrorSrc := '';
    myVars := TStringList.Create;
    myVars.Clear;
    setlength(FCmd, 0);
    FXpression := '[' + Lowercase(Exp) + ']';
    while FXpression.Length > 0 do
    begin
        OpIdx := Get_Closer_Op(FXpression, Op, OpCode);

        if OpIdx = 10000 then
        begin
            FXpression := ''   // we're done
        end
        else
        begin
            SubXp := FXpression.Substring(0, OpIdx - 1);
            if Op.Length > 1 then
                OpIdx := OpIdx + Op.Length;
            FXpression := FXpression.Substring(OpIdx, FXpression.Length);
            InterpretTStringListArray(SubXp, myVars);
            case OpCode of
                0:
                begin
                    setlength(FCmd, length(FCmd) + 2);
                    OpIdx := Get_Var_Idx(myVars[0]);     // the result is always placed at the begin
                    if OpIdx = 50001 then
                    begin
                        DoSimpleMsg('DynamicExp: the expression preceeding the "dt" operand has to be a state variable.', 50006);
                        ErrorSrc := 'preceeding differential output';
                    end
                    else
                    begin
                        if OpIdx < 0 then
                            ErrorSrc := myVars[0]
                        else
                        begin
                            FCmd[High(FCmd) - 1] := OpIdx;
                            FCmd[High(FCmd)] := -50;   // denotes the begining of an equation
                        end;
                    end;
                end;
                1, 6, 7, 8, 9:
                begin
              // Do nothing, it's just for notation reference at the user side
                end;
            else   // it is one of the basic operations or end of the diff eq
            begin
                for Idx := 0 to (myVars.Count - 1) do
                begin
                    setlength(FCmd, Length(FCmd) + 1);
                    OpIdx := Get_Var_Idx(myVars[idx]);
                    if OpIdx = 50001 then
                    begin
                        setlength(FVarConst, Length(FVarConst) + 1);
                        FVarConst[High(FVarConst)] := strtofloat(myVars[idx]);
                        FCmd[High(FCmd)] := 50000 + High(FVarConst);
                    end
                    else
                    begin
                        if OpIdx < 0 then
                            ErrorSrc := '"' + myVars[0] + '"'
                        else
                            FCmd[High(FCmd)] := Get_Var_Idx(myVars[idx]);
                    end;
                end;
                if OpCode <> 10 then
                begin
                    setlength(FCmd, Length(FCmd) + 1);
                    FCmd[High(FCmd)] := -1 * OpCode;    // assings the operator -> + - * /
                end;
            end;

            end;
        end;
        if ErrorSrc <> '' then
        begin
            DoSimpleMsg('DynamicExp: Variable ' + ErrorSrc + ' not Found.', 50005);
            FXpression := '';
            Result := TRUE;
        end;
    end;

    if not Result then
        FXpression := Exp;    // assings the expression again to keep a local copy

end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TDynamicExpObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
    begin

        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

    end;
end;

function TDynamicExpObj.GetPropertyValue(Index: Integer): String;
begin
    case Index of
        1:
            Result := Format('%d', [FNumVars]);
        4:
            Result := Format('%d', [FVarIdx]);

    else
        Result := inherited GetPropertyValue(index);
    end;
end;

procedure TDynamicExpObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '0';      // 'nvariables';
    PropertyValue[2] := '[]';     // 'varnames';
    PropertyValue[3] := '[0.0]';  // 'varinit';
    PropertyValue[4] := '';       // 'var';
    PropertyValue[5] := '0.0';    // 'varvalue';
    PropertyValue[6] := '';       // 'expression';


    inherited  InitPropertyValues(NumPropsThisClass);

end;


end.
