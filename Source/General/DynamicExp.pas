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

USES
   Command, DSSClass, DSSObject, UcMatrix, Arraydef, Classes;


TYPE

   TDynamicExp = class(TDSSClass)
     private
       SymComponentsChanged:Boolean;
       MatrixChanged:Boolean;

       Function Get_Code:String;  // Returns active line code string
       Procedure Set_Code(const Value:String);  // sets the  active linecode

     Protected
       Procedure DefineProperties;
       Function MakeLike(Const LineName:String):Integer;  Override;
     public
       
       constructor Create;
       destructor Destroy; override;

       Function Find(const ObjName: String): Pointer;
       Function Edit(ActorID : Integer):Integer; override;     // uses global parser
       Function Init(Handle:Integer; ActorID : Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       // Set this property to point ActiveLineCodeObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   end;

   TDynamicExpObj = class(TDSSObject)
      private
        FVarIdx,
        FNumvars            : Integer;              // Number of state variables
        FvarNames           : TStringList;          // List containing the state variable names
        FVarConst           : Array of double;      // Array containing the numeric constants of the expression
        FCmd                : Array of integer;     // Sequence of commands that implement the expression
        FActiveVar,                                 // Name of the active variable
        FXpression          : String;               // Differential equiation in RPN format

      public
        BaseFrequency       : Double;

        constructor Create(ParClass:TDSSClass; const LineCodeName:String);
        destructor Destroy; override;

        FUNCTION  InterpretDiffEq( Exp : String ):Boolean;
        FUNCTION  GetPropertyValue( Index : Integer ):String;Override;
        PROCEDURE InitPropertyValues( ArrayOffset  : Integer );Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;
        function  Get_Closer_Op(myExp : string; var myOp : string; var OpCode : Integer): Integer;
        function  Get_Var_Idx(myVar : string): Integer;
        function  Check_If_CalcValue(myVal : string; var myOp : Integer): Boolean;
        function  Get_Out_Idx(myVar : string): Integer;           // gets the index for the given variable if it is an output
        PROCEDURE SolveEq(var MemSpace : array of DynSlot);       // Equation solver
        FUNCTION  IsInitVal(myCode : integer): Boolean;            // returns true if the given code is for an initialization value
        FUNCTION  Get_DynamicEqVal(myIdx : integer; var MemSpace : array of DynSlot): double;
        FUNCTION  Get_VarName(myIdx : integer): String;

        property NumVars  : integer  read FNumVars write FNumVars;


   end;

Const
    myOps   : array [0..28] of string =
    ('dt', '=', '+', '-', '*', '/', '(', ')', ';', '[', ']',
    'sqr', 'sqrt', 'inv', 'ln', 'exp', 'log10', 'sin', 'cos',
    'tan', 'asin', 'acos', 'atan', 'atan2', 'rollup', 'rolldn',
    'swap', 'pi', '^');

VAR
   DynamicExpClass    : TDynamicExp;
   ActiveDynamicExpObj: TDynamicExpObj;

implementation

  USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils, Ucomplex, Utilities, LineUnits, math, RPN;

  Const      NumPropsThisClass = 5;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  constructor TDynamicExp.Create;  // Creates superstructure for all DynamicExp objects
  BEGIN
       Inherited Create;
       Class_Name         := 'DynamicExp';
       DSSClassType       := DSS_OBJECT;
       ActiveElement      := 0;

       DefineProperties;

       CommandList        := TCommandList.Create(Slice(PropertyName^, NumProperties));
       CommandList.Abbrev := TRUE;

       DynamicExpClass    := Self;
  END;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Destructor TDynamicExp.Destroy;

  BEGIN
      // ElementList and  CommandList freed in inherited destroy
      Inherited Destroy;
  END;
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Procedure TDynamicExp.DefineProperties;
  Begin

       Numproperties := NumPropsThisClass;
       CountProperties;   // Get inherited property count
       AllocatePropertyArrays;


       PropertyName[1] := 'nvariables';
       PropertyName[2] := 'varnames';
       PropertyName[3] := 'var';
       PropertyName[4] := 'varidx';
       PropertyName[5] := 'expression';

       PropertyHelp[1] := '(Int) Number of state variables to be considered in the differential equation.';
       PropertyHelp[2] := '([String]) Array of strings with the names of the state variables.';
       PropertyHelp[3] := '(String) Activates the state variable using the given name.';
       PropertyHelp[4] := '(Int) read-only, returns the index of the active state variable.';
       PropertyHelp[5] := 'It is the differential expression using OpenDSS RPN syntax. The expression must be contained within brackets in case of having multiple equations, for example:' + CRLF + CRLF +
                          'expression = "[w dt = 1 M / (P_m D*w - P_e -) *]"';


       ActiveProperty := NumPropsThisClass;
       inherited DefineProperties;  // Add defs of inherited properties to bottom of list

  End;
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Function TDynamicExp.NewObject(const ObjName:String):Integer;
  BEGIN
     // create a new object of this class and add to list
     With ActiveCircuit[ActiveActor] Do
     Begin
      ActiveDSSObject[ActiveActor] := TDynamicExpObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
     End;
  END;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  function TDynamicExp.Find(const ObjName: String): Pointer;
  begin
      If (Length(ObjName)=0) or (CompareText(ObjName, 'none')=0) Then Result := Nil
      Else Result := Inherited Find(ObjName);
  end;


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Function TDynamicExp.Edit(ActorID : Integer):Integer;
  VAR
     idx,
     ParamPointer : Integer;
     ParamName,
     Param        : String;

  BEGIN
    Result := 0;
    // continue parsing with contents of Parser
    ActiveDynamicExpObj := ElementList.Active;
    ActiveDSSObject[ActorID] := ActiveDynamicExpObj;
    SymComponentsChanged := False;
    MatrixChanged := False;

    WITH ActiveDynamicExpObj DO BEGIN

       ParamPointer := 0;
       ParamName := Parser[ActorID].NextParam;
       Param := Parser[ActorID].StrValue;
       WHILE Length(Param)>0 DO BEGIN
           IF Length(ParamName) = 0 THEN Inc(ParamPointer)
           ELSE ParamPointer := CommandList.GetCommand(ParamName);

           If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

           CASE ParamPointer OF
              0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 50001);
              1: Begin
                  FNumVars := Parser[ActorID].IntValue;  // Use property value to force reallocations
                 End;
              2: Begin
                  InterpretTStringListArray(Param, FVarNames);
                  // ensuring they are lower case
                  for idx := 0 to (FVarNames.Count - 1) do FVarNames[idx]  :=  LowerCase(FVarNames[idx]);
                 End;
              3: Begin
                  FActiveVar        :=  LowerCase(Parser[ActorID].StrValue);
                  FVarIdx   :=  FVarNames.IndexOf(FActiveVar);
                  if FVarIdx < 0 then
                  Begin
                    // Being here means that the given name doesn't exist
                    DoSimpleMsg('DynamicExp "' + FActiveVar + '" not found.', 50001);
                    FActiveVar    := '';
                  End;
                 End;
              5: Begin
                  if (InterpretDiffEq( Parser[ActorID].StrValue )) then
                    DoSimpleMsg('There are errors in the differential equation.', 50003);
                 End;
           ELSE
             ClassEdit(ActiveDynamicExpObj, Parampointer - NumPropsThisClass)
           END;
{
           CASE ParamPointer OF
               1 : ;
           END;

}
           ParamName := Parser[ActorID].NextParam;
           Param := Parser[ActorID].StrValue;
       END;

    END;

  END;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Function TDynamicExp.MakeLike(Const LineName:String):Integer;
  VAR
     OtherDynExpCode:TDynamicExpObj;
     i:Integer;
  BEGIN
     Result := 0;
     {See if we can find this DynamicExp code in the present collection}



  END;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Function TDynamicExp.Init(Handle:Integer; ActorID : Integer):Integer;

  BEGIN
     DoSimpleMsg('Need to implement TDynamicExp.Init', -1);
     REsult := 0;
  END;

  Function TDynamicExp.Get_Code:String;  // Returns active line code string

  BEGIN

    Result := '';

  END;

  Procedure TDynamicExp.Set_Code(const Value:String);  // sets the  active linecode
  VAR
    DynExpCodeObj:TDynamicExpObj;
  BEGIN

      ActiveDynamicExpObj := Nil;
      DynExpCodeObj := ElementList.First;
      WHILE DynExpCodeObj<>Nil DO BEGIN

         IF CompareText(DynExpCodeObj.Name, Value)=0 THEN BEGIN
            ActiveDynamicExpObj := DynExpCodeObj;
            Exit;
         END;

         DynExpCodeObj := ElementList.Next;
      END;

      DoSimpleMsg('DynamicExp: "' + Value + '" not Found.', 50004);

  END;


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  //      TDynamicExp Obj
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  constructor TDynamicExpObj.Create(ParClass:TDSSClass; const LineCodeName:String);

  BEGIN
    Inherited Create(ParClass);
    Name               :=  LowerCase(LineCodeName);
    DSSObjType         :=  ParClass.DSSClassType;
    FVarNames          :=  nil;
    FNumVars           :=  20;
    FActiveVar         :=  '';
    FVarIdx            :=  -1;
    FVarNames          :=  TStringList.create;
    FVarNames.Clear;
    setlength(FCmd, 0);
    setlength(FVarConst, 0);

    InitPropertyValues(0);
  END;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  {Checks if the given string is a value calculated by the element using the eq model}
  function  TDynamicExpObj.Check_If_CalcValue(myVal : string; var myOp : Integer): Boolean;
  var
    found : Boolean;
    Val   : String;
    idx   : Integer;
  Const
    ValNames  : Array [0..9] of string =
    ('p','q','vmag','vang','imag','iang','s', 'p0', 'q0', 'edp');

  Begin
    myOp  :=  -1;
    found :=  False;
    Val   :=  LowerCase(myVal);
    for idx := 0 to High(ValNames) do
    Begin
      if Val = ValNames[idx] then
      Begin
        myOp  :=  idx;
        Found :=  True;
        break;
      End;
    End;

    Result  :=  found;
  End;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  destructor TDynamicExpObj.Destroy;
  BEGIN
    FVarNames.Clear;
    setlength(FVarConst, 0);
    Inherited destroy;
  END;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  function  TDynamicExpObj.Get_Closer_Op(myExp : string; var myOp : string; var OpCode : Integer): Integer;
  var
    myPos,
    idx       : Integer;
  Begin
    Result    :=  10000;
    for idx := 0 to High(myOps) do
    Begin
      myPos   :=  Pos(myOps[idx], myExp);
      if (myPos < Result) and (myPos > 0) then
      Begin
        Result    :=  myPos;
        myOp      :=  myOps[idx];
        OpCode    :=  idx;
      End;
    End;
  End;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  {returns the index of the variable if it exists in the state variable list,
  otherwise, it returns 50001 if the string entered is a numeric constant (dbl)
  or -1 if the string entered is neither a numeric constant or state varaible}
  function  TDynamicExpObj.Get_Var_Idx(myVar : string): Integer;
  var
    dblval  : double;
    idx     : Integer;
  Begin

    dblval  :=  0.0;
    Result  :=  -1;   // error
    for idx := 0 to (FVarNames.Count - 1) do
      if Lowercase(myVar) = FVarNames[idx] then
      Begin
        Result  :=  idx;
        break;
      End;

    if Result < 0 then
    Begin
      // so, it's not a state variable, maybe a constant
      try
        dblval  :=  strtofloat(myVar);
        Result  :=  50001;  // returns this code to indicate that it is a constant
      except
        Result  :=  -1;  // it's not a number
      end;
    End;

  End;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  function  TDynamicExpObj.Get_Out_Idx(myVar : string): Integer;
  {returns the index of the variable if it exists in the state variable list,
  and if it is an output (-50 in the next cell ot the FCmd automation array)}
  var
    dblval  : double;
    CmdIdx,
    idx     : Integer;
  Begin

    dblval  :=  0.0;
    Result  :=  -1;   // error
    for idx := 0 to (FVarNames.Count - 1) do
      if Lowercase(myVar) = FVarNames[idx] then
      Begin
        // now, check if the index corresponds to an output
        for CmdIdx := 0 to High(FCmd) do
        Begin
          if ( idx = FCmd[CmdIdx] ) and (CmdIdx < High(FCmd)) then
            if FCmd[CmdIdx + 1] = -50 then
            Begin
              // Means that the variable found is an output, we can leave
              Result  :=  idx;
              break;
            End;
        End;

      End;

  End;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  FUNCTION  TDynamicExpObj.Get_VarName(myIdx : integer): String;
  VAR
    diffstr : String;
    myProt  : DynSlot;
    mylen,
    myCol,
    myRow   : Integer;
  Begin
    mylen   :=  length(myProt);
{$IFDEF FPC}
    myRow   :=  myIdx div mylen;
{$ELSE}
    myRow   :=  floor(double(myIdx) / double(mylen));
{$ENDIF}
    myCol   :=  myIdx - ( myRow * mylen );
    diffstr :=  '';
    if myCol > 0 then
    Begin
      diffstr :=  'd';
      if myCol > 1 then diffstr :=  diffstr + inttostr(myCol);
    End;

    Result:=  diffstr + FVarNames[myRow];

  End;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  FUNCTION TDynamicExpObj.Get_DynamicEqVal(myIdx : integer; var MemSpace : array of DynSlot): double;
  VAR
    mylen,
    myCol,
    myRow   : Integer;
  Begin
    mylen :=  length(MemSpace[0]);
{$IFDEF FPC}
    myRow   :=  myIdx div mylen;
{$ELSE}
    myRow   :=  floor(double(myIdx) / double(mylen));
{$ENDIF}
    myCol :=  myIdx - ( myRow * mylen );
    Result:=  MemSpace[myRow][myCol];
  End;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  FUNCTION TDynamicExpObj.IsInitVal(myCode : integer): Boolean;
  Begin
    Result  :=  False;
    case myCode of
      7,8,9: Result := True;
    end;
  End;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  PROCEDURE TDynamicExpObj.SolveEq(var MemSpace : array of DynSlot);
  var
    mylen,
    MaxIdx,
    OutIdx,
    idx       : Integer;
    RPN       : TRPNCalc;
  Begin

    RPN       :=  TRPNCalc.Create;
    OutIdx    :=  -1;
    for idx := 0 to High(FCmd) do
    Begin
      if ( FCmd[idx + 1] = -50 ) or ( FCmd[idx] = -50 ) then    // it's the begining of an equation
      Begin
        MaxIdx  :=  0;
        if ( FCmd[idx] <> -50 ) then                            // The index
        Begin
          if OutIdx >= 0 then                                   // It's not the first equation
            MemSpace[OutIdx][1]  :=  RPN.X;                     // Uploads value into memory space
          OutIdx  :=  FCmd[idx];
        End;
      End
      else
      Begin
        case FCmd[idx] of
          -2 : RPN.Add;             //Add
          -3 : RPN.Subtract;        //Sub
          -4 : RPN.Multiply;        //Mult
          -5 : RPN.Divide;          //Div
          -11: RPN.Square;          //Sqr
          -12: RPN.Sqrt;            //Sqrt
          -13: RPN.Inv;             //Inv
          -14: RPN.NatLog;          //ln
          -15: RPN.etothex;         //exp
          -16: RPN.TenLog;          //log10
          -17: RPN.Sindeg;          //Sin
          -18: RPN.Cosdeg;          //Cos
          -19: RPN.Tandeg;          //Tan
          -20: RPN.aSindeg;         //ASin
          -21: RPN.aCosdeg;         //ACos
          -22: RPN.aTandeg;         //ATan
          -23: RPN.aTan2deg;        //ATan2
          -24: RPN.RollUp;          //RollUp
          -25: RPN.RollDn;          //RollDn
          -26: RPN.SwapXY;          //Swap
          -27: RPN.EnterPi;         //Pi
          -28: RPN.YToTheXPower;    //^
          else
          Begin
            if FCmd[idx] >= 50000 then
              RPN.X  :=  FVarConst[FCmd[idx] - 50000]  // It's a constant
            else
              RPN.X  :=  MemSpace[FCmd[idx]][0];       // It's a variable
          End;

        end;

      End;

    End;
    MemSpace[OutIdx][1]  :=  RPN.X;               // Uploads value into memory space
    RPN.Free;                                     // Destroy RPN calculator
  End;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  FUNCTION  TDynamicExpObj.InterpretDiffEq( Exp : String ):Boolean;
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
    OpIdx     : integer;
    ErrorSrc,
    SubXp,
    Op        : String;
    myVars    : TStringList;
  Begin
    Result    :=  false;
    ErrorSrc  :=  '';
    myVars    :=  TStringList.Create;
    myVars.Clear;
    setlength(FCmd,0);
    FXpression  :=  '[' + Lowercase(Exp) + ']';
    while FXpression.Length > 0 do
    Begin
      OpIdx   :=  Get_Closer_Op(FXpression, Op, OpCode);

      if OpIdx = 10000 then
      Begin
        FXpression  :=  ''   // we're done
      end
      else
      Begin
        SubXp       :=  FXpression.Substring(0, OpIdx - 1);
        if Op.Length > 1 then OpIdx :=  OpIdx + Op.Length;
        FXpression  :=  FXpression.Substring(OpIdx, FXpression.Length);
        InterpretTStringListArray(SubXp, myVars);
        case OpCode of
          0:  Begin
                setlength(FCmd,length(FCmd) + 2);
                OpIdx :=  Get_Var_Idx(myVars[0]);     // the result is always placed at the begin
                if OpIdx = 50001 then
                Begin
                  DoSimpleMsg('DynamicExp: the expression preceeding the "dt" operand has to be a state variable.', 50006);
                  ErrorSrc  :=  'preceeding differential output';
                End
                else
                Begin
                  if OpIdx < 0 then ErrorSrc  :=  myVars[0]
                  else
                  Begin
                    FCmd[High(FCmd) - 1]  :=  OpIdx;
                    FCmd[High(FCmd)]      :=  -50;   // denotes the begining of an equation
                  End;
                End;
              End;
          1, 6, 7, 8, 9:
              Begin
              // Do nothing, it's just for notation reference at the user side
              End;
          else   // it is one of the basic operations or end of the diff eq
              Begin
                for Idx := 0 to ( myVars.Count - 1 ) do
                Begin
                  setlength(FCmd,Length(FCmd) + 1);
                  OpIdx                      :=  Get_Var_Idx(myVars[idx]);
                  if OpIdx = 50001 then
                  Begin
                    setlength(FVarConst,Length(FVarConst) + 1);
                    FVarConst[High(FVarConst)]  :=  strtofloat(myVars[idx]);
                    FCmd[High(FCmd)]            :=  50000 + High(FVarConst);
                  End
                  else
                  Begin
                    if OpIdx < 0 then ErrorSrc          :=  '"' + myVars[0] + '"'
                    else              FCmd[High(FCmd)]  :=  Get_Var_Idx(myVars[idx]);
                  End;
                End;
                if OpCode <> 10 then
                Begin
                  setlength(FCmd,Length(FCmd) + 1);
                  FCmd[High(FCmd)] :=  -1 * OpCode;    // assings the operator -> + - * /
                end;
              End;

        end;
      End;
      if ErrorSrc <> '' then
      Begin
        DoSimpleMsg('DynamicExp: Variable ' + ErrorSrc + ' not Found.', 50005);
        FXpression  :=  '';
        Result      :=  true;
      End;
    End;

    if not Result then  FXpression  :=  Exp;    // assings the expression again to keep a local copy

  End;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  PROCEDURE TDynamicExpObj.DumpProperties(var F: TextFile; Complete: Boolean);

  Var
     k,
     i,j :Integer;
     TempStr  : String;

  Begin
    Inherited DumpProperties(F, Complete);

    WITH ParentClass Do
    Begin

      For i := 1 to NumProperties Do
      Begin
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
      End;

    End;
  end;

  function TDynamicExpObj.GetPropertyValue(Index: Integer): String;
  var
    j       : Integer;
  begin
       case Index of
           1  : Result := Format('%d', [FNumVars]);
           4  : Result := Format('%d', [FVarIdx]);

       Else
          Result := Inherited GetPropertyValue(index);
       end;
  end;

  procedure TDynamicExpObj.InitPropertyValues(ArrayOffset: Integer);
  begin

       PropertyValue[1] :=  '0';      // 'nvariables';
       PropertyValue[2] :=  '[]';     // 'varnames';
       PropertyValue[3] :=  '[0.0]';  // 'varinit';
       PropertyValue[4] :=  '';       // 'var';
       PropertyValue[5] :=  '0.0';    // 'varvalue';
       PropertyValue[6] :=  '';       // 'expression';


      inherited  InitPropertyValues(NumPropsThisClass);

  end;


end.
