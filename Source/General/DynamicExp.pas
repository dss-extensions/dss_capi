unit DynamicExp;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

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

       Function Edit(ActorID : Integer):Integer; override;     // uses global parser
       Function Init(Handle:Integer; ActorID : Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       // Set this property to point ActiveLineCodeObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   end;

   TDynamicExpObj = class(TDSSObject)
      private
        FNumvars            : Integer;              // Number of state variables
        FvarNames           : TStringList;          // List containing the state variable names
        FVarConst,                                  // Array containing the numeric constants of the expression
        FvarValues,                                 // Array containing the variable values of the expression
        FvarInitValues      : Array of double;      // Array containing the initial values of the state variables
        FCmd,                                       // Sequence of commands that implement the expression
        FvarIOType          : Array of integer;     // Variable IO type
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


   end;

Const
    myOps   : array [0..10] of string =
    ('dt', '=', '+', '-', '*', '/', '(', ')', ';', '[', ']');

VAR
   DynamicExpClass    : TDynamicExp;
   ActiveDynamicExpObj: TDynamicExpObj;

implementation

  USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils, Ucomplex, Utilities, LineUnits, math;

  Const      NumPropsThisClass = 6;

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
       PropertyName[3] := 'varinit';
       PropertyName[4] := 'var';
       PropertyName[5] := 'varvalue';
       PropertyName[6] := 'expression';

       PropertyHelp[1] := '(Int) Number of state variables to be considered in the differential equation.';
       PropertyHelp[2] := '([String]) Array of strings with the names of the state variables.';
       PropertyHelp[3] := '([dbl]) Array of doubles indicating the intial values of state variables.';
       PropertyHelp[4] := '(String) Activates the state variable using the given name.';
       PropertyHelp[5] := '(dbl) Floating point number indicating the value of the active state variable.';
       PropertyHelp[6] := 'It is the differential expression using OpenDSS RPN syntax. The expression must be contained within brackets in case of having multiple equations, for example:' + CRLF + CRLF +
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
                  setlength(FVarValues,FNumVars);
                 End;
              2: Begin
                  InterpretTStringListArray(Param, FVarNames);
                  idx     :=  FVarNames.Count;
                  // ensuring they are lower case
                  for idx := 0 to (FVarNames.Count - 1) do FVarNames[idx]  :=  LowerCase(FVarNames[idx]);
                  if (FNumVars <>  FVarnames.Count) then
                  Begin
                   FNumVars  :=  FVarNames.Count;
                   setlength(FVarValues,FNumVars);
                  End;
                 End;
              3: Begin
                  setlength(FVarInitValues,FNumVars);
                  Parser[ActorID].ParseAsVector(FnumVars,@(FVarInitValues[0]))
                 End;
              4: Begin
                  FActiveVar      :=  LowerCase(Parser[ActorID].StrValue);
                  if not FVarNames.Find(FActiveVar, ActiveElement) then
                  Begin
                    // Being here means that the given name doesn't exist
                    DoSimpleMsg('DynamicExp "' + FActiveVar + '" not found.', 50001);
                    FActiveVar    := '';
                  End;
                 End;
              5: Begin
                  if ((ActiveElement < length(FVarValues)) and (ActiveElement >= 0)) then
                    FVarValues[ActiveElement]  :=  Parser[ActorID].DblValue
                  else
                    DoSimpleMsg('There is not valid DynamicExp active.', 50002);
                 End;
              6: Begin
                  if (InterpretDiffEq( Parser[ActorID].StrValue )) then
                    DoSimpleMsg('There are errors in the differential equation.', 50003);
                 End

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
  //      TLineCode Obj
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  constructor TDynamicExpObj.Create(ParClass:TDSSClass; const LineCodeName:String);

  BEGIN
    Inherited Create(ParClass);
    Name               :=  LowerCase(LineCodeName);
    DSSObjType         :=  ParClass.DSSClassType;
    FVarNames          :=  nil;
    FNumVars           :=  20;
    FActiveVar         :=  '';
    FVarNames          :=  TStringList.create;
    FVarNames.Clear;
    setlength(FVarValues, 0);
    setlength(FVarInitValues, 0);
    setlength(FVarIOType, 0);
    setlength(FCmd, 0);
    setlength(FVarConst, 0);

    InitPropertyValues(0);
  END;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  destructor TDynamicExpObj.Destroy;
  BEGIN
    FVarNames.Clear;
    setlength(FVarValues, 0);
    setlength(FVarInitValues, 0);
    setlength(FVarIOType, 0);
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
    for idx := 0 to High(FVarValues) do
      if myVar = FVarNames[idx] then
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

  FUNCTION  TDynamicExpObj.InterpretDiffEq( Exp : String ):Boolean;
  {Builds the list of commands required for operating the equation declared, this
  automation is intended to acelerate the calculation in run time.
  Notation:
    Positive integers represent the index to a variable slot (dbl)
    If the integer is a value >= 50000, it means that it is the index to a
    numeric constant that can be located at FVarConst
    If is a negative integer, represents one of the following operations:
      2 = Add
      3 = Subtraction
      4 = Mult
      5 = Div
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
    FXpression  :=  Lowercase(Exp);
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
          2, 3, 4, 5, 10:   // it is one of the basic operations or end of the diff eq
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
