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

        FNumvars            : Integer;
        FvarValues          : Array of double;
        FvarInitValues      : Array of double;
        FvarNames           : TStringList;
        FvarIOType          : Array of integer;

      public
        BaseFrequency       : Double;
        FActiveVar           : String;

        constructor Create(ParClass:TDSSClass; const LineCodeName:String);
        destructor Destroy; override;

        FUNCTION  InterpretDiffEq( Exp : String ):Boolean;
        FUNCTION  GetPropertyValue( Index : Integer ):String;Override;
        PROCEDURE InitPropertyValues( ArrayOffset  : Integer );Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

   end;

VAR
   DynamicExpClass    : TDynamicExp;
   ActiveDynamicExpObj: TDynamicExpObj;

implementation

  USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils, Ucomplex, Utilities, LineUnits;

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
       PropertyHelp[6] := 'It is the differential expression using OpenDSS syntax for example:' + CRLF +
                          'dt(w) = 1/M*(P_m - D*w - P_e)';


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
                    DoSimpleMsg('DynamicExp "' + FActiveVar + '" not found "', 50001);
                    FActiveVar    := '';
                  End;
                 End;
              5: Begin
                  if ((ActiveElement < length(FVarValues)) and (ActiveElement >= 0)) then
                    FVarValues[ActiveElement]  :=  Parser[ActorID].DblValue
                  else
                    DoSimpleMsg('There is not valid DynamicExp active"', 50002);
                 End;
              6: Begin
                  if (InterpretDiffEq( Parser[ActorID].StrValue )) then
                    DoSimpleMsg('There are errors in the dynamic expression"', 50003);
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

      DoSimpleMsg('DynamicExp: "' + Value + '" not Found.', 103);

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
    FVarNames.Clear;
    setlength(FVarValues, 0);
    setlength(FVarInitValues, 0);
    setlength(FVarIOType, 0);

    InitPropertyValues(0);
  END;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  destructor TDynamicExpObj.Destroy;
  BEGIN
    FVarNames.Clear;
    setlength(FVarValues, 0);
    setlength(FVarInitValues, 0);
    setlength(FVarIOType, 0);
    Inherited destroy;
  END;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  FUNCTION  TDynamicExpObj.InterpretDiffEq( Exp : String ):Boolean;
  Begin

    Result  :=  false;
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
