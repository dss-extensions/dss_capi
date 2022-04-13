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
   Command, DSSClass, DSSObject, UcMatrix, Arraydef;


TYPE

   TDynamicExp = class(TDSSClass)
     private
       SymComponentsChanged:Boolean;
       MatrixChanged:Boolean;

       Function Get_Code:String;  // Returns active line code string
       Procedure Set_Code(const Value:String);  // sets the  active linecode

       Procedure SetZ1Z0(i:Integer; Value:Double);
       Procedure SetUnits(Const s:String);  // decode units specification

       Procedure DoMatrix(i:Integer; ActorID : Integer);  // set impedances as matrices

       
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

        FNeutralConductor :Integer;

        Procedure Set_NPhases(Value:Integer);
        Procedure DoKronReduction;
        Function get_Rmatrix: string;
        Function get_Xmatrix: string;
        Function get_CMatrix: string;

      public
        NumAmpRatings,
        FNPhases            :Integer;

        SymComponentsModel,
        ReduceByKron        :Boolean;

        Z,         // Base Frequency Series Z matrix
        Zinv,
        YC                  :    TCMatrix;  // Shunt capacitance matrix at Base frequency.

        BaseFrequency       :Double;

        R1,
        X1,
        R0,
        X0,
        C1,
        C0,
        NormAmps,
        EmergAmps,
        FaultRate,
        PctPerm,
        HrsToRepair,
        Rg,
        Xg,
        rho               : Double;
        AmpRatings        : TRatingsArray;
        FLineType         : Integer; // Pointer to code for type of line

        Units:Integer;  {See LineUnits}

        constructor Create(ParClass:TDSSClass; const LineCodeName:String);
        destructor Destroy; override;
        Property NumPhases:Integer Read FNPhases Write Set_Nphases;
        Procedure CalcMatricesFromZ1Z0;

        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

   end;

VAR
   DynamicExpClass    : TDynamicExp;
   ActiveDynamicExpObj: TDynamicExpObj;

implementation

USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils, Ucomplex, Utilities, LineUnits;

Const      NumPropsThisClass = 3;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TDynamicExp.Create;  // Creates superstructure for all Line objects
BEGIN
     Inherited Create;
     Class_Name := 'LineCode';
     DSSClassType := DSS_OBJECT;
     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;

     DynamicExpClass := Self;
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
     PropertyName[3] := 'expression';

     PropertyHelp[1] := 'Number of state variables to be considered in the differential equation.';
     PropertyHelp[2] := 'Array of strings with the names of the state variables.';
     PropertyHelp[3] := 'It is the differential expression using OpenDSS syntax for example:' + CRLF +
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

Function TDynamicExpObj.get_Rmatrix: string;
var
  j: Integer;
  i: Integer;
begin
  Result := '[';
  for i := 1 to FNPhases do
  begin
    for j := 1 to FNphases do
    begin
      Result := Result + Format('%12.8f ', [Z.GetElement(i, j).re]);
    end;
    if i < FNphases then
      Result := Result + '|';
  end;
  Result := Result + ']';
end;

Function TDynamicExpObj.get_Xmatrix: string;
var
  j: Integer;
  i: Integer;
begin
  Result := '[';
  for i := 1 to FNPhases do
  begin
    for j := 1 to FNphases do
    begin
      Result := Result + Format('%12.8f ', [Z.GetElement(i, j).im]);
    end;
    if i < FNphases then
      Result := Result + '|';
  end;
  Result := Result + ']';
end;

function TDynamicExpObj.get_CMatrix: string;
Var  i,j:Integer;
begin
        Result := '[';
         FOR i := 1 to FNPhases DO Begin
           FOR j := 1 to FNphases DO Begin
               Result := Result + Format('%12.8f ',[Yc.GetElement(i,j).im/TwoPi/BaseFrequency * 1.0E9]);
           End;
           If i< FNphases then Result := Result + '|';
         End;
         Result := Result + ']';
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TDynamicExp.SetUnits(Const s:String);
// decodes the units string and sets the Units variable

BEGIN

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TDynamicExp.SetZ1Z0(i:Integer; Value:Double);
// set symmetrical component impedances and a flag to indicate they were changed
BEGIN



END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TDynamicExp.DoMatrix(i:Integer; ActorID : Integer);

VAR
    OrderFound, Norder,j:Integer;
    MatBuffer:pDoubleArray;
    Zvalues:pComplexArray;
    Factor:Double;

BEGIN

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TDynamicExp.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

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
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 101);
            1: Numphases := Parser[ActorID].IntValue;  // Use property value to force reallocations
            2: SetZ1Z0(1, Parser[ActorID].Dblvalue);  {R1}
            3: SetZ1Z0(2, Parser[ActorID].Dblvalue);  {X0}

         ELSE
           ClassEdit(ActiveDynamicExpObj, Parampointer - NumPropsThisClass)
         END;

         CASE ParamPointer OF
             9..11: SymComponentsModel := FALSE;
             18: IF ReduceByKron and Not SymComponentsModel Then DoKronReduction;
         END;

         
         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     END;

     IF SymComponentsModel THEN CalcMatricesFromZ1Z0;
     IF MatrixChanged THEN BEGIN
        Zinv.Copyfrom(Z);
        Zinv.Invert;
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
   {See if we can find this line code in the present collection}



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

    DoSimpleMsg('Linecode: "' + Value + '" not Found.', 103);

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLineCode Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TDynamicExpObj.Create(ParClass:TDSSClass; const LineCodeName:String);

BEGIN
     Inherited Create(ParClass);
     Name               := LowerCase(LineCodeName);
     DSSObjType         := ParClass.DSSClassType;

     FNPhases           := 3;  // Directly set conds and phases

    InitPropertyValues(0);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TDynamicExpObj.Destroy;
BEGIN
    Z.Free;
    Zinv.Free;
    Yc.Free;

    Inherited destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TDynamicExpObj.Set_NPhases(Value:Integer);
// Set the number of phases and reallocate phase-sensitive arrays
// Need to preserve values in Z matrices

BEGIN
    If Value>0 THEN BEGIN
      IF FNphases <> Value THEN BEGIN    // If size is no different, we don't need to do anything
        FNPhases := Value;
        FNeutralConductor := FNphases;  // Init to last conductor
        // Put some reasonable values in these matrices
        CalcMatricesFromZ1Z0;  // reallocs matrices
      END;
    END;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TDynamicExpObj.CalcMatricesFromZ1Z0;
VAR
   Zs,Zm,Ys, Ym, Ztemp:Complex;
   i,j:Integer;
   Yc1, Yc0, OneThird: double;

BEGIN
    IF Z<>nil    THEN Z.Free;
    IF Zinv<>nil THEN Zinv.Free;
    IF Yc<>nil   THEN Yc.Free;

    // For a line, nphases = ncond, for now
    Z    := TCmatrix.CreateMatrix(FNphases);
    Zinv := TCMatrix.CreateMatrix(FNphases);
    Yc   := TCMatrix.CreateMatrix(FNphases);

    OneThird := 1.0/3.0;  // Do this to get more precision in next few statements

    Ztemp := CmulReal(cmplx(R1,X1),2.0);
    Zs := CmulReal(CAdd(Ztemp, Cmplx(R0, X0)), OneThird);
    Zm := CmulReal(Csub(cmplx(R0, X0), Cmplx(R1, X1)), OneThird);

    Yc1 := TwoPi * BaseFrequency * C1;
    Yc0 := TwoPi * BaseFrequency * C0;

    Ys := CMulReal(Cadd(CMulReal(Cmplx(0.0, Yc1), 2.0), Cmplx(0.0, Yc0)), OneThird);
    Ym := CmulReal(Csub(cmplx(0.0, Yc0), Cmplx(0.0, Yc1)), OneThird);

    FOR i := 1 to FNphases DO BEGIN
       Z.SetElement(i,i, Zs);
       Yc.SetElement(i,i, Ys);
       FOR j := 1 to i-1 DO BEGIN
           Z.SetElemsym(i,j, Zm);
           Yc.SetElemsym(i,j, Ym);
       END;
    END;
    Zinv.Copyfrom(Z);
    Zinv.Invert;
END;

PROCEDURE TDynamicExpObj.DumpProperties(var F: TextFile; Complete: Boolean);

Var
   k,
   i,j :Integer;
   TempStr  : String;

Begin
    Inherited DumpProperties(F, Complete);

    WITH ParentClass Do
    Begin

        Writeln(F,'~ ',PropertyName^[1],'=',FNphases:0);
        Writeln(F,'~ ',PropertyName^[2],'=',R1:0:5);
        Writeln(F,'~ ',PropertyName^[3],'=',X1:0:5);
        Writeln(F,'~ ',PropertyName^[4],'=',R0:0:5);
        Writeln(F,'~ ',PropertyName^[5],'=',X0:0:5);
        Writeln(F,'~ ',PropertyName^[6],'=',C1 * 1.0e9:0:5);
        Writeln(F,'~ ',PropertyName^[7],'=',C0 * 1.0e9:0:5);
        Writeln(F,'~ ',PropertyName^[8],'=',PropertyValue[8]);
        Write(F,'~ ',PropertyName^[9],'=','"');
           FOR i := 1 to FNPhases DO Begin
             FOR j := 1 to FNphases DO Begin
                 Write(F, Z.GetElement(i,j).re:0:8,' ');
             End;
             Write(F,'|');
           End;
           Writeln(F,'"');
        Write(F,'~ ',PropertyName^[10],'=','"');
           FOR i := 1 to FNPhases DO Begin
             FOR j := 1 to FNphases DO Begin
                 Write(F, Z.GetElement(i,j).im:0:8,' ');
             End;
             Write(F,'|');
           End;
           Writeln(F,'"');
        Write(F,'~ ',PropertyName^[11],'=','"');
           FOR i := 1 to FNPhases DO Begin
             FOR j := 1 to FNphases DO Begin
                 Write(F, (Yc.GetElement(i,j).im/TwoPi/BaseFrequency * 1.0E9):0:8,' ');
             End;
             Write(F,'|');
           End;
           Writeln(F,'"');


         For i := 12 to 21 Do
         Begin
            Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
         End;

         Writeln(F, Format('~ %s=%d',[PropertyName^[22], FNeutralConductor]));
         Writeln(F, Format('~ %s=%d',[PropertyName^[25], NumAmpRatings]));
         TempStr   :=  '[';
         for  k:= 1 to NumAmpRatings do
          TempStr :=  TempStr + floattoStrf(AmpRatings[k-1],ffGeneral,8,4) + ',';
         TempStr   :=  TempStr + ']';
         Writeln(F, Format('~ %s=%s',[PropertyName^[26]]) + TempStr);


     End;

end;

function TDynamicExpObj.GetPropertyValue(Index: Integer): String;
var
  j       : Integer;
begin
     case Index of
         1  : Result := Format('%d', [FnPhases]);
         2  : If SymComponentsModel Then Result := Format('%.5g', [R1]) else Result := '----';
         3  : If SymComponentsModel Then Result := Format('%.5g', [X1]) else Result := '----';

     Else
        Result := Inherited GetPropertyValue(index);
     end;
end;

procedure TDynamicExpObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] :=  '0'; // 'nvariables';
     PropertyValue[2] :=  '[]'; // 'varnames';
     PropertyValue[3] :=  ''; // 'expression';


    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TDynamicExpObj.DoKronReduction;
Var
        NewZ, NewYC : TcMatrix;

begin
   If FneutralConductor=0 then Exit;   // Do Nothing

   NewZ := Nil;
   NewYC := Nil;
   
   If Fnphases>1 then Begin
        Try
          NewZ  := Z.Kron(FNeutralConductor);       // Perform Kron Reductions into temp space
        { Have to invert the Y matrix to eliminate properly}
          YC.Invert;  // Vn = 0 not In
          NewYC := YC.Kron(FNeutralConductor);
        Except
          On E:Exception Do DoSimpleMsg(Format('Kron Reduction failed: LineCode.%s. Attempting to eliminate Neutral Conductor %d.', [Name, FNeutralConductor]), 103);
        End;

        // Reallocate into smaller space   if Kron was successful

        If (NewZ<>Nil) and (NewYC<>Nil) Then Begin

            NewYC.Invert;  // Back to Y

            Numphases :=NewZ.order;

            // Get rid of Z and YC and replace
            Z.Free;
            YC.Free;

            Z  := NewZ;
            YC := NewYC;

            FNeutralConductor := 0;
            ReduceByKron := FALSE;

            {Change Property values to reflect Kron reduction for save circuit function}
             PropertyValue[1] := Format('%d', [FnPhases]);
             PropertyValue[9] := get_Rmatrix;
             PropertyValue[10] := get_Xmatrix;
             PropertyValue[11] := get_Cmatrix;

        End Else Begin
           DoSimpleMsg(Format('Kron Reduction failed: LineCode.%s. Attempting to eliminate Neutral Conductor %d.', [Name, FNeutralConductor]), 103);
        End;

   End Else Begin
       DoSimpleMsg('Cannot perform Kron Reduction on a 1-phase LineCode: LineCode.'+Name, 103);
   End;;

end;



end.
