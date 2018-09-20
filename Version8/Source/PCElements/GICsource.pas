unit GICsource;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2018, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Develpoed from Isource and GICLine May 2018
}


interface

USES DSSClass, PCClass,PCElement, ucmatrix, ucomplex,  Line ;

TYPE
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TGICsource = CLASS(TPCClass)
     private
     Protected
       Procedure DefineProperties;
       Function  MakeLike(Const OtherSource:STring):Integer;Override;
     public
       constructor Create;
       destructor  Destroy; override;

       Function Edit:Integer; override;
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;
   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TGICSourceObj = class(TPCElement)
     private

        FphaseShift  :Double;
        Bus2Defined  :Boolean;

        Volts       :Double;
        Vmag        :Double;
        Angle       :Double;
        SrcFrequency:Double;
        LineName    :String;
        pLineElem   :TLineObj;  // Pointer to associated Line

        ENorth,
        EEast,
        Lat1,
        Lon1,
        Lat2,
        Lon2        :Double;
        VN, VE      :Double;  // components of vmag
        VoltsSpecified :Boolean;

        LineClass :Tline;

        Procedure GetVterminalForSource;
        Function  Compute_VLine: Double;
      public

        constructor Create(ParClass:TDSSClass; const SourceName:String);
        destructor  Destroy; override;

        Procedure RecalcElementData; Override;
        Procedure CalcYPrim; Override;

        PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model

        Function  InjCurrents:Integer; Override;
        Procedure GetInjCurrents(Curr:pComplexArray); Override;
        Procedure GetCurrents(Curr: pComplexArray);Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        Procedure DumpProperties(Var F:TextFile; Complete:Boolean); Override;
        FUNCTION  GetPropertyValue(Index:Integer):String;Override;

   End;

VAR
    ActiveGICsourceObj:TGICSourceObj;
    GICsourceClass:TGICsource;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit, DSSClassDefs, DSSGlobals, Utilities, Sysutils, Command, dynamics;

Var  NumPropsThisClass:Integer;



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TGICsource.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name := 'GICsource';
     DSSClassType := SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;

     GICsourceClass := Self;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TGICsource.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TGICsource.DefineProperties;
Begin
     NumPropsThisClass := 10;

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names
     PropertyName[1]  := 'Volts';
     PropertyName[2]  := 'angle';
     PropertyName[3]  := 'frequency';
     PropertyName[4]  := 'phases';
     PropertyName[5]  := 'EN';
     PropertyName[6]  := 'EE';
     PropertyName[7]  := 'Lat1';
     PropertyName[8]  := 'Lon1';
     PropertyName[9]  := 'Lat2';
     PropertyName[10] := 'Lon2';

     // define Property help values
     PropertyHelp[1] := 'Voltage magnitude, in volts, of the GIC voltage induced across the associated line. ' +
                        'When specified, induced voltage is assumed defined by Voltage and Angle properties. ' + CRLF+CRLF+
                        'Specify this value' + CRLF + CRLF + 'OR' + CRLF + CRLF +
                        'EN, EE, lat1, lon1, lat2, lon2. ' + CRLF + CRLF +
                        'Not both!!  Last one entered will take precedence. ' +
                        'Assumed identical in each phase of the Line object.';
     PropertyHelp[2] := 'Phase angle in degrees of first phase. Default=0.0.  See Voltage property';
     PropertyHelp[3] := 'Source frequency.  Defaults to  0.1 Hz. So GICSource=0 at power frequency.';
     PropertyHelp[4] := 'Number of phases.  Defaults to 3. All three phases are assumed in phase (zero sequence)';
     PropertyHelp[5] := 'Northward Electric field (V/km). If specified, Voltage and Angle are computed from EN, EE, lat and lon values.';
     PropertyHelp[6] := 'Eastward Electric field (V/km).  If specified, Voltage and Angle are computed from EN, EE, lat and lon values.';
     PropertyHelp[7] := 'Latitude of Bus1 (degrees)';
     PropertyHelp[8] := 'Longitude of Bus1 (degrees)';
     PropertyHelp[9] := 'Latitude of Bus2 (degrees)';
     PropertyHelp[10] := 'Longitude of Bus2 (degrees)';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
     PropertyHelp[NumPropsThisClass+1] := 'Not used.';
     PropertyHelp[NumPropsThisClass+2] := 'Not used.';

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGICsource.NewObject(const ObjName:String):Integer;
Begin
    // Make a new voltage source and add it to GICsource class list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TGICSourceObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TGICsource.Edit:Integer;
VAR
   ParamPointer :Integer;
   ParamName,
   Param        :String;

Begin
  // continue parsing with contents of Parser
  ActiveGICsourceObj            := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveGICsourceObj;

  Result := 0;

  WITH ActiveGICsourceObj DO Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param     := Parser.StrValue;
     WHILE Length(Param) > 0 DO Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer > 0) and (ParamPointer <= NumProperties) Then PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 330);
            1: Volts     := Parser.DblValue;
            2: Angle     := Parser.DblValue; // Ang
            3: SrcFrequency    := Parser.DblValue; // freq   Usually 0.1 Hz
            4: Begin
                   Nphases     := Parser.IntValue; // num phases
                   FphaseShift := 0.0;     // Zero Sequence
                   NConds      := Fnphases;  // Force Reallocation of terminal info
               End;
            5: ENorth := Parser.DblValue;
            6: EEast  := Parser.DblValue;
            7: Lat1   := Parser.DblValue;
            8: Lon1   := Parser.DblValue;
            9: Lat2   := Parser.DblValue;
           10: Lon2   := Parser.DblValue;

         ELSE
            ClassEdit(ActiveGICsourceObj, ParamPointer - NumPropsThisClass);
         End;

         CASE ParamPointer of
              1, 2:   VoltsSpecified := TRUE;
              5..10:  VoltsSpecified := FALSE;
         END;

         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

     RecalcElementData;
     YPrimInvalid := True;
  End;

End;

//----------------------------------------------------------------------------
Function TGICsource.MakeLike(Const OtherSource:String):Integer;
VAR
   OtherGICsource :TGICSourceObj;
   i :Integer;

Begin
   Result := 0;
   {See if we can find this line name in the present collection}
   OtherGICsource := Find(OtherSource);
   IF   OtherGICsource <> Nil THEN
   WITH ActiveGICsourceObj DO Begin

       IF Fnphases <> OtherGICsource.Fnphases THEN Begin
           Nphases := OtherGICsource.Fnphases;
           NConds  := Fnphases;  // Forces reallocation of terminal stuff

           Yorder := Fnconds * Fnterms;
           YPrimInvalid := True;
       End;

       Volts           := OtherGICsource.Volts;
       Angle           := OtherGICsource.Angle;
       SrcFrequency    := OtherGICsource.SrcFrequency;
       LineName        := OtherGICsource.LineName;

       ENorth          := OtherGICsource.ENorth;
       EEast           := OtherGICsource.EEast;
       Lat1            := OtherGICsource.Lat1;
       Lon1            := OtherGICsource.Lon1;
       Lat2            := OtherGICsource.Lat2;
       Lon2            := OtherGICsource.Lon2;

       Bus2Defined     := OtherGICsource.Bus2Defined;

       ClassMakeLike(OtherGICsource); // set spectrum,  base frequency

       Spectrum := '';  // Spectrum not allowed
       SpectrumObj := Nil;

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherGICsource.PropertyValue[i];
       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in GICsource MakeLike: "' + OtherSource + '" Not Found.', 332);

End;

//----------------------------------------------------------------------------
Function TGICsource.Init(Handle:Integer):Integer;

Begin
   DoSimpleMsg('Need to implement TGICsource.Init', -1);
   Result := 0;
End;


//----------------------------------------------------------------------------
Constructor TGICSourceObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin
     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType; // SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List
     LineName := Name;  // GICsource name must be same as associated Line

     LineClass := DSSClassList.Get(ClassNames.Find('Line'));
     Nphases := 3;
     Fnconds := 3;
     Nterms  := 2;   // 4/27/2018 made a 2-terminal I source

     Volts    := 0.0;
     Angle    := 0.0;

     ENorth := 1.0;
     EEast := 1.0;
     Lat1  :=  33.613499;
     Lon1  := -87.373673;
     Lat2  :=  33.547885;
     Lon2  := -86.074605;

     VoltsSpecified := FALSE;
     SrcFrequency  := 0.1;   // this is the GIC source
     FphaseShift   := 0.0;    // always zero sequence
     Bus2Defined   := FALSE;
     InitPropertyValues(0);

     Yorder := Fnterms * Fnconds;
    // Don't do This here RecalcElementData;

     Spectrum := '';  // Spectrum not allowed
     SpectrumObj := Nil;

End;


//----------------------------------------------------------------------------
Destructor TGICSourceObj.Destroy;
Begin
    LineName := '';
    Inherited Destroy;
End;

//=============================================================================
function TGICSourceObj.Compute_VLine: Double;
Var
   Phi : Double;
   DeltaLat, DeltaLon : Double;

begin
     Phi := (Lat2 + Lat1)/2.0 * (pi/180.0);   // deg to radians
     DeltaLat := Lat2 - Lat1;
     DeltaLon := Lon2 - Lon1;
     VE    := (111.133 - 0.56    * cos(2.0*phi) )* DeltaLat * ENorth;
     VN    := (111.5065 - 0.1872 * cos(2.0*phi)) * Cos(phi) * DeltaLon * EEast ;
     Result := VN + VE;
end;

//----------------------------------------------------------------------------
Procedure TGICSourceObj.RecalcElementData;

Var
   GICBus : String;
   LineBus2 : String;

Begin

      pLineElem := LineClass.Find(LineName);

      IF pLineElem=NIL Then Begin
          DoSimpleMsg('Line Object "' + LineName + '" associated with GICsource.'+Name+' Not Found. Make sure you define it first.', 333);
      End
      Else Begin

         LineBus2 := pLineElem.GetBus(2);

         // If LineBus2 already begins with GIC, Don't insert the GIC Bis

         if CompareTextShortest('GIC_', LineBus2)<>0 then
         Begin
             // Define buses -- inserting a new bus GIC_{LineName}
             GICBus := 'GIC_'+LineName;
             SetBus(1, GICBus);
             SetBus(2, LineBus2);
             // Redefine the bus2 spec for LineElem
             Parser.CmdString := 'Bus2=' + GICBus;
             pLineElem.Edit;  // invoke the Line's editor to process Parser
         End;

         Bus2Defined   := TRUE;
         If Not VoltsSpecified Then Volts := Compute_VLine;

      End;

      Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

End;

//----------------------------------------------------------------------------
Procedure TGICSourceObj.CalcYPrim;

Var Rs, Rm, Rzero : Double;
    i  : Integer;
    Value : Complex;
    NegValue : Complex;

Begin

 // Build only YPrim Series
     IF YPrimInvalid THEN Begin
       IF YPrim_Series <> nil Then YPrim_Series.Free;
       YPrim_Series := TcMatrix.CreateMatrix(Yorder);
       IF YPrim <> nil Then YPrim.Free;
       YPrim := TcMatrix.CreateMatrix(Yorder);
     End
     ELSE Begin
          YPrim_Series.Clear;
          YPrim.Clear;
     End;


     {
       Assume 0.0001 ohms resistance for GIC Source
     }
     Value := Cmplx(10000.0, 0.0);
     NegValue := Cnegate(Value);
     With YPrim_Series Do
       FOR i := 1 to Fnphases Do
       Begin
           SetElement(i,i, Value);
           SetElement(i+Fnphases,i+Fnphases, Value);
           SetElemSym(i, i+Fnphases, NegValue);
       End;

       YPrim.Copyfrom(Yprim_Series);      // Initialize YPrim for series impedances
(* ****************************************************************************
    {
       Compute R0 of associated line
       Average diagonals of Z matrix and off-diagonals
       Z0=Zs + 2 Zm
     }

      IF abs(ActiveCircuit.Solution.Frequency - SrcFrequency) < EPSILON2 THEN Begin
          Rs := 0.0;  { zero the accumulators}
          Rm := 0.0;
          With pLineElem Do
          Begin
            for i := 1 to NPhases do Begin
              Rs := Rs + Z.GetElement(i,i).re;
              for j := i+1 to NPhases  do
                  Rm := Rm + Z.GetElement(i,j).re;
            End;
            Rs := Rs / NPhases;
            Rm := Rm / (NPhases * (NPhases-1)/2);
            Rzero :=  (Rs + 2.0*Rm) * Len / UnitsConvert;  // Total for entire line
            Gzero := 1.0/Rzero; // zero-sequence conductance of line

          End;
      End;
   ***********************************************************************************
 *)

     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
     Inherited CalcYPrim;

     YPrimInvalid := False;

End;

Procedure TGICSourceObj.GetVterminalForSource;

Var
   Vmag        : Double;
   i           : Integer;

Begin

  TRY
     // If the solution frequency not 0.1 Hz, source is shorted.
      WITH ActiveCircuit.Solution Do
      Begin
           IF abs(Frequency - SrcFrequency) < EPSILON2 THEN Vmag := Volts Else Vmag := 0.0;
           For i := 1 to Fnphases do
              Begin
                Vterminal^[i] :=  pdegtocomplex(Vmag, (360.0 + Angle) );   // all the same for zero sequence
                 // bottom part of the vector is zero
                VTerminal^[i+Fnphases] := CZERO;    // See comments in GetInjCurrents
              End;
       End;

  EXCEPT
      DoSimpleMsg('Error computing current for GICsource.'+Name+'. Check specification. Aborting.', 334);
      IF In_Redirect Then Redirect_Abort := TRUE;
  END;

End;

Function TGICSourceObj.InjCurrents:Integer;

{Sum Currents directly into solution array}

Begin

  GetInjCurrents(InjCurrent);

  Result := Inherited Injcurrents;  // Adds into system array

End;

Procedure TGICSourceObj.GetCurrents(Curr: pComplexArray);

{Total currents into a device}

VAR
   i:Integer;

Begin

  TRY
   WITH    ActiveCircuit.Solution  Do
   Begin

       FOR     i := 1 TO Yorder DO  Vterminal^[i] := NodeV^[NodeRef^[i]];

       YPrim.MVMult(Curr, Vterminal);  // Current from Elements in System Y

       GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
      // Add Together  with yprim currents
       FOR i := 1 TO Yorder DO Curr^[i] := Csub(Curr^[i], ComplexBuffer^[i]);

   End;  {With}

  EXCEPT
    On E: Exception
    Do DoErrorMsg(('GetCurrents for GICsource Element: ' + Name + '.'), E.Message,
        'Inadequate storage allotted for circuit element?', 335);
  End;

End;

Procedure TGICSourceObj.GetInjCurrents(Curr:pComplexArray);

  { source injection currents given by this formula:
     _     _           _         _
     |Iinj1|           |GICLineVolts  |
     |     | = [Yprim] |         |
     |Iinj2|           | 0       |
     _     _           _         _
   }

Begin
       GetVterminalForSource;    // only at 0.1 Hz
       YPrim.MVMult(Curr, Vterminal);

       ITerminalUPdated := FALSE;
End;

function TGICSourceObj.GetPropertyValue(Index: Integer): String;
begin
    begin
        Case Index of
          1 : Result := Format('%.8g',[Volts]);
          2 : Result := Format('%.8g',[Angle]);
          3 : Result := Format('%.8g',[SrcFrequency]);
        Else
          Result := Inherited GetPropertyValue(Index);
        End;
    end;
end;

Procedure TGICSourceObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    With ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete Then Begin
        Writeln(F);
        Writeln(F);
    End;

End;

procedure TGICSourceObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := '0';
     PropertyValue[2]  := '0';
     PropertyValue[3]  := Format('%-.6g',[SrcFrequency]);
     PropertyValue[4]  := '3';
     PropertyValue[5]  := '1.0';
     PropertyValue[6]  := '1.0';
     PropertyValue[7]  := '33.613499';
     PropertyValue[8]  := '-87.373673';
     PropertyValue[9]  := '33.547885';
     PropertyValue[10] := '-86.074605';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TGICSourceObj.MakePosSequence;
begin

  If Fnphases>1 Then
  Begin
     Parser.CmdString := 'phases=1';
     Edit;
  End;
  inherited;

end;



Initialization

end.
