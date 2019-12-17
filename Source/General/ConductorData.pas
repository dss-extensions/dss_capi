unit ConductorData;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2020, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{The ConductorData object is a general DSS object used by all circuits
 as a reference for obtaining line impedances.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the ConductorData Class.
 This sets the active ConductorData object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.
 }

USES
   Command, DSSClass, DSSObject, ArrayDef;

TYPE
  ConductorChoice = (Overhead, ConcentricNeutral, TapeShield, Unknown);

  ConductorChoiceArray = Array[1..100] of ConductorChoice;
  pConductorChoiceArray = ^ConductorChoiceArray;

  TConductorData = class(TDSSClass)
    private

    Protected
      Procedure CountProperties;
      Procedure DefineProperties;
      Function ClassEdit(Const ActiveObj:Pointer; Const ParamPointer:Integer):Integer;
      Procedure ClassMakeLike(Const OtherObj:Pointer);
    public
      NumConductorClassProps: Integer;
      constructor Create;
      destructor Destroy; override;
  end;

  TConductorDataObj = class(TDSSObject)
    private
      FRDC              : Double;
      FR60              : Double;
      FGMR60            : Double;
      Fcapradius60      : Double;  // in case it is different than radius for cap calcs
      Fradius           : Double;
      FGMRUnits         : Integer;
      FResistanceUnits  : Integer;
      FRadiusUnits      : Integer;
    public
      NormAmps          : Double;
      EmergAmps         : Double;
      NumAmpRatings     : Integer;
      AmpRatings        : TRatingsArray;

      constructor Create(ParClass:TDSSClass; const ConductorDataName:String);
      destructor Destroy; override;

      Property Rdc:Double Read FRDC;
      Property Rac:Double Read FR60;
      Property GMR:Double Read FGMR60;
      Property CapRadius:Double Read Fcapradius60;
      Property Radius:Double Read FRadius;
      Property ResUnits:Integer Read FresistanceUnits;
      Property RadiusUnits:Integer Read FradiusUnits;
      Property GMRUnits:Integer Read FGMRUnits;

      PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
      PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;
    end;

   TConductorDataArray = Array[1..100] of TConductorDataObj;
   pConductorDataArray = ^TConductorDataArray;
VAR
   ActiveConductorDataObj:TConductorDataObj;

implementation

USES  ParserDel,  DSSGlobals, DSSClassDefs, Sysutils, Ucomplex,  LineUNits, Utilities;

Const
  LineUnitsHelp = '{mi|kft|km|m|Ft|in|cm|mm} Default=none.';

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TConductorData.Create;  // Creates superstructure for all Line objects
BEGIN
  Inherited Create;
  NumConductorClassProps := 13;
  DSSClassType := DSS_OBJECT;
END;

Destructor TConductorData.Destroy;
BEGIN
  Inherited Destroy;
END;

Procedure TConductorData.CountProperties;
Begin
  NumProperties := NumProperties + NumConductorClassProps;
  Inherited CountProperties;
End;

Procedure TConductorData.DefineProperties;
Begin
  PropertyName^[ActiveProperty + 1] := 'Rdc';
  PropertyName^[ActiveProperty + 2] := 'Rac';
  PropertyName^[ActiveProperty + 3] := 'Runits';
  PropertyName^[ActiveProperty + 4] := 'GMRac';
  PropertyName^[ActiveProperty + 5] := 'GMRunits';
  PropertyName^[ActiveProperty + 6] := 'radius';
  PropertyName^[ActiveProperty + 7] := 'radunits';
  PropertyName^[ActiveProperty + 8] := 'normamps';
  PropertyName^[ActiveProperty + 9] := 'emergamps';
  PropertyName^[ActiveProperty + 10]:= 'diam';
  PropertyName^[ActiveProperty + 11]:= 'Seasons';
  PropertyName^[ActiveProperty + 12]:= 'Ratings';
  PropertyName^[ActiveProperty + 13]:= 'Capradius';

  PropertyHelp^[ActiveProperty + 1] := 'dc Resistance, ohms per unit length (see Runits). Defaults to Rac/1.02 if not specified.';
  PropertyHelp^[ActiveProperty + 2] := 'Resistance at 60 Hz per unit length. Defaults to 1.02*Rdc if not specified.';
  PropertyHelp^[ActiveProperty + 3] := 'Length units for resistance: ohms per ' + LineUnitsHelp;
  PropertyHelp^[ActiveProperty + 4] := 'GMR at 60 Hz. Defaults to .7788*radius if not specified.';
  PropertyHelp^[ActiveProperty + 5] := 'Units for GMR: ' + LineUnitsHelp;
  PropertyHelp^[ActiveProperty + 6] := 'Outside radius of conductor. Defaults to GMR/0.7788 if not specified.';
  PropertyHelp^[ActiveProperty + 7] := 'Units for outside radius: ' + LineUnitsHelp;
  PropertyHelp^[ActiveProperty + 8] := 'Normal ampacity, amperes. Defaults to Emergency amps/1.5 if not specified.';
  PropertyHelp^[ActiveProperty + 9] := 'Emergency ampacity, amperes. Defaults to 1.5 * Normal Amps if not specified.';
  PropertyHelp^[ActiveProperty + 10]:= 'Diameter; Alternative method for entering radius.';
  PropertyHelp^[ActiveProperty + 11]:= 'Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the "Ratings" property.';
  PropertyHelp^[ActiveProperty + 12]:= 'An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert' +
                                        CRLF + 'multiple ratings to change during a QSTS simulation to evaluate different ratings in lines.';
  PropertyHelp^[ActiveProperty + 13]:= 'Equivalent conductor radius for capacitor calcs. Specify this for bundled conductors. Defaults to same value as radius.';

  ActiveProperty := ActiveProperty + NumConductorClassProps;
  Inherited DefineProperties;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TConductorData.ClassEdit (Const ActiveObj:Pointer; Const ParamPointer:Integer):Integer;
var
  Param : String;
BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  If ParamPointer > 0 Then
    WITH TConductorDataObj(ActiveObj) DO BEGIN
      CASE ParamPointer OF
        1:  FRDC             :=  Parser[ActiveActor].Dblvalue;
        2:  FR60             :=  Parser[ActiveActor].DblValue;
        3:  FresistanceUnits :=  GetUnitsCode(Parser[ActiveActor].StrValue);
        4:  FGMR60           :=  Parser[ActiveActor].DblValue;
        5:  FGMRUnits        :=  GetUnitsCode(Parser[ActiveActor].StrValue);
        6:  Fradius          :=  Parser[ActiveActor].DblValue;
        7:  FRadiusUnits     :=  GetUnitsCode(Parser[ActiveActor].StrValue);
        8:  NormAmps         :=  Parser[ActiveActor].DblValue ;
        9:  EmergAmps        :=  Parser[ActiveActor].DblValue ;
       10:  Fradius          :=  Parser[ActiveActor].DblValue / 2.0;
       11:  Begin
              NumAmpRatings         :=  Parser[ActiveActor].IntValue;
              setlength(AmpRatings,NumAmpRatings);
            End;
       12:  Begin
              setlength(AmpRatings,NumAmpRatings);
              Param := Parser[ActiveActor].StrValue;
              NumAmpRatings := InterpretDblArray(Param, NumAmpRatings, pointer(AmpRatings));
            End;
       13: Fcapradius60        := Parser[ActiveActor].DblValue;
      ELSE
        Inherited ClassEdit(ActiveObj, ParamPointer - NumConductorClassProps)
      END;
      {Set defaults}
      CASE ParamPointer OF
        1: If FR60<0.0        Then FR60         := 1.02* FRDC;
        2: If FRDC<0.0        Then FRDC         := FR60 / 1.02;
        4: If Fradius<0.0     Then Fradius      := FGMR60 / 0.7788;
        5: If FradiusUnits =0 Then FradiusUnits := FGMRunits;
        6: Begin
             If FGMR60<0.0    Then FGMR60 := 0.7788 * FRadius;
             if Fcapradius60<0.0 then Fcapradius60 := Fradius;    // default to radius
           End;
        7: If FGMRUnits=0   Then FGMRunits := FradiusUnits;
        8: IF EmergAmps<0.0 Then EmergAmps := 1.5*NormAmps;
        9: If NormAmps<0.0  Then NormAmps := EmergAmps/1.5;
       10: Begin
              If FGMR60<0.0    Then FGMR60 := 0.7788 * FRadius;
              if Fcapradius60<0.0 then Fcapradius60 := Fradius;    // default to radius
           End;
       13: If Fcapradius60<0.0    Then Fcapradius60 := FRadius;
      END;
      {Check for critical errors}
      CASE ParamPointer OF
        4: If (Fradius = 0.0)  Then DoSimpleMsg('Error: Radius is specified as zero for ConductorData.' + Name,999);
        6: If (FGMR60 = 0.0)   Then DoSimpleMsg('Error: GMR is specified as zero for ConductorData.' + Name,999);
      END;
    End;
END;

Procedure TConductorData.ClassMakeLike(Const OtherObj:Pointer);
VAR
  OtherConductorData:TConductorDataObj;
BEGIN
  OtherConductorData := TConductorDataObj(OtherObj);
  WITH TConductorDataObj(ActiveDSSObject[ActiveActor]) DO BEGIN
    FRDC              := OtherConductorData.FRDC;
    FR60              := OtherConductorData.FR60;
    FResistanceUnits  := OtherConductorData.FResistanceUnits;
    FGMR60            := OtherConductorData.FGMR60;
    Fcapradius60      := OtherConductorData.Fcapradius60;
    FGMRUnits         := OtherConductorData.FGMRUnits;
    FRadius           := OtherConductorData.FRadius;
    FRadiusUnits      := OtherConductorData.FRadiusUnits;
    NormAmps          := OtherConductorData.NormAmps;
    EmergAmps         := OtherConductorData.EmergAmps;
  END;
  // Inherited ClassMakeLike(OtherObj);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TConductorData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TConductorDataObj.Create(ParClass:TDSSClass; const ConductorDataName:String);

BEGIN
  Inherited Create(ParClass);
  Name := LowerCase(ConductorDataName);
  DSSObjType := ParClass.DSSClassType;

  FRDC              := -1.0;
  FR60              := -1.0;
  FGMR60            := -1.0;
  Fradius           := -1.0;
  Fcapradius60      := -1.0;   // init to not defined
  FGMRUnits         := 0;
  FResistanceUnits  := 0;
  FRadiusUnits      := 0;
  Normamps    := -1.0;
  EmergAmps   := -1.0;
  {Initialize dynamic array for ratings}
  NumAmpRatings    :=  1;
  setlength(AmpRatings,NumAmpRatings);
  AmpRatings[0]  :=  NormAmps;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TConductorDataObj.Destroy;
BEGIN
  Inherited destroy;
END;

PROCEDURE TConductorDataObj.DumpProperties(var F: TextFile; Complete: Boolean);
Var
  j,
  i       : Integer;
  TempStr : String;
Begin
  Inherited DumpProperties(F, Complete);
  WITH ParentClass Do Begin
    For i := 1 to NumProperties Do Begin
      Write(F,'~ ',PropertyName^[i],'=');
      Case i of
        1 : Writeln(F, Format('%.6g',[FRDC]));
        2 : Writeln(F, Format('%.6g',[FR60]));
        3 : Writeln(F, Format('%s',[LineUnitsStr(FresistanceUnits)]));
        4 : Writeln(F, Format('%.6g',[FGMR60]));
        5 : Writeln(F, Format('%s',[LineUnitsStr(FGMRUnits)]));
        6 : Writeln(F, Format('%.6g',[Fradius]));
        7 : Writeln(F, Format('%s',[LineUnitsStr(FRadiusUnits)]));
        8 : Writeln(F, Format('%.6g',[NormAmps]));
        9 : Writeln(F, Format('%.6g',[EmergAmps]));
       10 : Writeln(F, Format('%.6g',[radius*2.0]));
       11 : Writeln(F, Format('%d',[NumAmpRatings]));
       12 : Begin
              TempStr   :=  '[';
              for  j:= 1 to NumAmpRatings do
                TempStr :=  TempStr + floattoStrf(AmpRatings[j-1],ffgeneral,8,4) + ',';
              TempStr   :=  TempStr + ']';
              Writeln(F, TempStr);
            End;
       13: Writeln(F, Format('%.6g',[Fcapradius60]));
      END;
    End;
  End;
end;

procedure TConductorDataObj.InitPropertyValues(ArrayOffset: Integer);
begin
  PropertyValue[ArrayOffset + 1] := '-1';
  PropertyValue[ArrayOffset + 2] :=  '-1';
  PropertyValue[ArrayOffset + 3] :=  'none';
  PropertyValue[ArrayOffset + 4] :=  '-1';
  PropertyValue[ArrayOffset + 5] :=  'none';
  PropertyValue[ArrayOffset + 6] :=  '-1';
  PropertyValue[ArrayOffset + 7] :=  'none';
  PropertyValue[ArrayOffset + 8] :=  '-1';
  PropertyValue[ArrayOffset + 9] :=  '-1';
  PropertyValue[ArrayOffset + 10] :=  '-1';
  PropertyValue[ArrayOffset + 11] :=  '1';
  PropertyValue[ArrayOffset + 12] :=  '[-1]';
  PropertyValue[ArrayOffset + 13] :=  '-1';
  inherited InitPropertyValues(ArrayOffset + 13);
end;

end.
