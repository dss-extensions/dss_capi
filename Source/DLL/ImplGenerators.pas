unit ImplGenerators;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
  1-12-00  Modified first..Next to reurn only enabled generators
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TGenerators = class(TAutoObject, IGenerators)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_RegisterNames: OleVariant; safecall;
    function Get_RegisterValues: OleVariant; safecall;
    function Get_ForcedON: WordBool; safecall;
    procedure Set_ForcedON(Value: WordBool); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_kV: Double; safecall;
    function Get_kvar: Double; safecall;
    function Get_kW: Double; safecall;
    function Get_PF: Double; safecall;
    function Get_Phases: Integer; safecall;
    procedure Set_kV(Value: Double); safecall;
    procedure Set_kvar(Value: Double); safecall;
    procedure Set_kW(Value: Double); safecall;
    procedure Set_PF(Value: Double); safecall;
    procedure Set_Phases(Value: Integer); safecall;
    function Get_Count: Integer; safecall;
    function Get_idx: Integer; safecall;
    procedure Set_idx(Value: Integer); safecall;
    function Get_Model: Integer; safecall;
    procedure Set_Model(Value: Integer); safecall;
    function Get_kVArated: Double; safecall;
    procedure Set_kVArated(Value: Double); safecall;
    function Get_Vmaxpu: Double; safecall;
    function Get_Vminpu: Double; safecall;
    procedure Set_Vmaxpu(Value: Double); safecall;
    procedure Set_Vminpu(Value: Double); safecall;
    { Protected declarations }
  end;

implementation

uses ComServ,
     DSSCLassDefs,
     DSSGlobals,
     Generator,
     CktElement,
     SysUtils,
     Variants;

Function IsGenerator(Const CktElem:TDSSCktElement):Boolean;

Begin
      Result := ((CktElem.DssObjtype AND CLASSMASK) = GEN_ELEMENT);
      If Not Result THEN
       DoSimpleMsg('GENERATOR Type Expected, but another found. Dss Class=' + CktElem.DSSClassName + CRLF +
       'Element name='+ CktElem.Name, 5002) ;
END;

function TGenerators.Get_AllNames: OleVariant;
Var
  GenElem:TGeneratorObj;
  k:Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     If Generators.ListSize>0 Then
     Begin
       VarArrayRedim(result, Generators.ListSize-1);
       k:=0;
       GenElem := Generators.First;
       WHILE GenElem<>Nil DO  Begin
          Result[k] := GenElem.Name;
          Inc(k);
          GenElem := Generators.Next;
       End;
     End;
end;


function TGenerators.Get_First: Integer;
Var
   pGen:TGeneratorObj;

Begin

   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pGen := ActiveCircuit[ActiveActor].Generators.First;
        If pGen <> Nil Then
        Begin
          Repeat
            If pGen.Enabled
            Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pGen;
              Result := 1;
            End
            Else pGen := ActiveCircuit[ActiveActor].Generators.Next;
          Until (Result = 1) or (pGen = nil);
        End
        Else
            Result := 0;  // signify no more
   End;
end;

function TGenerators.Get_Name: WideString;

Var
   pGen:TGeneratorObj;

Begin
   Result := '';
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pGen := ActiveCircuit[ActiveActor].Generators.Active;
        If pGen <> Nil Then
        Begin
          Result := pGen.Name;
        End
        Else
            Result := '';  // signify no name
   End;

end;

function TGenerators.Get_Next: Integer;
Var
   pGen:TGeneratorObj;

Begin

   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pGen := ActiveCircuit[ActiveActor].Generators.Next;
        If pGen <> Nil Then
        Begin
          Repeat
            If pGen.Enabled
            Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pGen;
              Result := ActiveCircuit[ActiveActor].Generators.ActiveIndex;
            End
            Else pGen := ActiveCircuit[ActiveActor].Generators.Next;
          Until (Result > 0) or (pGen = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TGenerators.Get_RegisterNames: OleVariant;
Var
    GeneratorClass:TGenerator;
    k :integer;

Begin
    GeneratorClass := DssClassList[ActiveActor].Get(Classnames[ActiveActor].Find('Generator'));
    Result := VarArrayCreate([0, NumGenRegisters - 1], varOleStr);
    For k := 0 to  NumGenRegisters - 1  Do Begin
       Result[k] := GeneratorClass.RegisterNames[k + 1];
    End;

end;

function TGenerators.Get_RegisterValues: OleVariant;
Var
   Gen :TGeneratorObj;
   k     :Integer;
Begin

   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        Gen :=  TGeneratorObj(ActiveCircuit[ActiveActor].Generators.Active);
        If Gen <> Nil Then
        Begin
            Result := VarArrayCreate([0, numGenRegisters-1], varDouble);
            FOR k := 0 to numGenRegisters-1 DO
            Begin
                Result[k] := Gen.Registers[k+1];
            End;
        End
        Else
            Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End;


end;

function TGenerators.Get_ForcedON: WordBool;
begin
     Result := FALSE;
     IF ActiveCircuit[ActiveActor]<> NIL
     THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0
             THEN Begin
                 Result := TGeneratorObj(Active).ForcedON;
             End;
         End;
     End;
end;

procedure TGenerators.Set_ForcedON(Value: WordBool);

begin

     IF ActiveCircuit[ActiveActor]<> NIL
     THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0
             THEN Begin
                 TGeneratorObj(Active).ForcedON := Value;
             End;
         End;
     End;

end;

procedure TGenerators.Set_Name(const Value: WideString);

VAR
    activesave :integer;
    Gen:TGeneratorObj;
    S: String;
    Found :Boolean;
Begin


  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin      // Search list of generators in active circuit for name
       WITH ActiveCircuit[ActiveActor].Generators DO
         Begin
             S := Value;  // Convert to Pascal String
             Found := FALSE;
             ActiveSave := ActiveIndex;
             Gen := First;
             While Gen <> NIL Do
             Begin
                IF (CompareText(Gen.Name, S) = 0)
                THEN Begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := Gen;
                    Found := TRUE;
                    Break;
                End;
                Gen := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('Generator "'+S+'" Not Found in Active Circuit.', 5003);
                 Gen := Get(ActiveSave);    // Restore active generator
                 ActiveCircuit[ActiveActor].ActiveCktElement := Gen;
             End;
         End;
  End;
end;

function TGenerators.Get_kV: Double;
begin
   Result := -1.0;  // not set
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).GenVars.kVGeneratorBase;
             End;
         End;
   End;
end;

function TGenerators.Get_kvar: Double;
begin
  Result := 0.0;
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).Presentkvar;
             End;
         End;
   End;
end;

function TGenerators.Get_kW: Double;
begin
   Result := 0.0;
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).PresentkW;
             End;
         End;
   End;
end;

function TGenerators.Get_PF: Double;
begin
   Result := 0.0;
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).PowerFactor;
             End;
         End;
   End;
end;

function TGenerators.Get_Phases: Integer;
begin
   Result := 0;  // not set
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).nphases;
             End;
         End;
   End;
end;

procedure TGenerators.Set_kV(Value: Double);
begin

   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).PresentkV := Value;
             End;
         End;
   End;

end;

procedure TGenerators.Set_kvar(Value: Double);
begin
    IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).Presentkvar := Value;
             End;
         End;
   End;
end;

procedure TGenerators.Set_kW(Value: Double);
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).PresentkW := Value;
             End;
         End;
   End;
end;

procedure TGenerators.Set_PF(Value: Double);
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).PowerFactor := Value;
             End;
         End;
   End;
end;

procedure TGenerators.Set_Phases(Value: Integer);
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
       WITH ActiveCircuit[ActiveActor].Generators Do Begin
           IF ActiveIndex<>0 THEN Begin
                TGeneratorObj(Active).Nphases := Value;
           End;
       End;
   End;
end;

function TGenerators.Get_Count: Integer;
begin
    If Assigned(ActiveCircuit[ActiveActor]) Then
          Result := ActiveCircuit[ActiveActor].Generators.ListSize;
end;

function TGenerators.Get_idx: Integer;
begin
    if ActiveCircuit[ActiveActor] <> Nil then
       Result := ActiveCircuit[ActiveActor].Generators.ActiveIndex
    else Result := 0;
end;

procedure TGenerators.Set_idx(Value: Integer);
Var
    pGen:TGeneratorObj;
begin
    if ActiveCircuit[ActiveActor] <> Nil then   Begin
        pGen := ActiveCircuit[ActiveActor].Generators.Get(Value);
        If pGen <> Nil Then ActiveCircuit[ActiveActor].ActiveCktElement := pGen;
    End;

end;

function TGenerators.Get_Model: Integer;
begin
   Result := -1;
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).GenModel ;
             End;
         End;
   End;
end;

procedure TGenerators.Set_Model(Value: Integer);
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  With TGeneratorObj(Active) Do Begin
                     GenModel := Value;
                     // Handle side effect
                     If GenModel=3 Then ActiveCircuit[ActiveActor].Solution.SolutionInitialized := FALSE ;
                  End;
             End;
         End;
   End;
end;

function TGenerators.Get_kVArated: Double;
begin
   Result := -1.0;
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).Genvars.kVArating  ;
             End;
         End;
   End;
end;

procedure TGenerators.Set_kVArated(Value: Double);
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  With TGeneratorObj(Active) Do Begin
                     Genvars.kVArating  := Value;
                  End;
             End;
         End;
   End;
end;

function TGenerators.Get_Vmaxpu: Double;
begin
   Result := -1.0;
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).Vmaxpu ;
             End;
         End;
   End;
end;

function TGenerators.Get_Vminpu: Double;
begin
   Result := -1.0;
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).Vminpu ;
             End;
         End;
   End;
end;

procedure TGenerators.Set_Vmaxpu(Value: Double);
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  With TGeneratorObj(Active) Do Begin
                     VMaxPu  := Value;
                  End;
             End;
         End;
   End;
end;

procedure TGenerators.Set_Vminpu(Value: Double);
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  With TGeneratorObj(Active) Do Begin
                     VMinPu  := Value;
                  End;
             End;
         End;
   End;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TGenerators, Class_Generators,
    ciInternal, tmApartment);
end.
