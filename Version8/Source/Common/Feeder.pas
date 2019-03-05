unit Feeder;

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  Feeder Class

   User cannot instantiate this object.  Feeders are created on the fly when
   a radial system is specified.  Feeders are created from Energymeters and are
   given the same name.


 6-24-04  Created from Isource (a simple PC Element)
 8-13-2006  Radial circuit solution removed

 Feeders get created from energy meters if Radial is set to yes and meter zones
 are already computed.  If Radial=Yes and the meterzones are reset, then the feeders
 are redefined.  If Radial is subsequently set to NO or a solution mode is used
 that doesn't utilize feeders, the get currents routines will not do anything.

 Feeders cannot be re-enabled unless the energymeter object allows them to be.

 Feeders are not saved.  This is implicit with the Energymeter saving.

}

interface

uses
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex,
    PointerLIst,
    CktElement,
    CktTree;

type
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TFeeder = class(TPCClass)
    PRIVATE
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const OtherFeederName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;
    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TFeederObj = class(TPCElement)
    PRIVATE
        SequenceList,
        ShuntList: TPointerList;

        RootElement: TDSSCktElement;
        FromTerminalOffset: Integer;

    PUBLIC

        IsSynched: Boolean;

        procedure InitializeFeeder(const BranchList: TCktTree; ActorID: Integer);
        procedure SetCktElementFeederFlags(Value: Boolean);

        constructor Create(ParClass: TDSSClass; const MeterName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model  - N/A

        function InjCurrents(ActorID: Integer): Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

    end;

var
    ActiveFeederObj: TFeederObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


uses
    ParserDel,
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    Sysutils,
    Command,
    Energymeter,
    PDElement;

var
    NumPropsThisClass: Integer;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TFeeder.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'Feeder';
    DSSClassType := FEEDER_ELEMENT; {+ PC_ELEMENT; } // add to PCElement list

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TFeeder.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TFeeder.DefineProperties;
begin
    NumPropsThisClass := 0;

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

// Can't Think of any properties we want the user to be able to set

     // Define Property names
//     PropertyName[1] := 'bus1';

     // define Property help values
//     PropertyHelp[1] := 'Name of bus to which source is connected.'+CRLF+'bus1=busname'+CRLF+'bus1=busname.1.2.3';


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TFeeder.NewObject(const ObjName: String): Integer;

// Called from EnergyMeter

var
    obj: TFeederObj;

begin
    //Make a new Feeder object
    // First see if this one already exists. If so, just reinitialize
    Obj := Find(ObjName);
    with ActiveCircuit[ActiveActor] do
        if Obj <> NIL then
        begin
            ActiveCktElement := Obj;
            Result := 0;
        end
        else
        begin
            ActiveCktElement := TFeederObj.Create(Self, ObjName);
            Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
            ActiveCircuit[ActiveActor].AddCktElement(Result);
      // done here because feeder objects are instantiated from energy meters
        end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TFeeder.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName,
    Param: String;

begin
  // continue parsing with contents of Parser
    ActiveFeederObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveFeederObj;

    Result := 0;

    with ActiveFeederObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 630);

            else
                ClassEdit(ActiveFeederObj, ParamPointer - NumPropsThisClass)
            end;

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        RecalcElementData(ActorID);
        YprimInvalid[ActorID] := TRUE;
    end;

end;

//----------------------------------------------------------------------------
function TFeeder.MakeLike(const OtherFeederName: String): Integer;
var
    OtherFeeder: TFeederObj;
    i: Integer;

begin
    Result := 0;
   {See if we can find this name in the present collection}
    OtherFeeder := Find(OtherFeederName);
    if OtherFeeder <> NIL then
        with ActiveFeederObj do
        begin

            if Fnphases <> OtherFeeder.Fnphases then
            begin
                Nphases := OtherFeeder.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff

                Yorder := Fnconds * Fnterms;
                YprimInvalid[ActiveActor] := TRUE;
            end;

// Put properties to copy here

            ClassMakeLike(OtherFeeder); // set spectrum,  base frequency

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherFeeder.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in Feeder MakeLike: "' + OtherFeederName + '" Not Found.', 631);

end;

//----------------------------------------------------------------------------
function TFeeder.Init(Handle: Integer; ActorID: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TFeeder.Init', -1);
    Result := 0;
end;

//----------------------------------------------------------------------------
constructor TFeederObj.Create(ParClass: TDSSClass; const MeterName: String);


begin
    inherited create(ParClass);

    Name := LowerCase(MeterName);
    DSSObjType := ParClass.DSSClassType; // This will be a current source (PCElement)

    SequenceList := TPointerList.Create(50);
    ShuntList := TPointerList.Create(50);

    IsSynched := FALSE;

     // Bus names and Nphases, etc are set up from EnergyMeter

     // Ready to rock 'n roll

    RecalcElementData(ActiveActor);
    InitPropertyValues(0);

end;


//----------------------------------------------------------------------------
destructor TFeederObj.Destroy;
begin
    SequenceList.Free;
    ShuntList.Free;
    inherited Destroy;
end;

procedure TFeederObj.InitializeFeeder(const BranchList: TCktTree; ActorID: Integer);
var
    i, bref: Integer;
    pElement, pShunt: TDSSCktElement;

begin
    SequenceList.Clear;  // Get rid of any previous definitions
    ShuntList.Clear;

    IsSynched := FALSE;
   // Now set up Feeder terminals and BusRef to match the from node of the first branch
    if BranchList <> NIL then
    begin
        RootElement := BranchList.First;

        Nphases := RootElement.NPhases; // Take care of allocating Terminal stuff
        Fnconds := RootElement.NConds;
        Nterms := 1;
        Yorder := Fnterms * Fnconds;

        Terminals^[1].BusRef := BranchList.PresentBranch.FromBusReference;
        SetBus(1, RootElement.GetBus(BranchList.Presentbranch.FromTerminal));  // set bus name same as first element
        FromTerminalOffset := (BranchList.Presentbranch.FromTerminal - 1) * FNconds;
        SetNodeRef(1, @RootElement.Noderef^[1 + FromTerminalOffset]);

       // Build The Sequence List  and ShuntList
        pElement := RootElement;
        while pElement <> NIL do
        begin
            SequenceList.Add(pElement);

           // Mark all the To buses for this branch as radial buses
            BranchList.PresentBranch.ResetToBusList;  // reset pointer to first to bus
            for i := 1 to pElement.NTerms - 1 do
            begin
                bref := BranchList.PresentBranch.ToBusReference; // each call pops off a new one
                if bref > 0 then
                    ActiveCircuit[ActorID].Buses^[bref].IsRadialBus := TRUE;
            end;

            pShunt := BranchList.PresentBranch.FirstShuntObject;
            while pShunt <> NIL do
            begin
                ShuntList.Add(pShunt);
                pShunt := BranchList.PresentBranch.NextShuntObject;
            end;
            pElement := BranchList.GoForward;
        end;

        IsSynched := TRUE;

        SetCktElementFeederFlags(TRUE);

    end;  {If BranchList <> Nil}
end;

//----------------------------------------------------------------------------
procedure TFeederObj.RecalcElementData(ActorID: Integer);


begin

     {Nothing to Do?? - Maybe remake bus lists}


end;

//----------------------------------------------------------------------------
procedure TFeederObj.CalcYPrim(ActorID: Integer);


begin

// For now, YPrim is null

 // Build only YPrim Series
    if YprimInvalid[ActorID] then
    begin
        if YPrim_Series <> NIL then
            YPrim_Series.Free;
        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        if YPrim <> NIL then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Series.Clear;
        YPrim.Clear;
    end;


     {Yprim = 0  for Ideal Current Source;  just leave it zeroed}

     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
    inherited CalcYPrim(ActorID);

    YprimInvalid[ActorID] := FALSE;

end;


function TFeederObj.InjCurrents(ActorID: Integer): Integer;

{Sum Currents directly into solution array}

{ This is where we do the backward Sweep - computing the currents from the present voltages}

begin


  // old implementation deleted.

    Result := 0;

end;

procedure TFeederObj.GetCurrents(Curr: pComplexArray; ActorID: Integer);

{Total currents into a feeder which are equal to the currents into the first element}
{Return the currents in the From terminal of the first element in the sequence list}

var
    i: Integer;
//   cBuffer:pComplexArray;
//   pElem :TCktElement;

begin
   // If the feeder exists and we switch away from radial solution we don' want
   // to report a current
   // Do this only if doing a radial solution
(*   If ActiveCircuit[ActiveActor].RadialSolution Then
   Begin
     TRY
       pElem :=  TCktElement(SequenceList.Get(1));
       Getmem(cBuffer, Sizeof(cBuffer^[1])*pElem.Yorder );
       pElem.GetCurrents(cBuffer);   // get all currents in first element in sequence list

      // Return only FROM terminal current
       FOR i := 1 TO Yorder DO Curr^[i] := cBuffer^[i+FromTerminalOffset];

       Freemem(cBuffer); // dump temp buffer

    EXCEPT
      On E: Exception
      Do DoErrorMsg(('GetCurrents for Feeder Element: ' + Name + '.'), E.Message,
        'Inadequate storage allotted for circuit element?', 632);
    End;
  End Else
  *)

    for i := 1 to Yorder do
        Curr^[i] := CZERO; // no contribution if not radial solution


end;


procedure TFeederObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);

{Fill Up an array of injection currents}

{Only thing this is used for is for GetCurrents.  Ignore for Feeder}


begin

    with ActiveCircuit[ActorID].solution do
    begin

         {**** Do Nothing!}

    end;
end;

procedure TFeederObj.DumpProperties(var F: TextFile; Complete: Boolean);


begin
    inherited DumpProperties(F, Complete);

  {Do Not dump any properties for a Feeder unless Debug}
  (*  With ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;
  *)

    if Complete then
    begin
    {Dump sequence lists, etc here...}
        Writeln(F);
        Writeln(F);
    end;

end;

procedure TFeederObj.InitPropertyValues(ArrayOffset: Integer);
begin

  //   PropertyValue[1]  := GetBus(1);


    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TFeederObj.MakePosSequence(ActorID: Integer);
begin
 { Do Nothing
  If Fnphases>1 Then
  Begin
     Parser.CmdString := 'phases=1';
     Edit;
  End;
  inherited;
 }
end;

procedure TFeederObj.SetCktElementFeederFlags(Value: Boolean);

var
    i: Integer;

begin
    for i := 1 to ShuntList.ListSize do
    begin
        TDSSCktElement(ShuntList.Get(i)).IsPartofFeeder := Value;
    end;

    for i := 1 to SequenceList.ListSize do
    begin
        TDSSCktElement(SequenceList.Get(i)).IsPartofFeeder := Value;
    end;

end;


end.
