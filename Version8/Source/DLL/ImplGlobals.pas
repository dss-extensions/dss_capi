unit ImplGlobals;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
// Globals for the implementation of the interface
{ Change Log
 8-14-99 Added DSSProgress
 8-??-00  Added Settings,Lines etc.

}
interface

uses
    ImplBus,
    ImplCircuit,
    ImplCktElement,
    ImplDSSElement,
    ImplError,
    ImplDSS,
    ImplSolution,
    ImplText,
    ImplDSSProperty,
    ImplGenerators,
    ImplMonitors,
    ImplMeters,
    ImplDSSProgress,
    ImplSettings,
    ImplLines,
    ImplCtrlQueue,
    ImplLoads,
    ImplActiveClass,
    ImplTransformers,
    ImplCapacitors,
    ImplSwtControls,
    ImplCapControls,
    ImplRegControls,
    ImplTopology,
    ImplDSS_Executive,
    ImplEvents,
    ImplSensors,
    ImplXYCurves,
    ImplPDElements,
    ImplReclosers,
    ImplRelays,
    ImplCmathLib,
    ImplParser,
    ImplLoadShapes,
    ImplFuses,
    ImplIsources,
    DSSClass,
    DSSClassDefs,
    ImplDSSimComs,  //Declares the existance of the class
    ImplPVSystems,
    ImplVsources,
    ImplParallel,
    ImplLineCodes,
    ImplGICSources,
    ImplReduce,
    OpenDSSengine_TLB;

var

{ Vars for Interfaces }
    Ftext: IText;
    FCircuit: ICircuit;
    FBus: IBus;
    FCktElement: ICktElement;
    FDSSElement: IDSSElement;
    FError: IError;
    FSolution: ISolution;
    FDSS: IDSS;
    FDSSProperty: IDSSProperty;
    FGenerators: IGenerators;
    FMonitors: IMonitors;
    FMeters: IMeters;
    FDSSProgress: IDSSProgress;
    FSettings: ISettings;
    FLines: ILines;
    FCtrlQueue: ICtrlQueue;
    FLoads: ILoads;
    FActiveClass: IActiveClass;
    FCapacitors: ICapacitors;
    FTransformers: ITransformers;
    FSwtControls: ISwtControls;
    FCapControls: ICapControls;
    FRegcontrols: IRegControls;
    FTopology: ITopology;
    FDSS_Executive: IDSS_Executive;
    FEvents: IDSSEvents;
    FSensors: ISensors;
    FXYcurves: IXYcurves;
    FPDElements: IPDElements;
    FReclosers: IReclosers;
    FRelays: IRelays;
    FCmathLib: ICmathLib;
    FParser: IParser;
    FLoadShapes: ILoadShapes;
    FFuses: IFuses;
    FIsources: IIsources;
    FDSSim_Coms: IDSSimComs; //Added 07-2015 DM
    FPVSystems: IPVSystems; // Added 08-2015
    FVsources: IVsources;
    FParallel: IParallel;
    FLineCodes: ILineCodes;
    FGICSources: IGICSources;
    FreduceCkt: IReduceCkt;

    FPropIndex: Integer;
    FPropClass: TDSSClass;

    FIntfInitialized: Boolean;


procedure InitializeInterfaces;

{Special Interface Routines for Text Interface w/o COM}
procedure DSS_PutCommand(S: pAnsichar); STDCALL;
procedure DSS_GetResult(R: pAnsichar); STDCALL; // Returns a pointer to global result String

// fire COM events using these functions from DSS code
procedure Fire_InitControls;
procedure Fire_StepControls;
procedure Fire_CheckControls;

implementation

uses
    DSSGlobals,
    Executive,
    sysutils;

procedure Fire_InitControls;
begin
    if assigned(FEvents) then
        TDSSEvents(FEvents).Fire_InitControls;
end;

procedure Fire_StepControls;
begin
    if assigned(FEvents) then
        TDSSEvents(FEvents).Fire_StepControls;
end;

procedure Fire_CheckControls;
begin
    if assigned(FEvents) then
        TDSSEvents(FEvents).Fire_CheckControls;
end;

procedure InitializeInterfaces;

begin
   // Create the references to internal interfaces so we can return them
   // Called from Main DLL

    FBus := TBus.Create;
    FCircuit := TCircuit.Create;
    FCktElement := TCktElement.Create;
    FDSSElement := TDSSElement.Create;
    FText := TText.Create;
    FSolution := TSolution.Create;
    FDSSProperty := TDSSProperty.Create;
    FError := TError.Create;

    FGenerators := TGenerators.Create;
    FMonitors := TMonitors.Create;
    FMeters := TMeters.Create;
    FDSSProgress := TDSSProgress.Create;
    FSettings := TSettings.Create;
    FLines := TLines.Create;
    FCtrlQueue := TCtrlQueue.Create;
    FLoads := TLoads.Create;
    FActiveClass := TActiveClass.Create;
    FCapacitors := TCapacitors.Create;
    FTransformers := TTransformers.Create;
    FSwtControls := TSwtControls.Create;
    FCapControls := TCapControls.Create;
    FRegcontrols := TRegControls.Create;
    FTopology := TTopology.Create;
    FDSS_Executive := TDSS_Executive.Create;
    FEvents := TDSSEvents.Create;
    FSensors := TSensors.Create;
    FXYCurves := TXYCurves.Create;
    FPDElements := TPDElements.Create;
    FReclosers := TReclosers.Create;
    FRelays := TRelays.Create;
    FCmathLib := TCmathLib.Create;
    FParser := TParser.Create;
    FLoadShapes := TLoadShapes.Create;
    FFuses := TFuses.Create;
    FIsources := TIsources.Create;
    FDSSim_Coms := TDSSimComs.Create;//Self create class
    FPVSystems := TPVSystems.Create;
    FVsources := TVsources.Create;
    FParallel := TParallel.Create;
    FLineCodes := TLineCodes.Create;
    FGICSources := TGICSources.Create;
    FReduceCkt := TReduceCkt.Create;

    FPropIndex := 0;
    FPropClass := NIL;

     {MessageDlg('Interfaces initialized', mtInformation, [mbOK], 0);}
    FIntfInitialized := TRUE;
end;


procedure DSS_PutCommand(S: pAnsichar); STDCALL;

begin
     // WriteDLLDebugFile(Format('String received: %s ', [S ]));
    DSSExecutive[ActiveActor].Command := Pchar(S);   // typecast
end;


procedure DSS_GetResult(R: pAnsiChar); STDCALL; // Returns a pointer to global result String

begin
     //  WriteDLLDebugFile(Format('Global Result: %s ', [GlobalResult]));
    StrPLCopy(R, pAnsiChar(Ansistring(GlobalResult)), Length(Globalresult));
end;


initialization

    FIntfInitialized := FALSE;

end.
