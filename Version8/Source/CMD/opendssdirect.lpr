library opendssdirect;

{$IFDEF Darwin}
{$linkframework CoreFoundation}
{$linkframework Carbon}
{$ENDIF}

{ ----------------------------------------------------------
  Copyright (c) 2008-2023, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
*	Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
*	Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
*	Neither the name of the Electric Power Research Institute, Inc.,
  nor the names of its contributors may be used to endorse or promote products
  derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY Electric Power Research Institute, Inc., "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL Electric Power Research Institute, Inc.,
  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
}

{
	09/18/2023  Created from OpenDSSDirect
 ----------------------------------------------------------
  Copyright (c) 2023 Battelle Memorial Institute
 ----------------------------------------------------------
}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  Classes,
  Arraydef in '..\Shared\Arraydef.pas',
  AutoAdd in '..\Common\AutoAdd.pas',
  AutoTrans in '..\PDElements\AutoTrans.pas',
  Bus in '..\Common\Bus.pas',
  CableConstants in '..\General\CableConstants.pas',
  CableData in '..\General\CableData.pas',
  Capacitor in '..\PDElements\Capacitor.pas',
  CapControl in '..\Controls\CapControl.pas',
  CapControlVars in '..\Controls\CapControlVars.pas',
  CapUserControl in '..\Controls\CapUserControl.pas',
  Circuit in '..\Common\Circuit.pas',
  CktElement in '..\Common\CktElement.pas',
  CktElementClass in '..\Common\CktElementClass.pas',
  CktTree in '..\Shared\CktTree.pas',
  CmdForms in 'CmdForms.pas',
  Command in '..\Shared\Command.pas',
  CNData in '..\General\CNData.pas',
  CNLineConstants in '..\General\CNLineConstants.pas',
  Conductor in '..\Common\Conductor.pas',
  ConductorData in '..\General\ConductorData.pas',
  ControlClass in '..\Controls\ControlClass.pas',
  ControlElem in '..\Controls\ControlElem.pas',
  ControlQueue in '..\Common\ControlQueue.pas',
  DSSCallBackRoutines in '..\Common\DSSCallBackRoutines.pas',
  DSSClass in '..\Common\DSSClass.pas',
  DSSClassDefs in '..\Common\DSSClassDefs.pas',
  DSSGlobals in '..\Common\DSSGlobals.pas',
  DSSObject in '..\General\DSSObject.pas',
  DynamicExp in '..\General\DynamicExp.pas',
  Dynamics in '..\Shared\Dynamics.pas',
  EnergyMeter in '..\Meters\EnergyMeter.pas',
  Equivalent in '..\PCElements\Equivalent.pas',
  EventQueue in '..\Common\EventQueue.pas',
  ExecCommands in '..\Executive\ExecCommands.pas',
  ExecHelper in '..\Executive\ExecHelper.pas',
  ExecOptions in '..\Executive\ExecOptions.pas',
  Executive in '..\Executive\Executive.pas',
  ExpControl in '..\Controls\ExpControl.pas',
  ExportCIMXML in '..\Common\ExportCIMXML.pas',
  ExportOptions in '..\Executive\ExportOptions.pas',
  ExportResults in '..\Common\ExportResults.pas',
  Fault in '..\PDElements\Fault.pas',
  Feeder in '..\Common\Feeder.pas',
  fuse in '..\PDElements\fuse.pas',
  UPFCControl in '..\Controls\UPFCControl.pas',
  GenDispatcher in '..\Controls\GenDispatcher.pas',
  generator in '..\PCElements\generator.pas',
  GeneratorVars in '..\PCElements\GeneratorVars.pas',
  GenUserModel in '..\PCElements\GenUserModel.pas',
  GICLine in '..\PCElements\GICLine.pas',
  GICTransformer in '..\PDElements\GICTransformer.pas',
  GISCommands in '..\GISCommands\GISCommands.pas',
  GrowthShape in '..\General\GrowthShape.pas',
  HashList in '..\Shared\HashList.pas',
  IniRegSave in '..\Shared\IniRegSave.pas',
  InvControl in '..\Controls\InvControl.pas',
  Isource in '..\PCElements\Isource.pas',
  KLUSolve in 'KLUSolve.pas',
  Line in '..\PDElements\Line.pas',
  LineCode in '..\General\LineCode.pas',
  LineConstants in '..\General\LineConstants.pas',
  LineGeometry in '..\General\LineGeometry.pas',
  LineSpacing in '..\General\LineSpacing.pas',
  LineUnits in '..\Shared\LineUnits.pas',
  Load in '..\PCElements\Load.pas',
  LoadShape in '..\General\LoadShape.pas',
  mathutil in '..\Shared\mathutil.pas',
  MemoryMap_lib in '..\Meters\MemoryMap_lib.pas',
  MeterClass in '..\Meters\MeterClass.pas',
  MeterElement in '..\Meters\MeterElement.pas',
  Monitor in '..\Meters\Monitor.pas',
  MyDSSClassDefs in 'MyDSSClassDefs.Pas',
  NamedObject in '..\General\NamedObject.pas',
  Notes in '..\Common\Notes.pas',
  NumCPULib in '..\Parallel_Lib\NumCPULib.pas',
  OHLineConstants in '..\General\OHLineConstants.pas',
  ParserDel in '..\Parser\ParserDel.pas',
  PCClass in '..\PCElements\PCClass.pas',
  PCElement in '..\PCElements\PCElement.pas',
  PDClass in '..\PDElements\PDClass.pas',
  PDElement in '..\PDElements\PDElement.pas',
  PointerList in '..\Shared\PointerList.pas',
  PriceShape in '..\General\PriceShape.pas',
  Pstcalc in '..\Shared\Pstcalc.pas',
  PVsystem in '..\PCElements\PVsystem.pas',
  PVSystemUserModel in '..\PCElements\PVSystemUserModel.pas',
  Reactor in '..\PDElements\Reactor.pas',
  Recloser in '..\Controls\Recloser.pas',
  ReduceAlgs in '..\Meters\ReduceAlgs.pas',
  RegControl in '..\Controls\RegControl.pas',
  Relay in '..\Controls\Relay.pas',
  RPN in '..\Parser\RPN.pas',
  Sensor in '..\Meters\Sensor.pas',
  ShowOptions in '..\Executive\ShowOptions.pas',
  ShowResults in '..\Common\ShowResults.pas',
  Solution in '..\Common\Solution.pas',
  SolutionAlgs in '..\Common\SolutionAlgs.pas',
  Sparse_Math in '..\Common\Sparse_Math.pas',
  Spectrum in '..\General\Spectrum.pas',
  StackDef in '..\Shared\StackDef.pas',
  Storage in '..\PCElements\Storage.pas',
  StorageController in '..\Controls\StorageController.pas',
  StorageVars in '..\PCElements\StorageVars.pas',
  StoreUserModel in '..\PCElements\StoreUserModel.pas',
  SwtControl in '..\Controls\SwtControl.pas',
  TCC_Curve in '..\General\TCC_Curve.pas',
  TCP_IP in '..\TCP_IP\TCP_IP.pas',
  TempShape in '..\General\TempShape.pas',
  Terminal in '..\Common\Terminal.pas',
  TOPExport in '..\Common\TOPExport.pas',
  Transformer in '..\PDElements\Transformer.pas',
  TSData in '..\General\TSData.pas',
  TSLineConstants in '..\General\TSLineConstants.pas',
  Ucmatrix in '..\Shared\Ucmatrix.pas',
  Ucomplex in '..\Shared\Ucomplex.pas',
  UPFC in '..\PCElements\UPFC.pas',
  Utilities in '..\Common\Utilities.pas',
  VCCS in '..\PCElements\vccs.pas',
  VSConverter in '..\PCElements\VSConverter.pas',
  VSource in '..\PCElements\VSource.pas',
  WireData in '..\General\WireData.pas',
  XfmrCode in '..\General\XfmrCode.pas',
  XYcurve in '..\General\XYcurve.pas',
  Ymatrix in '..\Common\Ymatrix.pas',
  DText in '..\DDLL\DText.pas',
  DLoads in '..\DDLL\DLoads.pas',
  DIDSSProperty in '..\DDLL\DIDSSProperty.pas',
  DCktElement in '..\DDLL\DCktElement.pas',
  DError in '..\DDLL\DError.pas',
  DCircuit in '..\DDLL\DCircuit.pas',
  DBus in '..\DDLL\DBus.pas',
  DSolution in '..\DDLL\DSolution.pas',
  DMonitors in '..\DDLL\DMonitors.pas',
  DMeters in '..\DDLL\DMeters.pas',
  DGenerators in '..\DDLL\DGenerators.pas',
  DDSSProgress in '..\DDLL\DDSSProgress.pas',
  DSettings in '..\DDLL\DSettings.pas',
  DLines in '..\DDLL\DLines.pas',
  DCtrlQueue in '..\DDLL\DCtrlQueue.pas',
  DDSSElement in '..\DDLL\DDSSElement.pas',
  DActiveClass in '..\DDLL\DActiveClass.pas',
  DCapacitors in '..\DDLL\DCapacitors.pas',
  DTransformers in '..\DDLL\DTransformers.pas',
  DSwtControls in '..\DDLL\DSwtControls.pas',
  DCapControls in '..\DDLL\DCapControls.pas',
  DRegControls in '..\DDLL\DRegControls.pas',
  DTopology in '..\DDLL\DTopology.pas',
  DDSSExecutive in '..\DDLL\DDSSExecutive.pas',
  DSensors in '..\DDLL\DSensors.pas',
  DXYCurves in '..\DDLL\DXYCurves.pas',
  DPDELements in '..\DDLL\DPDELements.pas',
  DReclosers in '..\DDLL\DReclosers.pas',
  DRelays in '..\DDLL\DRelays.pas',
  DCmathLib in '..\DDLL\DCmathLib.pas',
  DParser in '..\DDLL\DParser.pas',
  DLoadShape in '..\DDLL\DLoadShape.pas',
  DFuses in '..\DDLL\DFuses.pas',
  DISource in '..\DDLL\DISource.pas',
  DPVSystems in '..\DDLL\DPVSystems.pas',
  DVSources in '..\DDLL\DVSources.pas',
  DDSS in '..\DDLL\DDSS.pas',
  DYMatrix in '..\DDLL\DYMatrix.pas',
  DGICSources in '..\DDLL\DGICSources.pas',
  DLineCodes in '..\DDLL\DLineCodes.pas',
  DParallel in '..\DDLL\DParallel.pas',
  DReduceCkt in '..\DDLL\DReduceCkt.pas';

// Caution: these must match the case of the function names in
// py_dss_interface configurations.json
exports
   DSSPut_Command,
   DSSLoads, DSSLoadsF,DSSLoadsS, DSSLoadsV,
   DSSProperties,
   CktElementI, CktElementF, CktElementS, CktElementV,
   ErrorCode, ErrorDesc,
   CircuitI, CircuitF, CircuitS, CircuitV,
   BUSI, BUSF, BUSS, BUSV,
   SolutionI, SolutionF, SolutionS, SolutionV,
   MonitorsI, MonitorsS,MonitorsV,
   MetersI,MetersF,MetersS,MetersV,
   GeneratorsI,GeneratorsF,GeneratorsS,GeneratorsV,
   DSSProgressI,DSSProgressS,
   SettingsI,SettingsF,SettingsS,SettingsV,
   LinesI, LinesF, LinesS, LinesV,
   CtrlQueueI,CtrlQueueV,
   DSSElementI, DSSElementS, DSSElementV,
   ActiveClassI, ActiveClassS, ActiveClassV,
   CapacitorsI,CapacitorsF,CapacitorsS,CapacitorsV,
   TransformersI,TransformersF,TransformersS,TransformersV,
   SwtControlsI,SwtControlsF,SwtControlsS,SwtControlsV,
   CapControlsI,CapControlsF,CapControlsS,CapControlsV,
   RegControlsI,RegControlsF,RegControlsS,RegControlsV,
   TopologyI,TopologyS,TopologyV,
   DSSExecutiveI,DSSExecutiveS,
   SensorsI,SensorsF,SensorsS,SensorsV,
   XYCurvesI,XYCurvesF,XYCurvesS,XYCurvesV,
   PDElementsI,PDElementsF,PDElementsS,
   ReclosersI, ReclosersF, ReclosersS, ReclosersV,
   RelaysI,RelaysS,RelaysV,
   CmathLibF,CmathLibV,
   ParserI,ParserF,ParserS,ParserV,
   LoadShapeI,LoadShapeF,LoadShapeS,LoadShapeV,
   FusesI,FusesF,FusesS,FusesV,
   IsourceI,IsourceF,IsourceS,IsourceV,
   PVsystemsI,PVsystemsF,PVsystemsS,PVsystemsV,
   VsourcesI,VsourcesF,VsourcesS,VsourcesV,
   DSSI,DSSS,DSSV,
   InitAndGetYparams,GetCompressedYMatrix,ZeroInjCurr,GetSourceInjCurrents,GetPCInjCurr,
   SystemYChanged,BuildYMatrixD,UseAuxCurrents,AddInAuxCurrents,getIpointer,
   getVpointer,SolveSystem,ParallelI,ParallelV,LineCodesI,LineCodesF,LineCodesS,
   LineCodesV, GICSourcesI,GICSourcesF,GICSourcesS,GICSourcesV,ReduceCktI,ReduceCktF,ReduceCktS;

// {$R *.res}

begin
//  Writeln ('Begin OpenDSSDirect');
  IsDLL := TRUE;
  IsMultiThread := True;
///{Create one instance of DSS executive whenever the DSS Engine is init'd}
  DSSExecutive[ActiveActor] := TExecutive.Create;  // Start the DSS when DSS interface is created
  DSSExecutive[ActiveActor].CreateDefaultDSSItems;
///  //WriteDLLDebugFile(DSSDirectory);
end.
