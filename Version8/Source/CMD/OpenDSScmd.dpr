program OpenDSScmd;

{$APPTYPE CONSOLE}

//{$linkframework CoreFoundation}
//{$linkframework Carbon}

{ ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
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
  08/05/2008  Created from ESI DSS
}

{
	08/17/2016  Created from OpenDSS
 ----------------------------------------------------------
  Copyright (c) 2016 Battelle Memorial Institute
 ----------------------------------------------------------
}

uses
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF }
  CustApp,
  {$ENDIF }
  System.SysUtils,
  Arraydef in '..\Shared\Arraydef.pas',
  AutoAdd in '..\Common\AutoAdd.pas',
  Bus in '..\Common\Bus.pas',
  Capacitor in '..\PDElements\Capacitor.pas',
  CapControl in '..\Controls\CapControl.pas',
  Circuit in '..\Common\Circuit.pas',
  CktElement in '..\Common\CktElement.pas',
  CktElementClass in '..\Common\CktElementClass.pas',
  CktTree in '..\Shared\CktTree.pas',
  Command in '..\Shared\Command.pas',
  Conductor in '..\Common\Conductor.pas',
  ControlClass in '..\Controls\ControlClass.pas',
  ControlElem in '..\Controls\ControlElem.pas',
  ControlQueue in '..\Common\ControlQueue.pas',
  DSSCallBackRoutines in '..\Common\DSSCallBackRoutines.pas',
  DSSClass in '..\Common\DSSClass.pas',
  DSSClassDefs in '..\Common\DSSClassDefs.pas',
  DSSGlobals in '..\Common\DSSGlobals.pas',
  DSSObject in '..\General\DSSObject.pas',
  Dynamics in '..\Shared\Dynamics.pas',
  EnergyMeter in '..\Meters\EnergyMeter.pas',
  Equivalent in '..\PCElements\Equivalent.pas',
  EventQueue in '..\Common\EventQueue.pas',
  ExecCommands in '..\Executive\ExecCommands.pas',
  ExecHelper in '..\Executive\ExecHelper.pas',
  ExecOptions in '..\Executive\ExecOptions.pas',
  Executive in '..\Executive\Executive.pas',
  ExportCIMXML in '..\Common\ExportCIMXML.pas',
  ExportOptions in '..\Executive\ExportOptions.pas',
  ExportResults in '..\Common\ExportResults.pas',
  Fault in '..\PDElements\Fault.pas',
  Feeder in '..\Common\Feeder.pas',
  fuse in '..\PDElements\fuse.pas',
  UPFCControl in '..\Controls\UPFCControl.pas',
  GrowthShape in '..\General\GrowthShape.pas',
  HashList in '..\Shared\HashList.pas',
  IniRegSave in '..\Shared\IniRegSave.pas',
  Isource in '..\PCElements\Isource.pas',
  Line in '..\PDElements\Line.pas',
  LineCode in '..\General\LineCode.pas',
  LineGeometry in '..\General\LineGeometry.pas',
  LineSpacing in '..\General\LineSpacing.pas',
  LineUnits in '..\Shared\LineUnits.pas',
  Load in '..\PCElements\Load.pas',
  LoadShape in '..\General\LoadShape.pas',
  mathutil in '..\Shared\mathutil.pas',
  MeterClass in '..\Meters\MeterClass.pas',
  MeterElement in '..\Meters\MeterElement.pas',
  Monitor in '..\Meters\Monitor.pas',
  MyDSSClassDefs in 'MyDSSClassDefs.Pas',
  NamedObject in '..\General\NamedObject.pas',
  Notes in '..\Common\Notes.pas',
  OHLineConstants in '..\General\OHLineConstants.pas',
  ParserDel in '..\Parser\ParserDel.pas',
  PCClass in '..\PCElements\PCClass.pas',
  PCElement in '..\PCElements\PCElement.pas',
  PDClass in '..\PDElements\PDClass.pas',
  PDElement in '..\PDElements\PDElement.pas',
  PointerList in '..\Shared\PointerList.pas',
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
  Spectrum in '..\General\Spectrum.pas',
  StackDef in '..\Shared\StackDef.pas',
  Storage in '..\PCElements\Storage.pas',
  StorageController in '..\Controls\StorageController.pas',
  StoreUserModel in '..\PCElements\StoreUserModel.pas',
  SwtControl in '..\Controls\SwtControl.pas',
  TCC_Curve in '..\General\TCC_Curve.pas',
  Terminal in '..\Common\Terminal.pas',
  TOPExport in '..\Common\TOPExport.pas',
  Transformer in '..\PDElements\Transformer.pas',
  Ucmatrix in '..\Shared\Ucmatrix.pas',
  Ucomplex in '..\Shared\Ucomplex.pas',
  Utilities in '..\Common\Utilities.pas',
  VSource in '..\PCElements\VSource.pas',
  WireData in '..\General\WireData.pas',
  XfmrCode in '..\General\XfmrCode.pas',
  Ymatrix in '..\Common\Ymatrix.pas' {,
  TempShape in '..\General\TempShape.pas',
  XYcurve in '..\General\XYcurve.pas',
  PriceShape in '..\General\PriceShape.pas';

{$R *.RES},
  TempShape in '..\General\TempShape.pas',
  XYcurve in '..\General\XYcurve.pas',
  PriceShape in '..\General\PriceShape.pas',
  CNData in '..\General\CNData.pas',
  TSData in '..\General\TSData.pas',
  LineConstants in '..\General\LineConstants.pas',
  CNLineConstants in '..\General\CNLineConstants.pas',
  TSLineConstants in '..\General\TSLineConstants.pas',
  CableData in '..\General\CableData.pas',
  ConductorData in '..\General\ConductorData.pas',
  CableConstants in '..\General\CableConstants.pas',
  Pstcalc in '..\Shared\Pstcalc.pas',
  GICLine in '..\PCElements\GICLine.pas',
  VSConverter in '..\PCElements\VSConverter.pas',
  CapUserControl in '..\Controls\CapUserControl.pas',
  StorageVars in '..\PCElements\StorageVars.pas',
  CapControlVars in '..\Controls\CapControlVars.pas',
  InvControl in '..\Controls\InvControl.pas',
  GICTransformer in '..\PDElements\GICTransformer.pas',
  ExpControl in '..\Controls\ExpControl.pas',
  UPFC in '..\PCElements\UPFC.pas',
  GenDispatcher in '..\Controls\GenDispatcher.pas',
  KLUSolve in '..\Common\KLUSolve.pas',
  vccs in '..\PCElements\vccs.pas',
  MemoryMap_lib in '..\Meters\MemoryMap_lib.pas',
  Parallel_Lib in '..\Parallel_Lib\Parallel_Lib.pas',
  IndMach012 in '..\PCElements\IndMach012.pas',
  ESPVLControl in '..\Controls\ESPVLControl.pas',
  {$IFDEF MSWINDOWS}
  TCP_IP in '..\TCP_IP\TCP_IP.pas',
  {$ENDIF }
  ConnectOptions in '..\Executive\ConnectOptions.pas',
  Diakoptics in '..\Common\Diakoptics.pas',
  Sparse_Math in '..\Common\Sparse_Math.pas',
  MeTIS_Exec in '..\Common\MeTIS_Exec.pas',
  AutoTrans in '..\PDElements\AutoTrans.pas',
  GICsource in '..\PCElements\GICsource.pas',
  Generic5OrderMach in '..\PCElements\Generic5OrderMach.pas',
  fMonitor in '..\Meters\fMonitor.pas',
  LD_fm_infos in '..\Meters\LD_fm_infos.pas',
  VLNodeVars in '..\Meters\VLNodeVars.pas',
  GISCommands in '..\GISCommands\GISCommands.pas',
  djson in '..\Common\djson.pas',
  NumCPULib in '..\Parallel_Lib\NumCPULib.pas',
  WindGen in '..\PCElements\WindGen.pas',
  WindGenVars in '..\PCElements\WindGenVars.pas',
  WindGenUserModel in '..\PCElements\WindGenUserModel.pas',
  generator in '..\PCElements\generator.pas',
  GenUserModel in '..\PCElements\GenUserModel.pas',
  GeneratorVars in '..\PCElements\GeneratorVars.pas',
  {$IFDEF FPC}
   fncs in 'fncs.pas',
   linenoise in 'linenoise.pas';
  {$ENDIF }
  epiktimer in '..\epiktimer\epiktimer.pas' {/  linenoise in 'linenoise.pas';},
  CmdForms in 'CmdForms.pas';


{$R *.RES}

type
  TMyApplication = class(TObject)
  protected
    procedure DoRun(myParam  : array of string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure WriteLicensing; virtual;
    function GetOptionIdx(myCMD: String):Integer;
    procedure Execute(myCMD  : string);
  end;

CONST
  ExeName = 'OpenDSSCMD';

function UserFinished(Cmd:String):boolean;
Begin
	result := false;
	cmd := LowerCase (Cmd);
	if cmd='' then
		result := true
	else if cmd='exit' then
		result := true
	else if cmd[1]='q' then
		result := true;
End;

procedure TMyApplication.DoRun(myParam  : array of string);
var
  ErrorMsg,
  Cmd           : String;
  LNresult      : Pchar;
  i,
  CmdIdx        : Integer;

begin
	NoFormsAllowed            :=  True;
  ActiveActor               :=  1;
  IsDLL                     :=  FALSE;
	DSSExecutive[ActiveActor] := TExecutive.Create;  // Make a DSS object
	DSSExecutive[ActiveActor].CreateDefaultDSSItems;
	writeln('Startup Directory: ', StartupDirectory);
	writeln('Data Directory: ', DataDirectory[ActiveActor]);
	writeln('Output Directory: ', OutputDirectory[ActiveActor]);
	writeln('GetCurrentDir: ', GetCurrentDir);
	DataDirectory[ActiveActor] := StartupDirectory;
	OutputDirectory[ActiveActor] := StartupDirectory;
  SetCurrentDir(DataDirectory[ActiveActor]);
  NoFormsAllowed := False;  // messages will go to the console

  if length(myParam) > 0 then
  Begin
    for i := 0 to High(myParam) do
    Begin
      Cmd       :=  myParam[i];
      writeln(myParam[i]);
      Execute(Cmd);
    End;
  End
  else
  Begin
    write(CRLF);
    repeat
    begin  // this has no command history
      write('>>');
      readln(Cmd);
      Execute(Cmd);
    end until UserFinished (Cmd);

  End;
end;

procedure TMyApplication.Execute(myCMD  : string);
var
  CmdIdx    :   integer;
Begin
  CmdIdx    :=  GetOptionIdx(myCmd);

  if CmdIdx = 0 then
  Begin
    DSSExecutive[ActiveActor].Command := myCmd;
    if DSSExecutive[ActiveActor].LastError <> '' then
      writeln(DSSExecutive[ActiveActor].LastError)
    else
      writeln(GlobalResult);
  End
  else
  Begin
    case CmdIdx of
      1   : WriteHelp;
      2   : writeln('OpenDSS console ' + VersionString);
      3,4 : WriteLn('Not implemented in Embarcadero version');
      5   : WriteLicensing ;
      6,7 : WriteLn('Leaving the program');
    else
      writeln('option not recognized')
    End;
  End;

End;

constructor TMyApplication.Create;
begin
  inherited Create;
//  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

function TMyApplication.GetOptionIdx(myCMD: String):Integer;
Begin
  myCMD   :=  LowerCase(myCMD);
  Result  :=  0;
  if (myCMD = '-help') or (myCMD = '-h') then Result  :=  1;
  if (myCMD = '-v') then Result     :=  2;
  if (myCMD = '-f') then Result     :=  3;
  if (myCMD = '-l') then Result     :=  4;
  if (myCMD = '-lic') then Result   :=  5;
  if (myCMD = 'exit') then Result   :=  6;
  if (myCMD = 'q') then Result      :=  7;
End;

procedure TMyApplication.WriteHelp;

begin
  writeln('Usage: ', ExeName, ' [-v | -h | -f | -l] [stop_time] [filename]');
  writeln(' [filename] -> optional DSS command file.');
  writeln('      If provided, runs this file and exits.');
  writeln('      If not provided, accepts user commands at the >> prompt.');
  writeln(' -h -> displays this message and exits');
  writeln(' -f -> stop_time [filename] starts in FNCS co-simulation mode');
	writeln('      Stop_time is the co-simulation stopping time in seconds;');
	writeln('        may also append a single character d(ay), h(our) or m(inute) for units');
	writeln('      If filename is provided, that will be compiled before starting FNCS');
  writeln('      This option requires FNCS installation and opendss.yaml file');
	writeln('      Run with Envar FNCS_LOG_LEVEL=WARNING (default), INFO or DEBUG* to generate more logging output,');
	writeln('        DEBUG1 for FNCS command echo, outside the time step loop,');
	writeln('        DEBUG2 for FNCS command echo, inside the time step loop,');
	writeln('        DEBUG3 for FNCS topic map echo.');
  writeln(' -l -> stop_time [filename] starts in HELICS co-simulation mode');
	writeln('      Stop_time is the co-simulation stopping time in seconds;');
	writeln('        may also append a single character d(ay), h(our) or m(inute) for units');
	writeln('      If filename is provided, that will be compiled before starting HELICS');
  writeln('      This option requires HELICS installation and opendss.json file');
	writeln('      Run with Envar HELICS_LOG_LEVEL=WARNING (default), INFO or DEBUG* to generate more logging output,');
	writeln('        DEBUG1 for HELICS command echo, outside the time step loop,');
	writeln('        DEBUG2 for HELICS command echo, inside the time step loop,');
	writeln('        DEBUG3 for HELICS topic map echo.');
  writeln(' -v -> displays the version and exits');
  writeln(' -Lic -> displays the licese agreement');
  writeln(' exit, q -> leaves the program');
end;

procedure TMyApplication.WriteLicensing;

begin
  writeln(     'Copyright (c) 2008-2021, Electric Power Research Institute, Inc.'+ CRLF +
     'All rights reserved.'+ CRLF +
     ''+ CRLF +
'Redistribution and use in source and binary forms, with or without'+ CRLF +
'modification, are permitted provided that the following conditions are met:'+ CRLF +
'    * Redistributions of source code must retain the above copyright'+ CRLF +
'      notice, this list of conditions and the following disclaimer.'+ CRLF +
'    * Redistributions in binary form must reproduce the above copyright'+ CRLF +
'      notice, this list of conditions and the following disclaimer in the'+ CRLF +
'      documentation and/or other materials provided with the distribution.'+ CRLF +
'    * Neither the name of the Electric Power Research Institute, Inc., nor'+ CRLF +
'      the names of its contributors may be used to endorse or promote'+ CRLF +
'      products derived from this software without specific prior written'+ CRLF +
'      permission.'+ CRLF +
''+ CRLF +
'THIS SOFTWARE IS PROVIDED BY Electric Power Research Institute, Inc., "AS IS"'+ CRLF +
'AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE'+ CRLF +
'IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR'+ CRLF +
      'PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Electric Power Research Institute, Inc.,'+ CRLF +
     'OR ANY OTHER ENTITY CONTRIBUTING TO OR INVOLVED IN THE PROVISION OF THE SOFTWARE,'+ CRLF +
     'BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR'+ CRLF +
     'CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF'+ CRLF +
     'SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS'+ CRLF +
     'INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN'+ CRLF +
     'CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)'+ CRLF +
     'ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE'+ CRLF +
     'POSSIBILITY OF SUCH DAMAGE.'+ CRLF);
end;

var
  Application : TMyApplication;
  i,
  NumParam    : Integer;                                       // number of parameters obtained when called
  myParam     : Array of string;

begin
  setlength(myParam,0);
  NumParam      :=  ParamCount;
  if NumParam > 0 then
  Begin
    for i := 1 to NumParam do
    Begin
      setlength(myParam,length(myParam) + 1);
      myParam[high(myParam)]  :=  ParamStr(i);
    End;
  End;

  Application   :=  TMyApplication.Create;
  writeln('**********************EPRI OpenDSS simulator**********************');
  writeln('* OpenDSS console ' + VersionString + '                 *');
  writeln('*Copyright (c) 2008-2021, Electric Power Research Institute, Inc.*');
  writeln('******************************************************************');
  Application.DoRun(myParam);
  ExitCode      := DSSExecutive[ActiveActor].Error;
  Application.Free;
end.
end.
