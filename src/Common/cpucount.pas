// Unit originally from FPC Wiki: http://wiki.freepascal.org/Example_of_multi-threaded_application:_array_of_threads

unit cpucount;
interface
//returns number of cores: a computer with two hyperthreaded cores will report 4
function GetLogicalCpuCount: Integer;

implementation
 
{$IF defined(windows)}
uses windows;
{$endif}
 
{$IF defined(darwin)}
uses ctypes, sysctl, unixtype;
{$endif} 
 
{$IFDEF Linux}
uses initc, ctypes;
 
const _SC_NPROCESSORS_ONLN = 83;
function sysconf(i: cint): clong; cdecl; external name 'sysconf';
{$ENDIF}
 
 
function GetLogicalCpuCount: integer;
// returns a good default for the number of threads on this system
{$IF defined(windows)}
//returns total number of processors available to system including logical hyperthreaded processors
var
  i: Integer;
  ProcessAffinityMask, SystemAffinityMask: DWORD_PTR;
  Mask: DWORD;
  SystemInfo: SYSTEM_INFO;
begin
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask, SystemAffinityMask)
  then begin
    Result := 0;
    for i := 0 to 31 do begin
      Mask := DWord(1) shl i;
      if (ProcessAffinityMask and Mask)<>0 then
        inc(Result);
    end;
  end else begin
    //can't get the affinity mask so we just report the total number of processors
    GetSystemInfo(SystemInfo);
    Result := SystemInfo.dwNumberOfProcessors;
  end;
end;
{$ELSEIF defined(freebsd) or defined(darwin)}
const
  param: string = 'hw.logicalcpu';
var
  len: size_t;
  t: size_t;
begin
  len := sizeof(t);
  fpsysctlbyname(pchar(param), @t, @len, nil, 0);
  Result := t;
end;
{$ELSEIF defined(linux)}
  begin
    Result:=sysconf(_SC_NPROCESSORS_ONLN);
  end;
 
{$ELSE}
  begin
    Result:=1;
  end;
{$ENDIF}
end.