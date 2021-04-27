unit Parallel_Lib;

{**********************Parallel Library for OpenDSS*****************************
* This library gives acces to the processor to handle the affinity of the
* Specified process and thread to a specific processor core
* This library gives access to the Windows API for such purpose
* Written by Davis Montenegro 06-17-2016
*
* Used for multi-core processing in OpenDSS
********************************************************************************
}


interface

uses
{$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages, vcl.Dialogs,
{$ELSE}
    {$IFDEF MSWINDOWS}
    windows,
    {$ELSE}
    initc, cpucount, BaseUnix, Unix,
    {$ENDIF}
{$ENDIF}  
  SysUtils, Variants, Classes, math;

const REALTIME_PRIORITY_CLASS = 16;
  
{$IFDEF MSWINDOWS}
const THREAD_PRIORITY_TIME_CRITICAL = 15;
type
  TParallel_Lib = class(TObject)
  public
    function Set_Thread_Affinity(Hnd : THandle; CPU : integer): Integer;
    function Set_Process_Priority(Hnd: THandle; P_priority : integer): Integer;
    function Set_Thread_Priority(Hnd: THandle; T_priority : integer): Integer;
    function Get_Thread_Priority(Hnd: THandle): String;
    function Get_Number_of_CPUs(): Integer;
  end;
{$ELSE}
const THREAD_PRIORITY_TIME_CRITICAL = tpTimeCritical;
type
  TParallel_Lib = class(TObject)
  public
    function Set_Thread_Affinity(Hnd: TThreadId; CPU: integer): Integer;
    function Set_Process_Priority(Hnd: TPid; P_priority: integer): Integer;
    function Set_Thread_Priority(thread: TThread; T_priority: TThreadPriority): Integer;
    function Get_Thread_Priority(Hnd: TThreadId): String;
    function Get_Number_of_CPUs(): Integer;
  end;
    
{$ENDIF}

implementation
{$IFNDEF UNIX}
    function TParallel_Lib.Set_Thread_Affinity(Hnd : THandle; CPU : integer): Integer;
    var
      CPU_bit   : integer;
      Op_Result : Dword;
    begin
       CPU_bit    :=  floor(power(2, CPU));
       Op_Result  := SetThreadAffinityMask(Hnd,CPU_bit);
       if Op_Result = 0 then raise Exception.Create('Error setting thread affinity mask : ' + IntToStr(GetLastError));
       Result   :=  Op_Result;
    end;
    function TParallel_Lib.Set_Process_Priority(Hnd: THandle; P_priority : integer):Integer;
    var
      Op_result   :  bool;
    begin
      Result      :=  0;
      Op_Result   :=  SetPriorityClass(Hnd, P_priority);
      if Op_result=false then 
        {$IFNDEF FPC}ShowMessage{$ELSE}WriteLn{$ENDIF}('Impossible to set the Process Priority');
      if Op_result then Result  :=1;
    end;
    function TParallel_Lib.Set_Thread_Priority(Hnd: THandle; T_priority : integer):Integer;
    var
      Op_result   :  bool;
    begin
      Result      :=  0;
      Op_Result   :=  SetThreadPriority(Hnd,T_priority);
      if Op_Result = false then 
        {$IFNDEF FPC}ShowMessage{$ELSE}WriteLn{$ENDIF}('Impossible to set the Thread Priority');
      if Op_result then Result  :=1;
    end;
    function TParallel_Lib.Get_Thread_Priority(Hnd: THandle): String;
    var
      Num_priority  : integer;
    begin
      Num_Priority  :=  GetThreadPriority(Hnd);
      case Num_Priority of
        0:  Result    :=  'Normal';
        1:  Result    :=  'Above Normal';
        2:  Result    :=  'Highest';
        15: Result    :=  'Time Critical';
        -1: Result    :=  'Below Normal';
        -2: Result    :=  'Lowest';
      else
          Result    :=  'Not known';
      end;
    end;
    function TParallel_Lib.Get_Number_of_CPUs(): Integer;
    begin
      Result  :=  CPUCount;
    end;
{$ELSE}    
    //function pthread_setaffinity_np(pid : Ptruint; cpusetsize : QWord; cpuset : pointer) : longint; cdecl; external;
    
    function TParallel_Lib.Set_Thread_Affinity(Hnd: TThreadId; CPU: integer): Integer;
    // The following commented code segfaults but it's based on 
    // http://free-pascal-general.1045716.n5.nabble.com/GetAffinity-SetAffinity-tp3351231p5717539.html
    // An alternative may be to include a separate C file to handle this using the correct macros
    {const
      cpu_SetSize = 8; // 64 cores max
    var
      cpu_set : QWord;  //cpu_set_type sufficient for 64-core CPU
      Op_Result : longint;
    begin
      cpu_set := 1 shl CPU;
      Op_Result := pthread_setaffinity_np(Hnd,cpu_SetSize,@cpu_set);
      if Op_Result = 0 then raise Exception.Create('Error setting thread affinity mask');
      Result   :=  Op_Result;
    end;}
    begin
      Result := 0;
    end;
    
    function TParallel_Lib.Set_Thread_Priority(thread: TThread; T_priority : TThreadPriority): Integer;
    begin
      Result      :=  0;
      try
        thread.Priority := T_priority;
        if thread.Priority = T_priority then
            Result  := 1;
      except
        WriteLn('Impossible to set the Thread Priority');
      end;
    end;
    
    function TParallel_Lib.Set_Process_Priority(Hnd: TPid; P_priority : integer):Integer;
    var
      Op_result   :  Integer;
    begin
      Result      :=  0;
      Op_Result   :=  fpsetpriority (prio_process,Hnd,P_priority);
      if Op_result = -1 then 
        WriteLn('Impossible to set the Process Priority');
      if Op_result <> 0 then Result  :=1;
    end;
    
    function TParallel_Lib.Get_Thread_Priority(Hnd: TThreadId): String;
    var
      Num_priority  : integer;
    begin
      Num_Priority  :=  ThreadGetPriority(Hnd);
      case Num_Priority of
        0:  Result    :=  'Normal';
        1:  Result    :=  'Above Normal';
        2:  Result    :=  'Highest';
        15: Result    :=  'Time Critical';
        -1: Result    :=  'Below Normal';
        -2: Result    :=  'Lowest';
      else
          Result    :=  'Not known';
      end;
    end;
    function TParallel_Lib.Get_Number_of_CPUs(): Integer;
    begin
      Result  :=  GetLogicalCpuCount();
    end;
{$ENDIF}
end.
