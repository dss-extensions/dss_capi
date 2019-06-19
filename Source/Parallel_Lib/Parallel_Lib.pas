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
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.Messages,
  {$ENDIF}
  System.SysUtils,
  System.Variants,
  System.Classes
  ,math
  {$IFDEF MSWINDOWS}
  ,vcl.Dialogs
  {$ENDIF}
  ,TypInfo;

  CONST

     NumCPU     = 0;
     NumCore    = 1;
     NumSocket  = 2;
     NumNUMA    = 3;

type
  TLogicalProcessorInformation = record
    LogicalProcessorCount : integer;
    NumaNodeCount : integer;
    ProcessorCoreCount : integer;
    ProcessorL1CacheCount : integer;
    ProcessorL2CacheCount : integer;
    ProcessorL3CacheCount : integer;
    ProcessorPackageCount : integer;
end;


type
  TParallel_Lib = class(TObject)
  public
    function Set_Thread_Affinity(Hnd : THandle; CPU : integer): Integer;
    function Set_Process_Priority(Hnd: THandle; P_priority : integer): Integer;
    function Set_Thread_Priority(Hnd: THandle; T_priority : integer): Integer;
    function Get_Thread_Priority(Hnd: THandle): String;
    function Get_Number_of_CPUs(): Integer;
    function Get_Processor_Info(InfoType: Integer): Integer;
  end;


implementation
    // taken from
    // https://stackoverflow.com/questions/46093706/how-to-detect-number-of-logical-and-physical-processors-efficiently
    function CountSetBits(bitMask : NativeUInt) : integer;
    var
      lShift, i : integer;
      bitTest : NativeUInt;
    begin
      lShift := SizeOf(NativeUInt)*8 - 1;
      result := 0;
      bitTest := 1 shl lShift;
      for i := 0 to lShift do begin
        if (bitMask and bitTest) <> 0 then Inc(result);
        bitTest := bitTest shr 1;
      end;
    end;

    function GetLogicalProcessorInfo : TLogicalProcessorInformation;
    var
      i: Integer;
      ReturnLength: DWORD;
      Buffer: array of TSystemLogicalProcessorInformation;
    begin
      result.LogicalProcessorCount := 0;
      result.NumaNodeCount := 0;
      result.ProcessorCoreCount := 0;
      result.ProcessorL1CacheCount := 0;
      result.ProcessorL2CacheCount := 0;
      result.ProcessorL3CacheCount := 0;
      result.ProcessorPackageCount := 0;
      SetLength(Buffer, 256);
      if not GetLogicalProcessorInformation(@Buffer[0], ReturnLength) then
      begin
        if GetLastError = ERROR_INSUFFICIENT_BUFFER then begin
          SetLength(Buffer,
            ReturnLength div SizeOf(TSystemLogicalProcessorInformation) + 1);
          if not GetLogicalProcessorInformation(@Buffer[0], ReturnLength) then
            RaiseLastOSError;
        end else
          RaiseLastOSError;
      end;
      SetLength(Buffer, ReturnLength div SizeOf(TSystemLogicalProcessorInformation));

      for i := 0 to High(Buffer) do begin
        case Buffer[i].Relationship of
            RelationNumaNode: Inc(result.NumaNodeCount);
            RelationProcessorCore:
              begin
                Inc(result.ProcessorCoreCount);
                result.LogicalProcessorCount := result.LogicalProcessorCount + CountSetBits(Buffer[i].ProcessorMask);
              end;
            RelationCache:
              begin
                if (Buffer[i].Cache.Level = 1) then Inc(result.ProcessorL1CacheCount)
                else if (Buffer[i].Cache.Level = 2) then Inc(result.ProcessorL2CacheCount)
                else if (Buffer[i].Cache.Level = 3) then Inc(result.ProcessorL3CacheCount);
              end;
            RelationProcessorPackage: Inc(result.ProcessorPackageCount);
            else
              raise Exception.Create('Error: Unsupported LOGICAL_PROCESSOR_RELATIONSHIP value.');
        end;
      end;
    end;





    function TParallel_Lib.Set_Thread_Affinity(Hnd : THandle; CPU : integer): Integer;
    var
      CPU_bit   : integer;
      Op_Result : {$IFDEF MSWINDOWS}Dword{$ELSE}Cardinal{$ENDIF};
    begin
       CPU_bit    :=  floor(power(2, CPU));

       {$IFDEF MSWINDOWS}
       Op_Result  := SetThreadAffinityMask(Hnd,CPU_bit);
       {$ELSE}
       Op_Result  :=  1;
       {$ENDIF}
       if Op_Result = 0 then raise Exception.Create('Error setting thread affinity mask : ' + IntToStr(GetLastError));
       Result   :=  Op_Result;
    end;
    function TParallel_Lib.Set_Process_Priority(Hnd: THandle; P_priority : integer):Integer;
   {$IFDEF MSWINDOWS}
    var
      Op_result   :  bool;
    {$ENDIF}
    begin
      Result      :=  0;
      {$IFDEF MSWINDOWS}
      Op_Result   :=  SetPriorityClass(Hnd, P_priority);
      if Op_result=false then ShowMessage('Impossible to set the Process Priority');
      if Op_result then Result  :=1;
      {$ENDIF}
    end;
    function TParallel_Lib.Set_Thread_Priority(Hnd: THandle; T_priority : integer):Integer;
   {$IFDEF MSWINDOWS}
    var
      Op_result   :  bool;
    {$ENDIF}
    begin
      Result      :=  0;
     {$IFDEF MSWINDOWS}
      Op_Result   :=  SetThreadPriority(Hnd,T_priority);
      if Op_Result = false then ShowMessage('Impossible to set the Thread Priority');
      if Op_result then Result  :=1;
     {$ENDIF}
    end;
    function TParallel_Lib.Get_Thread_Priority(Hnd: THandle): String;
    var
      Num_priority  : integer;
    begin
     {$IFDEF MSWINDOWS}
      Num_Priority  :=  GetThreadPriority(Hnd);
     {$ELSE}
      Num_Priority  :=  0;
     {$ENDIF}
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
    function TParallel_Lib.Get_Processor_Info(InfoType: Integer): Integer;
    Var
      LProcInfo : TLogicalProcessorInformation;
    Begin
      LProcInfo := GetLogicalProcessorInfo;
      case InfoType of
        NumCPU:     Result  :=  LProcInfo.LogicalProcessorCount; // gets the number of CPUs (Threads)
        NumCore:    Result  :=  LProcInfo.ProcessorCoreCount;    // gets the number of physical cores
        NumSocket:  Result  :=  LProcInfo.NumaNodeCount;         // Number of NUMA Nodes (Should be the same number of sockets)
      end;
    End;
end.
