UNIT CAPI_DSSimComs;
{$inline on}

INTERFACE

USES CAPI_Utils, UComplex;

PROCEDURE DSSimComs_BusVoltagepu(var ResultPtr: PDouble; var ResultCount: Integer; Index: PtrUInt);cdecl;
PROCEDURE DSSimComs_BusVoltage(var ResultPtr: PDouble; var ResultCount: Integer; Index: PtrUInt);cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Executive, SysUtils, solution, CktElement;

PROCEDURE DSSimComs_BusVoltagepu(var ResultPtr: PDouble; var ResultCount: Integer; Index: PtrUInt);cdecl;
VAR
  Result: PDoubleArray;
   i,j:Integer;
   Volts,BaseFactor:Double;
begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       i:=Index;
       Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (Buses^[i].NumNodesThisBus-1) + 1);
       If Buses^[i].kVBase >0.0 then BaseFactor :=  1000.0* Buses^[i].kVBase  Else BaseFactor := 1.0;
         For j := 1 to Buses^[i].NumNodesThisBus  DO
         Begin
           Volts := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)]);
           Result[j-1] := Volts/BaseFactor;
         End;
     End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
end;
//------------------------------------------------------------------------------
PROCEDURE DSSimComs_BusVoltage(var ResultPtr: PDouble; var ResultCount: Integer; Index: PtrUInt);cdecl;
VAR
  Result: PDoubleArray;
   i,j,k:Integer;
   Volts:Complex;
begin
   IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       i:=Index;
       Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*Buses^[i].NumNodesThisBus-1) + 1);
         For j := 1 to Buses^[i].NumNodesThisBus DO
         Begin
           Volts := ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)];
           k:=(j-1)*2;
           Result[k] := Volts.re;
           Result[k+1] := Volts.im;
         End;
     End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
END.
