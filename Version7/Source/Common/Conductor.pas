unit Conductor;

{
   ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{USES
    TCC_Curve;}

type

    TConductor = class(TObject)
    PRIVATE
        TCCName: String;
      //TCC:TTCC_Curve;  // pointer to TCC curve or nil
        AmbientTemp: Double;
        Accum_Isqt: Double; // Accumulated I2t
   //   ThermalConstant:Double;  // Meaure of how fast heat can be conducted away
        procedure Set_Ambient(Value: Double);
        procedure Set_TCCname(const Value: String);

    PUBLIC
        Closed: Boolean;    // change this variable to indicate open or closed switch
        FuseBlown: Boolean;
        procedure CalcIsqt(CurrentMag: Double);  // Computes whether conductor has burned down
        procedure ResetIsqt;  // restore the conductor and reset the i2t calcs

        constructor Create;
        destructor Destroy; OVERRIDE;

        property Ambient: Double WRITE Set_Ambient;
        property TccCurve: String READ TCCname WRITE Set_TCCname;


    end;

    pTConductorArray = ^TConductorArray;
    TConductorArray = array[1..1] of Tconductor;

implementation

uses
    Sysutils,
    DSSGlobals;

constructor TConductor.Create;
begin
    inherited Create;
    Closed := TRUE;
    FuseBlown := FALSE;
    Accum_Isqt := 0.0;
     //TCC := nil;
    TCCName := '';
end;

destructor TConductor.Destroy;
begin

    inherited Destroy;
end;

procedure TConductor.Set_Ambient(Value: Double);
begin
    AmbientTemp := Value;
end;

procedure TConductor.Set_TCCname(const Value: String);
begin
    TCCname := lowercase(value);

end;


procedure TConductor.CalcIsqt(CurrentMag: Double);  // Computes whether conductor has burned down
begin


    DoSimpleMsg('Need to implement Tconductor.CalcIsqrt', 770);


end;

procedure TConductor.ResetIsqt;  // restore the conductor and reset the i2t calcs
begin


    DoSimpleMsg('Need to implement Tconductor.ResetIsqt', 771);


end;


end.
