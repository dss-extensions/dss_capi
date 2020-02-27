unit DSSClassDefs;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    DSSClass,
    PointerList,
    HashList;

const

    BASECLASSMASK: Cardinal = $00000007;
    CLASSMASK: Cardinal = $FFFFFFF8;

      {Basic element types}
    NON_PCPD_ELEM = 1;  // A circuit Element we don't want enumerated in PD and PC Elements
    PD_ELEMENT = 2;
    PC_ELEMENT = 3;
    CTRL_ELEMENT = 4;
    METER_ELEMENT = 5;
    HIDDEN_ELEMENT = 6;

      {Specific element Types}
    MON_ELEMENT = 1 * 8;
    DSS_OBJECT = 2 * 8;   // Just a general DSS object, accessible to all circuits
    SOURCE = 3 * 8;
    XFMR_ELEMENT = 4 * 8;
    SUBSTATION = 5 * 8;  // not used
    LINE_ELEMENT = 6 * 8;
    LOAD_ELEMENT = 7 * 8;
    FAULTOBJECT = 8 * 8;
    ENERGY_METER = 9 * 8;
    GEN_ELEMENT = 10 * 8;
    CAP_CONTROL = 11 * 8;
    REG_CONTROL = 12 * 8;
    CAP_ELEMENT = 13 * 8;
    RELAY_CONTROL = 14 * 8;
    RECLOSER_CONTROL = 15 * 8;
    FUSE_CONTROL = 16 * 8;
    REACTOR_ELEMENT = 17 * 8;
    FEEDER_ELEMENT = 18 * 8;
    GEN_CONTROL = 19 * 8;
    SENSOR_ELEMENT = 20 * 8;
    STORAGE_ELEMENT = 21 * 8;
    STORAGE_CONTROL = 22 * 8;
    SWT_CONTROL = 23 * 8;
    PVSYSTEM_ELEMENT = 24 * 8;
    PVSYSTEM2_ELEMENT = 25 * 8; // Using 25 (PR)
      // Deleted --- VV_CONTROL       = 25 * 8;
    GIC_Line = 26 * 8;
    GIC_Transformer = 27 * 8;
    INV_CONTROL = 28 * 8;
    VS_CONVERTER = 29 * 8;
    EXP_CONTROL = 30 * 8;
    UPFC_ELEMENT = 31 * 8;
    UPFC_CONTROL = 32 * 8;
    VCCS_ELEMENT = 33 * 8;
    ESPVL_CONTROL = 34 * 8;
    INDMACH012_ELEMENT = 35 * 8;
    GIC_SOURCE = 36 * 8;
    AUTOTRANS_ELEMENT = 37 * 8;
    FMON_ELEMENT = 38 * 8;                        {BY Dahei UCF}
    Generic5OrderMach_ELEMENT = 39 * 8;         {BY Dahei UCF}
    INV_CONTROL2 = 40 * 8;
    STORAGE2_ELEMENT = 41 * 8;
    STORAGE2_CONTROL = 42 * 8;

var
    NumIntrinsicClasses,
    NumUserClasses: Integer;

procedure CreateDSSClasses;
procedure DisposeDSSClasses(AllActors: Boolean);
function GetDSSClassPtr(const ClassName: String): TDSSClass;
function SetObjectClass(const ObjType: String): Boolean;


implementation

uses
    SysUtils,
    DSSGlobals,
    DSSObject,
    ParserDel,
    MyDSSClassDefs,
    Solution,
    Bus,
    Line,
    VSource,
    Isource,
    VCCS,
    LineCode,
    Spectrum,
    WireData,
    CNData,
    TSData,
    LineGeometry,
    LineSpacing,
    Load,
    LoadShape,
    TempShape,
    PriceShape,
    XYCurve,
    Monitor,
    EnergyMeter,
    GrowthShape,
    TCC_Curve,
    Transformer,
    Capacitor,
    Reactor,
    Fault,
    Generator,
    RegControl,
    CapControl,
    GenDispatcher,
    Relay,
    Recloser,
    Fuse,
    Sensor,
    Feeder,
    XfmrCode,
    Storage,
    Storage2,
    StorageController,
    StorageController2,
    SwtControl,
    PVSystem,
    PVSystem2,
    InvControl,
    InvControl2,
    GICLine,
    GICTransformer,
    VSConverter,
    ExpControl,
    UPFC,
    UPFCControl,
    ESPVLControl,
    IndMach012,
    GICsource,
    AutoTrans,
     //by Dahei
    Generic5OrderMach,
     // By Dahei
    FMonitor;


{--------------------------------------------------------------}
procedure CreateDSSClasses;


begin

    Classnames[ActiveActor] := THashList.Create(25);   // Makes 5 sub lists
    DSSClassList[ActiveActor] := TPointerList.Create(10);  // 10 is initial size and increment
    if not Assigned(DSSClasses) then
        DSSClasses := TDSSClasses.Create;  // class to handle junk for defining DSS classes

     {General DSS objects, not circuit elements}
    DSSObjs[ActiveActor] := TPointerList.Create(25);  // 25 is initial size and increment

     {instantiate all Intrinsic Object Classes}

     {Generic Object classes first in case others refer to them}
    DSSClasses.New := TDSSSolution.Create;
    SolutionClass[ActiveActor] := ActiveDSSClass[ActiveActor];     // this is a special class
    DSSClasses.New := TLineCode.Create;
    LoadShapeClass[ActiveActor] := TLoadShape.Create;
    DSSClasses.New := LoadShapeClass[ActiveActor];
    TShapeClass[ActiveActor] := TTShape.Create;
    DSSClasses.New := TShapeClass[ActiveActor];
    PriceShapeClass[ActiveActor] := TPriceShape.Create;
    DSSClasses.New := PriceShapeClass[ActiveActor];
    XYCurveClass[ActiveActor] := TXYCurve.Create;
    DSSClasses.New := XYCurveClass[ActiveActor];
    GrowthShapeClass[ActiveActor] := TGrowthShape.Create;
    DSSClasses.New := GrowthShapeClass[ActiveActor];
    TCC_CurveClass[ActiveActor] := TTCC_Curve.Create;
    DSSClasses.New := TCC_CurveClass[ActiveActor];
    SpectrumClass[ActiveActor] := TSpectrum.Create;
    DSSClasses.New := SpectrumClass[ActiveActor];
    WireDataClass[ActiveActor] := TWireData.Create;
    DSSClasses.New := WireDataClass[ActiveActor];
    CNDataClass[ActiveActor] := TCNData.Create;
    DSSClasses.New := CNDataClass[ActiveActor];
    TSDataClass[ActiveActor] := TTSData.Create;
    DSSClasses.New := TSDataClass[ActiveActor];
    DSSClasses.New := TLineGeometry.Create;
    LineSpacingClass[ActiveActor] := TLineSpacing.Create;
    DSSClasses.New := LineSpacingClass[ActiveActor];
    DSSClasses.New := TXfmrCode.Create;

     {Circuit Element Classes}
    DSSClasses.New := TLine.Create;
    ActiveVSource[ActiveActor] := TVSource.Create;
    DSSClasses.New := ActiveVSource[ActiveActor];   // 2-terminal Vsource
    DSSClasses.New := TISource.Create;              // 2-terminal Isource
    DSSClasses.New := TVCCS.Create;
    DSSClasses.New := TLoad.Create;
    DSSClasses.New := TTransf.Create;
    DSSClasses.New := TRegControl.Create;
    DSSClasses.New := TCapacitor.Create;
    DSSClasses.New := TReactor.Create;
    DSSClasses.New := TCapControl.Create;
    DSSClasses.New := TFault.Create;
    DSSClasses.New := TGenerator.Create;
    DSSClasses.New := TGenDispatcher.Create;
    StorageClass[ActiveActor] := TStorage.Create;
    DSSClasses.New := StorageClass[ActiveActor];
    Storage2Class[ActiveActor] := TStorage2.Create;
    DSSClasses.New := Storage2Class[ActiveActor];
    DSSClasses.New := TStorageController.Create;
    DSSClasses.New := TStorageController2.Create;
    DSSClasses.New := TRelay.Create;
    DSSClasses.New := TRecloser.Create;
    DSSClasses.New := TFuse.Create;
//     FeederClass    := TFeeder.Create;
//     DSSClasses.New := FeederClass;
    DSSClasses.New := TSwtControl.Create;
    PVSystemClass[ActiveActor] := TPVSystem.Create;
    DSSClasses.New := PVSystemClass[ActiveActor];
    PVSystem2Class[ActiveActor] := TPVSystem2.Create;
    DSSClasses.New := PVSystem2Class[ActiveActor];
    DSSClasses.New := TUPFC.Create;
    DSSClasses.New := TUPFCControl.Create;
    DSSClasses.New := TESPVLControl.Create;
    DSSClasses.New := TIndMach012.Create;

    DSSClasses.New := TGICsource.Create; // GIC source
    DSSClasses.New := TAutoTrans.Create; // Auto Transformer


    InvControlClass[ActiveActor] := TInvControl.Create;
    DSSClasses.New := InvControlClass[ActiveActor];
    InvControl2Class[ActiveActor] := TInvControl2.Create;
    DSSClasses.New := InvControl2Class[ActiveActor];

    ExpControlClass[ActiveActor] := TExpControl.Create;
    DSSClasses.New := ExpControlClass[ActiveActor];

    DSSClasses.New := TGICLine.Create;
    DSSClasses.New := TGICTransformer.Create;

    DSSClasses.New := TVSConverter.Create;

    MonitorClass[ActiveActor] := TDSSMonitor.Create;  // Have to do this AFTER Generator
    DSSClasses.New := MonitorClass[ActiveActor];
    EnergyMeterClass[ActiveActor] := TEnergyMeter.Create;  // Have to do this AFTER Generator
    DSSClasses.New := EnergyMeterClass[ActiveActor];
    SensorClass[ActiveActor] := TSensor.Create;      // Create state estimation sensors
    DSSClasses.New := SensorClass[ActiveActor];


   {Add user-defined objects}
   //by Dahei (UCF)
    FMonitorClass[ActiveActor] := TDSSFMonitor.Create;  // Have to do this AFTER Generator
    DSSClasses.New := FMonitorClass[ActiveActor];
    DSSClasses.New := TGeneric5.Create;

 { Create Classes for custom implementations }
    CreateMyDSSClasses;

    NumIntrinsicClasses := DSSClassList[ActiveActor].ListSize;
    NumUserClasses := 0;


   {This feature has been disabled - doesn't work in IIS}

   // Check all DLLs in present directory and home DSS directory to see if they
   // are a user-defined DSS class

   //**** LoadUserClasses;


end;

//----------------------------------------------------------------------------
procedure DisposeDSSClasses(AllActors: Boolean);

var
    DSSCidx,
    temp,
    i: Integer;
    DSSObj: TDSSObject;
    DSSClass_idx: TDSSClass;
    TraceName,
    SuccessFree: String;

begin
    if not AllActors then
    begin
        try
            SuccessFree := 'First Object';
            for i := 1 to DSSObjs[ActiveActor].ListSize do
            begin
                DSSObj := DSSObjs[ActiveActor].Get(i);
                TraceName := DSSObj.DSSClassName + '.' + DSSObj.Name;
                DSSObj.Free;
                SuccessFree := TraceName;
            end;
            TraceName := '(DSSObjs Class)';
            FreeAndNil(DSSObjs[ActiveActor]);
        except
            On E: Exception do
                Dosimplemsg('Exception disposing of DSS Obj "' + TraceName + '". ' + CRLF +
                    'Last Successful dispose was for object "' + SuccessFree + '" ' + CRLF +
                    E.Message, 901);
        end;

        try
            for i := 1 to DSSClassList[ActiveActor].ListSize do
            begin
                DSSClass_idx := DSSClassList[ActiveActor].Get(i);
                FreeAndNil(DSSClass_idx);
            end;
            TraceName := '(DSS Class List)';
            FreeAndNil(DSSClassList[ActiveActor]);
            TraceName := '(ClassNames)';
            FreeAndNil(ClassNames[ActiveActor]);

        except
            On E: Exception do
                Dosimplemsg('Exception disposing of DSS Class"' + TraceName + '". ' + CRLF + E.Message, 902);
        end;

    end
    else
    begin
        temp := ActiveActor;
        for DSSCidx := 1 to NumOfActors do
        begin
            ActiveActor := DSSCidx;
            DisposeDSSClasses(FALSE);
        end;
        TraceName := '(DSS Classes)';
        FreeAndNil(DSSClasses);
        ActiveActor := 1;
    end;
end;


{--------------------------------------------------------------}
procedure AddUserClass;

begin
      // ***** ADD STUFF HERE ****

      {Assumes DLL has been loaded by call to LoadLibrary and the Handle is stored
       in LastUserDLLHandle.  Also, assumes DSSRegisterProc has the address of
       the user.}


     { ***** Needs to be re-done ****** }


end;

{--------------------------------------------------------------}
procedure LoadUserClasses;
var
    F: TSearchRec;
begin

{  Rework This !!!!}

    // Check All DLLs in present directory
    if FindFirst('*.dll', 0, F) = 0 then
    begin
        repeat
            if IsDSSDLL(F.Name) then
                AddUserclass; // Attempt to add (ignored if classname already exists)
        until FindNext(F) <> 0;
    end;

    // Check All DLLs in DSS Directory   unless that is the directory we just checked
    if comparetext(StartupDirectory, DSSDirectory) <> 0 then
        if FindFirst(DSSDirectory + '*.dll', 0, F) = 0 then
        begin
            repeat
                if IsDSSDLL(F.Name) then
                    AddUserclass; // Attempt to add (ignored if classname already exists)
            until FindNext(F) <> 0;
        end;

end;

//----------------------------------------------------------------------------
function SetObjectClass(const ObjType: String): Boolean;

// set LastClassReferenced variable by class name

var
    Classref: Integer;

begin

    Classref := ClassNames[ActiveActor].Find(ObjType);

    case Classref of
        0:
        begin
            DoSimpleMsg('Error! Object Class "' + ObjType + '" not found.' + CRLF + parser[ActiveActor].CmdString, 903);
            Result := FALSE;
            Exit;
        end;{Error}
    else
        LastClassReferenced[ActiveActor] := Classref;
    end;

    Result := TRUE;

end;

//----------------------------------------------------------------------------
function GetDSSClassPtr(const ClassName: String): TDSSClass;
begin
    Result := TDSSClass(DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find(lowercase(ClassName))));
end;


end.
