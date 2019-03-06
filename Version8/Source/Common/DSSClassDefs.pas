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
    StorageController,
    SwtControl,
    PVSystem,
    InvControl,
    GICLine,
    GICTransformer,
    VSConverter,
    ExpControl,
    UPFC,
    UPFCControl,
    ESPVLControl,
    IndMach012,
    GICsource,
    AutoTrans;


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

    LineCodeClass[ActiveActor] := TLineCode.Create;
    DSSClasses.New := LineCodeClass[ActiveActor];

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

    LineGeometryClass[ActiveActor] := TLineGeometry.Create;
    DSSClasses.New := LineGeometryClass[ActiveActor];

    LineSpacingClass[ActiveActor] := TLineSpacing.Create;
    DSSClasses.New := LineSpacingClass[ActiveActor];

    XfmrCodeClass[ActiveActor] := TXfmrCode.Create;
    DSSClasses.New := XfmrCodeClass[ActiveActor];

     {Circuit Element Classes}
    LineClass[ActiveActor] := TLine.Create;
    DSSClasses.New := LineClass[ActiveActor];

    VSourceClass[ActiveActor] := TVSource.Create;    // 2-terminal Vsource
    DSSClasses.New := VSourceClass[ActiveActor];

    ISourceClass[ActiveActor] := TISource.Create;    // 2-terminal Isource
    DSSClasses.New := ISourceClass[ActiveActor];

    VCSSClass[ActiveActor] := TVCCS.Create;
    DSSClasses.New := VCSSClass[ActiveActor];

    LoadClass[ActiveActor] := TLoad.Create;
    DSSClasses.New := LoadClass[ActiveActor];

    TransformerClass[ActiveActor] := TTransf.Create;
    DSSClasses.New := TransformerClass[ActiveActor];

    RegControlClass[ActiveActor] := TRegControl.Create;
    DSSClasses.New := RegControlClass[ActiveActor];

    CapacitorClass[ActiveActor] := TCapacitor.Create;
    DSSClasses.New := CapacitorClass[ActiveActor];

    ReactorClass[ActiveActor] := TReactor.Create;
    DSSClasses.New := ReactorClass[ActiveActor];

    CapControlClass[ActiveActor] := TCapControl.Create;
    DSSClasses.New := CapControlClass[ActiveActor];

    FaultClass[ActiveActor] := TFault.Create;
    DSSClasses.New := FaultClass[ActiveActor];

    GeneratorClass[ActiveActor] := TGenerator.Create;
    DSSClasses.New := GeneratorClass[ActiveActor];

    GenDispatcherClass[ActiveActor] := TGenDispatcher.Create;
    DSSClasses.New := GenDispatcherClass[ActiveActor];

    StorageClass[ActiveActor] := TStorage.Create;
    DSSClasses.New := StorageClass[ActiveActor];

    StorageControllerClass[ActiveActor] := TStorageController.Create;
    DSSClasses.New := StorageControllerClass[ActiveActor];

    RelayClass[ActiveActor] := TRelay.Create;
    DSSClasses.New := RelayClass[ActiveActor];

    RecloserClass[ActiveActor] := TRecloser.Create;
    DSSClasses.New := RecloserClass[ActiveActor];

    FuseClass[ActiveActor] := TFuse.Create;
    DSSClasses.New := FuseClass[ActiveActor];

//     FeederClass[ActiveActor]    := TFeeder.Create;
//     DSSClasses.New := FeederClass[ActiveActor];

    SwtControlClass[ActiveActor] := TSwtControl.Create;
    DSSClasses.New := SwtControlClass[ActiveActor];

    PVSystemClass[ActiveActor] := TPVSystem.Create;
    DSSClasses.New := PVSystemClass[ActiveActor];

    UPFCClass[ActiveActor] := TUPFC.Create;
    DSSClasses.New := UPFCClass[ActiveActor];

    UPFCControlClass[ActiveActor] := TUPFCControl.Create;
    DSSClasses.New := UPFCControlClass[ActiveActor];

    ESPVLControlClass[ActiveActor] := TESPVLControl.Create;
    DSSClasses.New := ESPVLControlClass[ActiveActor];

    IndMach012Class[ActiveActor] := TIndMach012.Create;
    DSSClasses.New := IndMach012Class[ActiveActor];

    GICsourceClass[ActiveActor] := TGICsource.Create; // GIC source
    DSSClasses.New := GICsourceClass[ActiveActor];

    AutoTransClass[ActiveActor] := TAutoTrans.Create; // Auto Transformer
    DSSClasses.New := AutoTransClass[ActiveActor];

    InvControlClass[ActiveActor] := TInvControl.Create;
    DSSClasses.New := InvControlClass[ActiveActor];

    ExpControlClass[ActiveActor] := TExpControl.Create;
    DSSClasses.New := ExpControlClass[ActiveActor];

    GICLineClass[ActiveActor] := TGICLine.Create;
    DSSClasses.New := GICLineClass[ActiveActor];

    GICTransformerClass[ActiveActor] := TGICTransformer.Create;
    DSSClasses.New := GICTransformerClass[ActiveActor];

    VSConverterClass[ActiveActor] := TVSConverter.Create;
    DSSClasses.New := VSConverterClass[ActiveActor];

    MonitorClass[ActiveActor] := TDSSMonitor.Create;  // Have to do this AFTER Generator
    DSSClasses.New := MonitorClass[ActiveActor];

    EnergyMeterClass[ActiveActor] := TEnergyMeter.Create;  // Have to do this AFTER Generator
    DSSClasses.New := EnergyMeterClass;

    SensorClass[ActiveActor] := TSensor.Create;      // Create state estimation sensors
    DSSClasses.New := SensorClass[ActiveActor];


 { Create Classes for custom implementations }
    CreateMyDSSClasses;

    NumIntrinsicClasses := DSSClassList[ActiveActor].ListSize;
    NumUserClasses := 0;

   {Add user-defined objects}

   {This feature has been disabled - doesn't work in IIS}

   // Check all DLLs in present directory and home DSS directory to see if they
   // are a user-defined DSS class

   //**** LoadUserClasses;


end;

//----------------------------------------------------------------------------
procedure DisposeDSSClasses(AllActors: Boolean);

var
    i: Integer;
    DSSObj: TDSSObject;
    TraceName: String;
    SuccessFree: String;
    DSSCidx, temp: Integer;

begin
    if not AllActors then
    begin
        try
            SuccessFree := 'First Object';
            for i := 1 to DSSObjs[ActiveActor].ListSize do
            begin
                DSSObj := DSSObjs[ActiveActor].Get(i);
                TraceName := DSSObj.ParentClass.Name + '.' + DSSObj.Name;
                DSSObj.Free;
                SuccessFree := TraceName;
            end;
            TraceName := '(DSSObjs Class)';
            DSSObjs[ActiveActor].Free;
        except
            On E: Exception do
                Dosimplemsg('Exception disposing of DSS Obj "' + TraceName + '". ' + CRLF +
                    'Last Successful dispose was for object "' + SuccessFree + '" ' + CRLF +
                    E.Message, 901);
        end;

        try
            for i := 1 to DSSClassList[ActiveActor].ListSize do
                TDSSClass(DSSClassList[ActiveActor].Get(i)).Free;
            TraceName := '(DSS Class List)';
            DSSClassList[ActiveActor].Free;
            TraceName := '(ClassNames)';
            ClassNames[ActiveActor].Free;
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
        DSSClasses.Free;
        DSSClasses := NIL;
        ActiveActor := temp;
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
