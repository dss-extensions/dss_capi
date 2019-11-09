unit DSSClassDefs;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
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

var
    NumIntrinsicClasses,
    NumUserClasses: Integer;

procedure CreateDSSClasses;
procedure DisposeDSSClasses;
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
    GICSource,
    AutoTrans,
    
    DSSHelper;


{--------------------------------------------------------------}
procedure CreateDSSClasses;
begin
    DSSPrime := TDSS.Create();

    Classnames := THashList.Create(25);   // Makes 5 sub lists
    DSSClassList := TPointerList.Create(10);  // 10 is initial size and increment
    DSSClasses := TDSSClasses.Create;  // class to handle junk for defining DSS classes

     {General DSS objects, not circuit elements}
    DSSObjs := TPointerList.Create(25);  // 25 is initial size and increment

     {instantiate all Intrinsic Object Classes}

     {Generic Object classes first in case others refer to them}
    DSSPrime.SolutionClass := TDSSSolution.Create(DSSPrime);
    DSSClasses.New(DSSPrime.SolutionClass); // this is a special class

    DSSPrime.LineCodeClass := TLineCode.Create(DSSPrime);
    DSSClasses.New(DSSPrime.LineCodeClass);

    DSSPrime.LoadShapeClass := TLoadShape.Create(DSSPrime);
    DSSClasses.New(DSSPrime.LoadShapeClass);

    DSSPrime.TShapeClass := TTShape.Create(DSSPrime);
    DSSClasses.New(DSSPrime.TShapeClass);

    DSSPrime.PriceShapeClass := TPriceShape.Create(DSSPrime);
    DSSClasses.New(DSSPrime.PriceShapeClass);

    DSSPrime.XYCurveClass := TXYCurve.Create(DSSPrime);
    DSSClasses.New(DSSPrime.XYCurveClass);

    DSSPrime.GrowthShapeClass := TGrowthShape.Create(DSSPrime);
    DSSClasses.New(DSSPrime.GrowthShapeClass);

    DSSPrime.TCC_CurveClass := TTCC_Curve.Create(DSSPrime);
    DSSClasses.New(DSSPrime.TCC_CurveClass);

    DSSPrime.SpectrumClass := TSpectrum.Create(DSSPrime);
    DSSClasses.New(DSSPrime.SpectrumClass);

    DSSPrime.WireDataClass := TWireData.Create(DSSPrime);
    DSSClasses.New(DSSPrime.WireDataClass);

    DSSPrime.CNDataClass := TCNData.Create(DSSPrime);
    DSSClasses.New(DSSPrime.CNDataClass);

    DSSPrime.TSDataClass := TTSData.Create(DSSPrime);
    DSSClasses.New(DSSPrime.TSDataClass);

    DSSPrime.LineGeometryClass := TLineGeometry.Create(DSSPrime);
    DSSClasses.New(DSSPrime.LineGeometryClass);
    
    DSSPrime.LineSpacingClass := TLineSpacing.Create(DSSPrime);
    DSSClasses.New(DSSPrime.LineSpacingClass);

    DSSPrime.XfmrCodeClass := TXfmrCode.Create(DSSPrime);
    DSSClasses.New(DSSPrime.XfmrCodeClass);

     {Circuit Element Classes}
    DSSPrime.LineClass := TLine.Create(DSSPrime);
    DSSClasses.New(DSSPrime.LineClass);

    DSSPrime.VSourceClass := TVSource.Create(DSSPrime);    // 2-terminal Vsource
    DSSClasses.New(DSSPrime.VSourceClass);

    DSSPrime.ISourceClass := TISource.Create(DSSPrime);    // 2-terminal Isource
    DSSClasses.New(DSSPrime.ISourceClass);

    DSSPrime.VCSSClass := TVCCS.Create(DSSPrime);
    DSSClasses.New(DSSPrime.VCSSClass);

    DSSPrime.LoadClass := TLoad.Create(DSSPrime);
    DSSClasses.New(DSSPrime.FLoadClass);

    DSSPrime.TransformerClass := TTransf.Create(DSSPrime);
    DSSClasses.New(DSSPrime.TransformerClass);

    DSSPrime.RegControlClass := TRegControl.Create(DSSPrime);
    DSSClasses.New(DSSPrime.RegControlClass);

    DSSPrime.CapacitorClass := TCapacitor.Create(DSSPrime);
    DSSClasses.New(DSSPrime.CapacitorClass);

    DSSPrime.FReactorClass := TReactor.Create(DSSPrime);
    DSSClasses.New(DSSPrime.FReactorClass);

    DSSPrime.CapControlClass := TCapControl.Create(DSSPrime);
    DSSClasses.New(DSSPrime.CapControlClass);

    DSSPrime.FaultClass := TFault.Create(DSSPrime);
    DSSClasses.New(DSSPrime.FaultClass);

    DSSPrime.GeneratorClass := TGenerator.Create(DSSPrime);
    DSSClasses.New(DSSPrime.GeneratorClass);

    DSSPrime.GenDispatcherClass := TGenDispatcher.Create(DSSPrime);
    DSSClasses.New(DSSPrime.GenDispatcherClass);

    DSSPrime.StorageClass := TStorage.Create(DSSPrime);
    DSSClasses.New(DSSPrime.StorageClass);

    DSSPrime.StorageControllerClass := TStorageController.Create(DSSPrime);
    DSSClasses.New(DSSPrime.StorageControllerClass);

    DSSPrime.RelayClass := TRelay.Create(DSSPrime);
    DSSClasses.New(DSSPrime.RelayClass);

    DSSPrime.RecloserClass := TRecloser.Create(DSSPrime);
    DSSClasses.New(DSSPrime.RecloserClass);

    DSSPrime.FuseClass := TFuse.Create(DSSPrime);
    DSSClasses.New(DSSPrime.FuseClass);

//     DSSPrime.FeederClass    := TFeeder.Create(DSSPrime);
//     DSSClasses.New(DSSPrime.FeederClass);

    DSSPrime.SwtControlClass := TSwtControl.Create(DSSPrime);
    DSSClasses.New(DSSPrime.SwtControlClass);

    DSSPrime.PVSystemClass := TPVSystem.Create(DSSPrime);
    DSSClasses.New(DSSPrime.PVSystemClass);

    DSSPrime.UPFCClass := TUPFC.Create(DSSPrime);
    DSSClasses.New(DSSPrime.UPFCClass);

    DSSPrime.UPFCControlClass := TUPFCControl.Create(DSSPrime);
    DSSClasses.New(DSSPrime.UPFCControlClass);

    DSSPrime.ESPVLControlClass := TESPVLControl.Create(DSSPrime);
    DSSClasses.New(DSSPrime.ESPVLControlClass);

    DSSPrime.IndMach012Class := TIndMach012.Create(DSSPrime);
    DSSClasses.New(DSSPrime.IndMach012Class);

    DSSPrime.GICsourceClass := TGICsource.Create(DSSPrime); // GIC source
    DSSClasses.New(DSSPrime.GICsourceClass);

    DSSPrime.AutoTransClass := TAutoTrans.Create(DSSPrime); // Auto Transformer
    DSSClasses.New(DSSPrime.AutoTransClass);

    DSSPrime.InvControlClass := TInvControl.Create(DSSPrime);
    DSSClasses.New(DSSPrime.InvControlClass);

    DSSPrime.ExpControlClass := TExpControl.Create(DSSPrime);
    DSSClasses.New(DSSPrime.ExpControlClass);

    DSSPrime.GICLineClass := TGICLine.Create(DSSPrime);
    DSSClasses.New(DSSPrime.GICLineClass);
    
    DSSPrime.GICTransformerClass := TGICTransformer.Create(DSSPrime);
    DSSClasses.New(DSSPrime.GICTransformerClass);

    DSSPrime.VSConverterClass := TVSConverter.Create(DSSPrime);
    DSSClasses.New(DSSPrime.VSConverterClass);

    DSSPrime.MonitorClass := TDSSMonitor.Create(DSSPrime);  // Have to do this AFTER Generator
    DSSClasses.New(DSSPrime.MonitorClass);

    DSSPrime.EnergyMeterClass := TEnergyMeter.Create(DSSPrime);  // Have to do this AFTER Generator
    DSSClasses.New(DSSPrime.EnergyMeterClass);

    DSSPrime.SensorClass := TSensor.Create(DSSPrime);      // Create state estimation sensors
    DSSClasses.New(DSSPrime.SensorClass);

 { Create Classes for custom implementations }
    CreateMyDSSClasses;

    NumIntrinsicClasses := DSSClassList.ListSize;
    NumUserClasses := 0;

   {Add user-defined objects}


   {This feature has been disabled - doesn't work in IIS}

   // Check all DLLs in present directory and home DSS directory to see if they
   // are a user-defined DSS class

   //**** LoadUserClasses;


end;

//----------------------------------------------------------------------------
procedure DisposeDSSClasses;

var
    i: Integer;
    DSSObj: TDSSObject;
    TraceName: String;
    SuccessFree: String;

begin

    try
        SuccessFree := 'First Object';
        for i := 1 to DSSObjs.ListSize do
        begin
            DSSObj := DSSObjs.Get(i);
            TraceName := DSSObj.ParentClass.Name + '.' + DSSObj.Name;
            DSSObj.Free;
            SuccessFree := TraceName;
        end;
        TraceName := '(DSSObjs Class)';
        DSSObjs.Free;
    except
        On E: Exception do
            Dosimplemsg('Exception disposing of DSS Obj "' + TraceName + '". ' + CRLF +
                'Last Successful dispose was for object "' + SuccessFree + '" ' + CRLF +
                E.Message, 901);
    end;

    try
        for i := 1 to DSSClassList.ListSize do
            TDSSClass(DSSClassList.Get(i)).Free;
        TraceName := '(DSS Class List)';
        DSSClassList.Free;
        TraceName := '(DSS Classes)';
        DSSClasses.Free;
        TraceName := '(ClassNames)';
        ClassNames.Free;
    except
        On E: Exception do
            Dosimplemsg('Exception disposing of DSS Class"' + TraceName + '". ' + CRLF + E.Message, 902);
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

    Classref := ClassNames.Find(ObjType);

    case Classref of
        0:
        begin
            DoSimpleMsg('Error! Object Class "' + ObjType + '" not found.' + CRLF + parser.CmdString, 903);
            Result := FALSE;
            Exit;
        end;{Error}
    else
        LastClassReferenced := Classref;
    end;

    Result := TRUE;

end;

//----------------------------------------------------------------------------
function GetDSSClassPtr(const ClassName: String): TDSSClass;
begin
    Result := TDSSClass(DSSClassList.Get(ClassNames.Find(lowercase(ClassName))));
end;


end.
