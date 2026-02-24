unit Spectrum;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

// Harmonic Spectrum specified as Harmonic, pct magnitude and angle
//
// Spectrum is shifted by the fundamental angle and stored in MultArray
// so that the fundamental is at zero degrees phase shift

interface

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    CAPI_Types,
    UComplex, DSSUcomplex;

type
{$SCOPEDENUMS ON}
    TSpectrumPropLegacy = (
        INVALID = 0,
        NumHarm = 1,
        harmonic = 2,
        pctmag = 3,
        angle = 4,
        CSVFile = 5
    );
    TSpectrumProp = (
        INVALID = 0,
        NumHarm = 1,
        Harmonic = 2,
        pctMag = 3,
        Angle = 4,
        CSVFile = 5
    );
{$SCOPEDENUMS OFF}
    
    TSpectrumObj = class;

    TSpectrum = class(TDSSClass)
    PROTECTED
        procedure DefineProperties(); override;
    PUBLIC
        DefaultGeneral: TSpectrumObj;
        DefaultLoad: TSpectrumObj;
        DefaultGen: TSpectrumObj;
        DefaultVSource: TSpectrumObj;

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
        procedure BindDefaults();
    end;

    TSpectrumObj = class(TDSSObject)
    PRIVATE
        puMagArray,
        AngleArray: pDoubleArray;
        MultArray: pComplexArray;
        csvfile: string;

        procedure SetMultArray();
        function HarmArrayHasaZero(var zeropoint: Integer): Boolean;

    PUBLIC
        NumHarm: Integer;          // Public so solution can get to it.
        HarmArray: pDoubleArray;

        constructor Create(ParClass: TDSSClass; const SpectrumName: String);
        destructor Destroy; OVERRIDE;
        procedure MakeLike(OtherPtr: Pointer); override;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;

        function GetMult(const h: Double): Complex;

        procedure DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
        procedure ReadCSVFile(const FileName: String; setterFlags: TDSSPropertySetterFlags);
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TSpectrumObj;
    TProp = TSpectrumProp;
    TPropLegacy = TSpectrumPropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    

procedure DoCSVFile(obj: TObj; const FileName: String; setterFlags: TDSSPropertySetterFlags);forward;

constructor TSpectrum.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

    inherited Create(dssContext, DSS_OBJECT, 'Spectrum');
end;

destructor TSpectrum.Destroy;
begin
    inherited Destroy;
end;

procedure TSpectrum.BindDefaults();
begin
    DefaultGeneral := Find('default');
    DefaultLoad := Find('defaultload');
    DefaultGen := Find('defaultgen');
    DefaultVSource := Find('defaultvsource');
end;

procedure TSpectrum.DefineProperties();
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    NumProperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    PropertyStructArrayCountOffset := PtrInt(@obj.NumHarm);

    SpecSetNames := ArrayOfString.Create(
        'Harmonic, Angle, pctMag',
        'CSVFile'
    );
    SpecSets := TSpecSets.Create(
        TSpecSet.Create(ord(TProp.Harmonic), ord(TProp.Angle), ord(TProp.pctMag)),
        TSpecSet.Create(ord(TProp.CSVFile))
    );

    // strings
    PropertyType[ord(TProp.csvfile)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.csvfile)] := PtrInt(@obj.csvfile);
    PropertyFlags[ord(TProp.csvfile)] := [TPropertyFlag.IsFilename, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.GlobalCount];

    // integer properties
    PropertyType[ord(TProp.NumHarm)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.NumHarm)] := PtrInt(@obj.NumHarm);
    PropertyFlags[ord(TProp.NumHarm)] := [TPropertyFlag.SuppressJSON];

    // double arrays
    PropertyType[ord(TProp.harmonic)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.harmonic)] := PtrInt(@obj.HarmArray);
    PropertyOffset2[ord(TProp.harmonic)] := PtrInt(@obj.NumHarm);
    PropertyFlags[ord(TProp.harmonic)] := [TPropertyFlag.RequiredInSpecSet];

    PropertyType[ord(TProp.angle)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.angle)] := PtrInt(@obj.AngleArray);
    PropertyOffset2[ord(TProp.angle)] := PtrInt(@obj.NumHarm);
    PropertyFlags[ord(TProp.angle)] := [TPropertyFlag.RequiredInSpecSet];

    PropertyType[ord(TProp.pctmag)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.pctmag)] := PtrInt(@obj.puMagArray);
    PropertyOffset2[ord(TProp.pctmag)] := PtrInt(@obj.NumHarm);
    PropertyScale[ord(TProp.pctmag)] := 0.01;
    PropertyFlags[ord(TProp.pctmag)] := [TPropertyFlag.RequiredInSpecSet];

    ActiveProperty := NumPropsThisClass;
    inherited;
end;

function TSpectrum.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    obj: TObj;
begin
    obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := obj;
    obj.ClassIndex := AddObjectToList(obj, Activate);
    Result := obj;
end;

procedure TSpectrumObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
var
    i: Integer;
begin
    case Idx of
        ord(TProp.NumHarm):
        begin
            if (HarmArray <> NIL) then // leave it NIL since there is a validation that uses that in Edit
                ReAllocmem(HarmArray, Sizeof(Double) * NumHarm); 
            //if (HarmArray <> NIL) then
            ReAllocmem(AngleArray, Sizeof(Double) * NumHarm); // Make a dummy Angle array
            for i := 1 to NumHarm do
                AngleArray[i] := 0.0; //TODO: remove -- left for backwards compatiblity, but this is kinda buggy
        end;
        ord(TProp.csvfile):
            DoCSVFile(self, csvfile, setterFlags);
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

function TSpectrum.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
var
    iZeroPoint: Integer;  // for error trapping
    obj: TObj;
begin
    obj := TObj(ptr);
    if (obj.HarmArray <> NIL) then   // Check this after HarmArray is allocated  2/20/2018
    begin
        if obj.HarmArrayHasaZero(iZeroPoint) then
            DoSimpleMsg('Error: Zero frequency detected in %s, point %d. Not allowed', [obj.FullName(), iZeroPoint], 65001)

        else
        if (obj.HarmArray <> NIL) and (obj.puMagArray <> NIL) and (obj.AngleArray <> NIL) then
            obj.SetMultArray();
    end;
    Exclude(obj.Flags, Flg.EditingActive);
    Result := True;
end;

procedure TSpectrumObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    NumHarm := Other.NumHarm;

    ReallocMem(HarmArray, Sizeof(HarmArray[1]) * NumHarm);
    ReallocMem(puMagArray, Sizeof(puMagArray[1]) * NumHarm);
    ReallocMem(AngleArray, Sizeof(AngleArray[1]) * NumHarm);

    for i := 1 to NumHarm do
    begin
        HarmArray[i] := Other.HarmArray[i];
        puMagArray[i] := Other.puMagArray[i];
        AngleArray[i] := Other.AngleArray[i];
    end;
end;

constructor TSpectrumObj.Create(ParClass: TDSSClass; const SpectrumName: String);
begin
    inherited Create(ParClass, SpectrumName);
    DSSObjType := ParClass.DSSClassType;

    NumHarm := 0;
    HarmArray := NIL;
    puMagArray := NIL;
    AngleArray := NIL;
    MultArray := NIL;
    csvfile := '';
end;

destructor TSpectrumObj.Destroy;
begin
    Reallocmem(HarmArray, 0);
    Reallocmem(puMagArray, 0);
    Reallocmem(AngleArray, 0);
    Reallocmem(MultArray, 0);
    inherited destroy;
end;

procedure TSpectrumObj.ReadCSVFile(const FileName: String; setterFlags: TDSSPropertySetterFlags);
var
    F: TStream = nil;
    numRead: Integer;
    inputLine: String;
    maxNum: Integer;
begin
    try
        F := DSS.GetInputStreamEx(FileName);
    except
        DoSimpleMsg('Error Opening CSV File: "%s"', [FileName], 653);
        FreeAndNil(F);
        Exit;
    end;

    if (TDSSPropertySetterFlag.ImplicitSizes in setterFlags) then
    begin
        try
            maxNum := NumHarm;
            NumHarm := 0; //TODO: raise exception if already allocated?
            if maxNum <= 0 then
                maxNum := 100;

            ReAllocmem(HarmArray, Sizeof(Double) * maxNum);
            ReAllocmem(puMagArray, Sizeof(Double) * maxNum);
            ReAllocmem(AngleArray, Sizeof(Double) * maxNum);

            numRead := 0;
            while true do
            begin
                if (F.Position + 1) >= F.Size then
                    break;

                if (numRead + 1) >= maxNum then
                begin
                    maxNum := maxNum * 3 div 2; // 100, 150, 225, 337, 505, 757, 1135, 1702...
                    ReAllocmem(HarmArray, Sizeof(Double) * maxNum);
                    ReAllocmem(puMagArray, Sizeof(Double) * maxNum);
                    ReAllocmem(AngleArray, Sizeof(Double) * maxNum);
                end;
                inc(numRead);
                FSReadln(F, inputLine);
                DSS.AuxParser.SetCmdString(inputLine);

                DSS.AuxParser.NextParam();
                HarmArray[numRead] := DSS.AuxParser.MakeDouble();
                DSS.AuxParser.NextParam();
                puMagArray[numRead] := DSS.AuxParser.MakeDouble() * 0.01;
                DSS.AuxParser.NextParam();
                AngleArray[numRead] := DSS.AuxParser.MakeDouble();
            end;
            FreeAndNil(F);
            NumHarm := numRead;
        except
            On E: Exception do
            begin
                DoSimpleMsg('Error reading %d-th numeric row from file: "%s" Error is:', [numRead, FileName, E.message], 705);
                FreeAndNil(F);
                NumHarm := numRead;
                Exit;
            end;
        end;
        Exit;
    end;

    // >> (TDSSPropertySetterFlag.ImplicitSizes NOT in setterFlags) <<
    try
        ReAllocmem(HarmArray, Sizeof(Double) * NumHarm);
        ReAllocmem(puMagArray, Sizeof(Double) * NumHarm);
        ReAllocmem(AngleArray, Sizeof(Double) * NumHarm);
        numRead := 0;
        while ((F.Position + 1) < F.Size) and (numRead < NumHarm) do
        begin
            Inc(numRead);
            FSReadln(F, inputLine);  // Use Auxparser, which allows for formats
            DSS.AuxParser.SetCmdString(inputLine);
            DSS.AuxParser.NextParam();
            HarmArray[numRead] := DSS.AuxParser.MakeDouble();
            DSS.AuxParser.NextParam();
            puMagArray[numRead] := DSS.AuxParser.MakeDouble() * 0.01;
            DSS.AuxParser.NextParam();
            AngleArray[numRead] := DSS.AuxParser.MakeDouble();
        end;
        F.Free();
        if (NumHarm <> numRead) and ((DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.PermissiveProperties)) = 0) Then
        begin
            DoSimpleMsg('%s.Spectrum: CSV file "%s" contains %d items, expected %d.', [self.FullName(), FileName, numRead, NumHarm], 2024108);
        end
        else
        begin
            NumHarm := numRead;   // reset number of points
        end;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error Processing CSV File: "%s". %s', [FileName, E.Message], 654);
            FreeAndNil(F);
            Exit;
        end;
    end;
end;

procedure DoCSVFile(obj: TObj; const FileName: String; setterFlags: TDSSPropertySetterFlags);
begin
    obj.ReadCSVFile(FileName, setterFlags);
end;

procedure TSpectrumObj.DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);

    for i := 1 to ParentClass.NumProperties do
        FSWriteln(F, '~ ' + ParentClass.PropertyName[i] + '=' + PropertyValue(i));

    if Complete then
    begin
        FSWriteln(F, 'Multiplier Array:');
        FSWriteln(F, 'Harmonic, Mult.re, Mult.im, Mag,  Angle');
        for i := 1 to NumHarm do
        begin
            FSWrite(F, Format('%-g', [HarmArray[i]]), ', ');
            FSWrite(F, Format('%-g, %-g, ', [MultArray[i].re, MultArray[i].im]));
            FSWrite(F, Format('%-g, %-g', [Cabs(MultArray[i]), Cdang(MultArray[i])]));
            FSWriteln(F);
        end;
    end;
end;

function TSpectrumObj.GetMult(const h: Double): Complex;
var
    i: Integer;
begin
    // Search List for  harmonic (nearest 0.01 harmonic) and return multiplier
    for i := 1 to NumHarm do
    begin
        if Abs(h - HarmArray[i]) < 0.01 then
        begin
            Result := MultArray[i];
            Exit;
        end;
    end;

    // None Found, return zero
    Result := 0;
end;

function TSpectrumObj.HarmArrayHasaZero(var ZeroPoint: Integer): Boolean;
var
    i: Integer;
begin
    Result := FALSE;
    ZeroPoint := 0;
    for i := 1 to NumHarm do
        if HarmArray[i] = 0.0 then
        begin
            Result := TRUE;
            ZeroPoint := i;
            Break;
        end;
end;

procedure TSpectrumObj.SetMultArray();
// Rotate all phase angles so that the fundamental is at zero
var
    i: Integer;
    FundAngle: Double;

begin
    try

        FundAngle := 0.0;
        for i := 1 to NumHarm do
        begin
            if Round(HarmArray[i]) = 1 then
            begin
                FundAngle := AngleArray[i];
                Break;
            end;
        end;

        Reallocmem(MultArray, Sizeof(MultArray[1]) * NumHarm);
        for i := 1 to NumHarm do
            MultArray[i] := pdegtocomplex(puMagArray[i], (AngleArray[i] - HarmArray[i] * FundAngle));

    except
        DoSimpleMsg('Exception while computing %s. Check Definition. Aborting', [FullName()], 655);
        if DSS.In_Redirect then
            DSS.Redirect_Abort := TRUE;
    end;
end;

end.
