unit Spectrum;

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

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
    Arraydef,
    UComplex, DSSUcomplex;

type
{$SCOPEDENUMS ON}
    TSpectrumProp = (
        INVALID = 0,
        NumHarm = 1,
        harmonic = 2,
        pctmag = 3,
        angle = 4,
        CSVFile = 5
    );
{$SCOPEDENUMS OFF}
    
    TSpectrumObj = class;

    TSpectrum = class(TDSSClass)
    PROTECTED
        procedure DefineProperties; override;
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

        procedure SetMultArray;
        function HarmArrayHasaZero(var zeropoint: Integer): Boolean;

    PUBLIC
        NumHarm: Integer;          // Public so solution can get to it.
        HarmArray: pDoubleArray;

        constructor Create(ParClass: TDSSClass; const SpectrumName: String);
        destructor Destroy; OVERRIDE;
        procedure MakeLike(OtherPtr: Pointer); override;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;

        function GetMult(const h: Double): Complex;

        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
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
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer;    

procedure DoCSVFile(Obj: TObj; const FileName: String);forward;

constructor TSpectrum.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

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

procedure TSpectrum.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    NumProperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // strings
    PropertyType[ord(TProp.csvfile)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.csvfile)] := ptruint(@obj.csvfile);
    PropertyFlags[ord(TProp.csvfile)] := [TPropertyFlag.IsFilename];

    // integer properties
    PropertyType[ord(TProp.NumHarm)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.NumHarm)] := ptruint(@obj.NumHarm);

    // double arrays
    PropertyType[ord(TProp.harmonic)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.harmonic)] := ptruint(@obj.HarmArray);
    PropertyOffset2[ord(TProp.harmonic)] := ptruint(@obj.NumHarm);

    PropertyType[ord(TProp.angle)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.angle)] := ptruint(@obj.AngleArray);
    PropertyOffset2[ord(TProp.angle)] := ptruint(@obj.NumHarm);

    PropertyType[ord(TProp.pctmag)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.pctmag)] := ptruint(@obj.puMagArray);
    PropertyOffset2[ord(TProp.pctmag)] := ptruint(@obj.NumHarm);
    PropertyScale[ord(TProp.pctmag)] := 0.01;

    ActiveProperty := NumPropsThisClass;
    inherited;
end;

function TSpectrum.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TSpectrumObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
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
                AngleArray^[i] := 0.0; //TODO: remove -- left for backwards compatiblity, but this is kinda buggy
        end;
        ord(TProp.csvfile):
            DoCSVFile(self, csvfile);
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TSpectrum.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
var
    iZeroPoint: Integer;  // for error trapping
begin
    with TObj(ptr) do
    begin
        if (HarmArray <> NIL) then   // Check this after HarmArray is allocated  2/20/2018
        begin
            if HarmArrayHasaZero(iZeroPoint) then
                DoSimpleMsg('Error: Zero frequency detected in %s, point %d. Not allowed', [FullName, iZeroPoint], 65001)

            else
            if (HarmArray <> NIL) and (puMagArray <> NIL) and (AngleArray <> NIL) then
                SetMultArray;
        end;
        Exclude(Flags, Flg.EditionActive);
    end;
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

    ReallocMem(HarmArray, Sizeof(HarmArray^[1]) * NumHarm);
    ReallocMem(puMagArray, Sizeof(puMagArray^[1]) * NumHarm);
    ReallocMem(AngleArray, Sizeof(AngleArray^[1]) * NumHarm);

    for i := 1 to NumHarm do
    begin
        HarmArray^[i] := Other.HarmArray^[i];
        puMagArray^[i] := Other.puMagArray^[i];
        AngleArray^[i] := Other.AngleArray^[i];
    end;
end;

constructor TSpectrumObj.Create(ParClass: TDSSClass; const SpectrumName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(SpectrumName);
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

procedure DoCSVFile(Obj: TObj; const FileName: String);
var
    F: TStream = nil;
    i: Integer;
    s: String;
begin
    with Obj do
    begin
        try
            F := DSS.GetROFileStream(FileName);
        except
            DoSimpleMsg('Error Opening CSV File: "%s"', [FileName], 653);
            FreeAndNil(F);
            Exit;
        end;

        try
            ReAllocmem(HarmArray, Sizeof(HarmArray^[1]) * NumHarm);
            ReAllocmem(puMagArray, Sizeof(puMagArray^[1]) * NumHarm);
            ReAllocmem(AngleArray, Sizeof(AngleArray^[1]) * NumHarm);
            i := 0;
            while ((F.Position + 1) <> F.Size) and (i < NumHarm) do
            begin
                Inc(i);
                FSReadln(F, S);  // Use Auxparser, which allows for formats
                with DSS.AuxParser do
                begin
                    CmdString := S;
                    NextParam;
                    HarmArray^[i] := DblValue;
                    NextParam;
                    puMagArray^[i] := DblValue * 0.01;
                    NextParam;
                    AngleArray^[i] := DblValue;
                end;
            end;
            F.Free();
            NumHarm := i;   // reset number of points
        except
            On E: Exception do
            begin
                DoSimpleMsg('Error Processing CSV File: "%s". %s', [FileName, E.Message], 654);
                F.Free();
                Exit;
            end;
        end;
    end;
end;

procedure TSpectrumObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);

    if Complete then
    begin
        FSWriteln(F, 'Multiplier Array:');
        FSWriteln(F, 'Harmonic, Mult.re, Mult.im, Mag,  Angle');
        for i := 1 to NumHarm do
        begin
            FSWrite(F, Format('%-g', [HarmArray^[i]]), ', ');
            FSWrite(F, Format('%-g, %-g, ', [MultArray^[i].re, MultArray^[i].im]));
            FSWrite(F, Format('%-g, %-g', [Cabs(MultArray^[i]), Cdang(MultArray^[i])]));
            FSWriteln(F);
        end;
    end;
end;

function TSpectrumObj.GetMult(const h: Double): Complex;
var
    i: Integer;
begin
     {Search List for  harmonic (nearest 0.01 harmonic) and return multiplier}
    for i := 1 to NumHarm do
    begin
        if Abs(h - HarmArray^[i]) < 0.01 then
        begin
            Result := MultArray^[i];
            Exit;
        end; {IF}
    end; {For}

     {None Found, return zero}
    Result := cZERO;
end;

function TSpectrumObj.HarmArrayHasaZero(var ZeroPoint: Integer): Boolean;
var
    i: Integer;
begin
    Result := FALSE;
    ZeroPoint := 0;
    for i := 1 to NumHarm do
        if HarmArray^[i] = 0.0 then
        begin
            Result := TRUE;
            ZeroPoint := i;
            Break;
        end;
end;

procedure TSpectrumObj.SetMultArray;
{Rotate all phase angles so that the fundamental is at zero}
var
    i: Integer;
    FundAngle: Double;

begin
    try

        FundAngle := 0.0;
        for i := 1 to NumHarm do
        begin
            if Round(HarmArray^[i]) = 1 then
            begin
                FundAngle := AngleArray^[i];
                Break;
            end;
        end;

        Reallocmem(MultArray, Sizeof(MultArray^[1]) * NumHarm);
        for i := 1 to NumHarm do
            MultArray^[i] := pdegtocomplex(puMagArray^[i], (AngleArray^[i] - HarmArray^[i] * FundAngle));

    except
        DoSimpleMsg('Exception while computing %s. Check Definition. Aborting', [FullName], 655);
        if DSS.In_Redirect then
            DSS.Redirect_Abort := TRUE;
    end;
end;

initialization
    PropInfo := NIL;
end.
