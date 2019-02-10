unit Spectrum;

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{ Created 10/25/00

   Harmonic Spectrum specified as Harmonic, pct magnitude and angle

   Spectrum is shifted by the fundamental angle and stored in MultArray
   so that the fundamental is at zero degrees phase shift

}

interface

uses
    Command,
    DSSClass,
    DSSObject,
    Arraydef,
    ucomplex;

type

    TSpectrum = class(TDSSClass)
    PRIVATE

        function Get_Code: String;  // Returns active spectrum code string
        procedure Set_Code(const Value: String);  // sets the  active Spectrum
        procedure DoCSVFile(const FileName: String);

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const LineName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

       // Set this property to point ActiveSpectrumObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;

    end;

    TSpectrumObj = class(TDSSObject)
    PRIVATE
        puMagArray,
        AngleArray: pDoubleArray;
        MultArray: pComplexArray;

        procedure SetMultArray;
        function HarmArrayHasaZero(var zeropoint: Integer): Boolean;

    PUBLIC
        NumHarm: Integer;          // Public so solution can get to it.
        HarmArray: pDoubleArray;

        constructor Create(ParClass: TDSSClass; const SpectrumName: String);
        destructor Destroy; OVERRIDE;

        function GetMult(const h: Double): Complex;

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;


    end;

var
    ActiveSpectrumObj: TSpectrumObj;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Utilities;

const
    NumPropsThisClass = 5;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TSpectrum.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'Spectrum';
    DSSClassType := DSS_OBJECT;
    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TSpectrum.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TSpectrum.DefineProperties;
begin

    NumProperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


    PropertyName[1] := 'NumHarm';
    PropertyName[2] := 'harmonic';
    PropertyName[3] := '%mag';
    PropertyName[4] := 'angle';
    PropertyName[5] := 'CSVFile';

    PropertyHelp[1] := 'Number of frequencies in this spectrum. (See CSVFile)';
    PropertyHelp[2] := 'Array of harmonic values. You can also use the syntax' + CRLF +
        'harmonic = (file=filename)     !for text file one value per line' + CRLF +
        'harmonic = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'harmonic = (sngfile=filename)  !for packed file of singles ';
    PropertyHelp[3] := 'Array of magnitude values, assumed to be in PERCENT. You can also use the syntax' + CRLF +
        '%mag = (file=filename)     !for text file one value per line' + CRLF +
        '%mag = (dblfile=filename)  !for packed file of doubles' + CRLF +
        '%mag = (sngfile=filename)  !for packed file of singles ';
    PropertyHelp[4] := 'Array of phase angle values, degrees.You can also use the syntax' + CRLF +
        'angle = (file=filename)     !for text file one value per line' + CRLF +
        'angle = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'angle = (sngfile=filename)  !for packed file of singles ';
    PropertyHelp[5] := 'File of spectrum points with (harmonic, magnitude-percent, angle-degrees) values, one set of 3 per line, in CSV format. ' +
        'If fewer than NUMHARM frequencies found in the file, NUMHARM is set to the smaller value.';


    ActiveProperty := NumPropsThisClass;
    inherited;  // Add defs of inherited properties to bottom of list

end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TSpectrum.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveDSSObject[ActiveActor] := TSpectrumObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TSpectrum.Edit(ActorID: Integer): Integer;
var
    i,
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    iZeroPoint: Integer;  // for error trapping

begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveSpectrumObj := ElementList.Active;
    ActiveDSSObject[ActorID] := ActiveSpectrumObj;

    with ActiveSpectrumObj do
    begin

        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Name + '"', 650);
                1:
                begin
                    NumHarm := Parser[ActorID].IntValue;
                    ReAllocmem(AngleArray, Sizeof(AngleArray^[1]) * NumHarm); // Make a dummy Angle array
                    for i := 1 to NumHarm do
                        AngleArray^[i] := 0.0;
                end;
                2:
                begin
                    ReAllocmem(HarmArray, Sizeof(HarmArray^[1]) * NumHarm);
                    InterpretDblArray(Param, NumHarm, HarmArray);
                end;
                3:
                begin
                    ReAllocmem(puMagArray, Sizeof(puMagArray^[1]) * NumHarm);
                    InterpretDblArray(Param, NumHarm, puMagArray);
                    for i := 1 to NumHarm do
                        puMagArray^[i] := puMagArray^[i] * 0.01;  // convert to per unit
                end;
                4:
                begin
                    ReAllocmem(AngleArray, Sizeof(AngleArray^[1]) * NumHarm);
                    InterpretDblArray(Param, NumHarm, AngleArray);
                end;
                5:
                    DoCSVFile(Param);
            else
          // Inherited parameters
                ClassEdit(ActiveSpectrumObj, Parampointer - NumPropsThisClass)
            end;


            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;       {WHILE}

        if (HarmArray <> NIL) then   // Check this after HarmArray is allocated  2/20/2018
        begin
            if HarmArrayHasaZero(iZeroPoint) then
                DoSimpleMsg(Format('Error: Zero frequency detected in Spectrum.%s, point %d. Not allowed', [Name, iZeroPoint]), 65001)
            else
            if (HarmArray <> NIL) and (puMagArray <> NIL) and (AngleArray <> NIL) then
                SetMultArray;
        end;

    end; {WITH}

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TSpectrum.MakeLike(const LineName: String): Integer;
var
    OtherSpectrum: TSpectrumObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this line code in the present collection}
    OtherSpectrum := Find(LineName);
    if OtherSpectrum <> NIL then
        with ActiveSpectrumObj do
        begin

            NumHarm := OtherSpectrum.NumHarm;

            ReallocMem(HarmArray, Sizeof(HarmArray^[1]) * NumHarm);
            ReallocMem(puMagArray, Sizeof(puMagArray^[1]) * NumHarm);
            ReallocMem(AngleArray, Sizeof(AngleArray^[1]) * NumHarm);

            for i := 1 to NumHarm do
            begin
                HarmArray^[i] := OtherSpectrum.HarmArray^[i];
                puMagArray^[i] := OtherSpectrum.puMagArray^[i];
                AngleArray^[i] := OtherSpectrum.AngleArray^[i];
            end;

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherSpectrum.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in Spectrum MakeLike: "' + LineName + '" Not Found.', 651);


end;


function TSpectrum.Get_Code: String;  // Returns active line code string
var
    SpectrumObj: TSpectrumObj;

begin

    SpectrumObj := ElementList.Active;
    Result := SpectrumObj.Name;

end;

procedure TSpectrum.Set_Code(const Value: String);  // sets the  active Spectrum
var
    SpectrumObj: TSpectrumObj;
begin

    ActiveSpectrumObj := NIL;
    SpectrumObj := ElementList.First;
    while SpectrumObj <> NIL do
    begin

        if CompareText(SpectrumObj.Name, Value) = 0 then
        begin
            ActiveSpectrumObj := SpectrumObj;
            Exit;
        end;

        SpectrumObj := ElementList.Next;
    end;

    DoSimpleMsg('Spectrum: "' + Value + '" not Found.', 652);

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TSpectrum Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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


    InitPropertyValues(0);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TSpectrumObj.Destroy;
begin
    Reallocmem(HarmArray, 0);
    Reallocmem(puMagArray, 0);
    Reallocmem(AngleArray, 0);
    Reallocmem(MultArray, 0);
    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TSpectrum.DoCSVFile(const FileName: String);

var
    F: Textfile;
    i: Integer;
    s: String;

begin
    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening CSV File: "' + FileName, 653);
        CloseFile(F);
        Exit;
    end;

    try

        with ActiveSpectrumObj do
        begin
            ReAllocmem(HarmArray, Sizeof(HarmArray^[1]) * NumHarm);
            ReAllocmem(puMagArray, Sizeof(puMagArray^[1]) * NumHarm);
            ReAllocmem(AngleArray, Sizeof(AngleArray^[1]) * NumHarm);
            i := 0;
            while (not EOF(F)) and (i < NumHarm) do
            begin
                Inc(i);
                Readln(F, S);  // Use Auxparser, which allows for formats
                with AuxParser[ActiveActor] do
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
            CloseFile(F);
            if i <> NumHarm then
                NumHarm := i;   // reset number of points
        end;

    except
        On E: Exception do
        begin
            DoSimpleMsg('Error Processing CSV File: "' + FileName + '. ' + E.Message, 654);
            CloseFile(F);
            Exit;
        end;
    end;

end;


procedure TSpectrumObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, j: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            case i of
                2:
                begin
                    Write(F, '~ ', PropertyName^[i], '=(');
                    for j := 1 to NumHarm do
                        Write(F, Format('%-g, ', [HarmArray^[j]]));
                    Writeln(F, ')');
                end;
                3:
                begin
                    Write(F, '~ ', PropertyName^[i], '=(');
                    for j := 1 to NumHarm do
                        Write(F, Format('%-g, ', [puMagArray^[j] * 100.0]));
                    Writeln(F, ')');
                end;
                4:
                begin
                    Write(F, '~ ', PropertyName^[i], '=(');
                    for j := 1 to NumHarm do
                        Write(F, Format('%-g, ', [AngleArray^[j]]));
                    Writeln(F, ')');
                end;
            else
                Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
            end;
        end;

    if Complete then
    begin
        Writeln(F, 'Multiplier Array:');
        Writeln(F, 'Harmonic, Mult.re, Mult.im, Mag,  Angle');
        for i := 1 to NumHarm do
        begin
            Write(F, Format('%-g', [HarmArray^[i]]), ', ');
            Write(F, Format('%-g, %-g, ', [MultArray^[i].re, MultArray^[i].im]));
            Write(F, Format('%-g, %-g', [Cabs(MultArray^[i]), Cdang(MultArray^[i])]));
            Writeln(F);
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

function TSpectrumObj.GetPropertyValue(Index: Integer): String;
var
    i: Integer;
begin
    case Index of
        2..4:
            Result := '(';
    else
        Result := '';
    end;

    case Index of
        1:
            Result := IntToStr(NumHarm);
        2:
            for i := 1 to NumHarm do
                Result := Result + Format('%-g, ', [HarmArray^[i]]);
        3:
            for i := 1 to NumHarm do
                Result := Result + Format('%-g, ', [puMagArray^[i] * 100.0]);
        4:
            for i := 1 to NumHarm do
                Result := Result + Format('%-g, ', [AngleArray^[i]]);
    else
        Result := inherited GetPropertyValue(index);
    end;

    case Index of
        2..4:
            Result := Result + ')';
    else
    end;

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

procedure TSpectrumObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '0';
    PropertyValue[2] := '';
    PropertyValue[3] := '';
    PropertyValue[4] := '';
    PropertyValue[5] := '';

    inherited InitPropertyValues(NumPropsThisClass);

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
        DoSimpleMsg('Exception while computing Spectrum.' + Name + '. Check Definition. Aborting', 655);
        if In_Redirect then
            Redirect_Abort := TRUE;
    end;

end;

end.
