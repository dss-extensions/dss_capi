unit DSSGraph;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
   Interface to DSSView Program

   Writes some files with the appropriate info and then invokes DSSView.exe

   Adapted from interface to old DSSGraph.DLL
}

interface

uses
    ArrayDef,
    Graphics,
    Sysutils;

type
    GridStyleType = (gsNone, gsPoints, gsVertLines, gsHorizLines, gsLines, gsHorizDotLines, gsVertDotLines, gsDotLines);
    EDSSGraphProblem = class(Exception);

    TDSSGraphProperties = packed record
        Xmin: Double;
        Xmax: Double;
        Ymin: Double;
        YMax: Double;
        ChartColor: TColor;
        WindColor: TColor;
        Isometric: Boolean;
        GridStyle: GridStyleType;
    end;


//    Procedure DSSGraphInit(ptrCallBackStruct:pDSSCallBacks); StdCall;  external 'DSSGraph.dll' name 'Init';  // Call this once
function MakeNewGraph(const Filename: String): Integer;      // Call this to make a new graph

procedure AddNewLine(X1, Y1, X2, Y2: Double; Color: TColor; Thickness: Byte; Style: TPenStyle; Dots: Boolean; const LineName: String;
    MarkCenter: Boolean; CenterMarkerCode, NodeMarkerCode, NodeMarkerWidth: Integer);
procedure AddNewCurve(Xarray, Yarray: pDoubleArray; NumPoints: Integer; Color: TColor; Thickness: Byte; Style: TPenStyle;
    Curvemarkers: Boolean; CurveMarker: Integer; const CurveName: String);
procedure AddNewText(X1, Y1: Double; Color: TColor; Size: Integer; S: String);
procedure AddNewCircle(Xc, Yc, Radius: Double; LineColor, FColor: TColor);
procedure AddNewMarker(X, Y: Double; Color: TColor; Symbol, Size: Byte);

   {Routines to manage DSSGraph Properties. }
   {Invoke a Get_Properties to populate the Props struct first then change values}
procedure Set_Properties(var Props: TDSSGraphProperties);
procedure Get_Properties(var Props: TDSSGraphProperties);
procedure Set_XaxisLabel(s: String);
procedure Set_YaxisLabel(s: String);
procedure Set_Caption(s: String);
procedure Set_ChartCaption(s: String);

procedure Set_LineWidth(Width: Integer);
procedure Set_AutoRange(PctRim: Double);
procedure Set_KeepAspectRatio(Value: Boolean);
procedure Set_DataColor(clr: TColor);
procedure Set_TextAlignment(Option: Integer);
procedure Set_KeyClass(Value: Integer);
procedure Set_Range(LoX, HiX, LoY, HiY: Double);
procedure Set_FontStyle(Fs: TFontStyle);
procedure Set_NoScales;
procedure Set_LeftJustifyTransparent(LabelIdx: Integer);
procedure MoveTo(X, Y: Double);
procedure DrawTo(X, Y: Double);
procedure Rectangle(x1, y1, x2, y2: Double);
procedure EnableClickonDiagram;
procedure ShowGraph;
function AddTextLabel(X, Y: Double; TextColor: TColor; Txt: String; Template: Integer): Integer;
procedure LockInTextLabel(idx: Integer);
procedure BoldTextLabel(idx: Integer);
procedure MarkAt(x, y: Double; Marker, Size: Byte);
procedure CenteredText15(x, y: Double; size: Integer; txt: String);


implementation

uses
    Windows,
    DSSGlobals,
    Utilities,
    ShellAPI,
    Math,
    PDElement,
    ParserDel;

var
    ActiveDSSGraphFile: TextFile;
    ActiveSolutionFileHdl: Integer;
    ActiveFileName,
    ActiveSolutionFileName: String;
    ActiveGraphProps: TDSSGraphProperties;
    TextLabelCount: Integer;

procedure Checkminmax(const x, y: Double);
begin
    with ActiveGraphProps do
    begin
        XMin := Min(X, Xmin);
        Xmax := Max(X, Xmax);
        YMin := Min(Y, Ymin);
        Ymax := Max(Y, Ymax);
    end;
end;

function MakeNewGraph(const Filename: String): Integer;      // Call this to make a new graph file
begin

    Result := 0;
    ActiveFileName := '';
    try
        AssignFile(ActiveDSSGraphFile, FileName);
        Rewrite(ActiveDSSGraphFile);
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error opening DSSView file: ' + Filename + ', ' + E.message, 45001);
            Exit;
        end;
    end;

    ActiveFileName := FileName;
    ActiveSolutionFileName := ChangeFileExt(ActiveFileName, '.dbl');

    try
        ActiveSolutionFileHdl := Sysutils.FileCreate(ActiveSolutionFileName);
        if ActiveSolutionFileHdl < 0 then
        begin
            DoSimpleMsg('Error occured opening DSSView binary Solution file: ' + Filename, 45001);
            CloseFile(ActiveDSSGraphFile);
            Exit;
        end;

    except
        On E: Exception do
        begin
            DoSimpleMsg('Error opening DSSView Solution file: ' + Filename + ', ' + E.message, 45001);
            Exit;
        end;
    end;


    with  ActiveGraphProps do
    begin
        Xmin := 1.0e50;
        Xmax := -1.0e50;
        Ymin := 1.0e50;
        YMax := -1.0e50;
        ChartColor := clWhite;
        WindColor := clWhite;
        Isometric := FALSE;
        GridStyle := gsLines;
    end;

    TextLabelCount := 0;

    Set_Properties(ActiveGraphProps);

    Result := 1;


end;

function WriteActiveCktElementVIToFile: Int64;
var
    Count: Cardinal;
    CountWritten: Cardinal;
begin
  // get present file position
    Result := sysutils.FileSeek(ActiveSolutionFileHdl, Int64(0), 1);

    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement.ComputeVterminal(ActiveActor);
        ActiveCktElement.ComputeIterminal(ActiveActor);
        Count := ActiveCktElement.Yorder * 2 * Sizeof(Double);
        CountWritten := Sysutils.FileWrite(ActiveSolutionFileHdl, ActiveCktElement.Vterminal^, Count);
        if CountWritten = Count then
            CountWritten := Sysutils.FileWrite(ActiveSolutionFileHdl, ActiveCktElement.Iterminal^, Count);
    end;

    if CountWritten <> Count then
    begin
        Sysutils.FileClose(ActiveSolutionFileHdl);
        CloseFile(ActiveDSSGraphFile);
        raise EDSSGraphProblem.Create('Aborting. Problem writing solution file: ' + ActiveSolutionFileName);
    end;

end;

procedure AddNewLine(X1, Y1, X2, Y2: Double; Color: TColor; Thickness: Byte; Style: TPenStyle; Dots: Boolean; const LineName: String;
    MarkCenter: Boolean; CenterMarkerCode, NodeMarkerCode, NodeMarkerWidth: Integer);
var
    Offset: Int64;
    Bus1, Bus2: String;
    Bus1Idx: Integer;
    DataCount: Integer;
    kV_Base: Double;
    Dist: Double;
    pDElem: TPDElement;
    NumCust, TotalCust: Integer;

begin
    Offset := WriteActiveCktElementVIToFile;

    with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].ActiveCktElement do
    begin
        Bus1Idx := Terminals^[1].BusRef;
        kV_Base := Buses^[Bus1Idx].kVBase;
        Dist := Buses^[Bus1Idx].DistFromMeter;
        Bus1 := GetBus(1);
        Bus2 := GetBus(2);
        DataCount := Yorder;
    end;

    if ActiveCircuit[ActiveActor].ActiveCktElement is TPDElement then
    begin
        pDElem := ActiveCircuit[ActiveActor].ActiveCktElement as TPDElement;
        NumCust := pDElem.BranchNumCustomers;
        TotalCust := pDElem.BranchTotalCustomers;
    end
    else
    begin
        NumCust := 0;
        TotalCust := 0;
    end;


    Writeln(ActiveDSSGraphFile, Format('Line, "%s", "%s", "%s", %d, %d,  %d, %d, %.8g, %.8g, %.8g, %.8g, %.8g, %.8g, %d, %d, %d, %d, %d, %d, %d, %d',
        [LineName, Bus1, Bus2, Offset, DataCount, NumCust, TotalCust, kV_Base, Dist,
        X1, Y1, X2, Y2,
        Color, Thickness, Ord(Style), Ord(Dots), Ord(MarkCenter), CentermarkerCode, NodeMarkerCode, NodeMarkerWidth]));
    CheckMinMax(X1, Y1);
    CheckMinMax(X2, Y2);
end;

procedure AddNewCurve(Xarray, Yarray: pDoubleArray; NumPoints: Integer; Color: TColor; Thickness: Byte; Style: TPenStyle;
    Curvemarkers: Boolean; CurveMarker: Integer; const CurveName: String);
var
    i: Integer;

begin
    Write(ActiveDSSGraphFile, Format('Curve, %d, %d, %d, %d, %d, %d, "%s"',
        [NumPoints, Color, Thickness, ord(Style), ord(CurveMarkers), CurveMarker, CurveName]));
    for i := 1 to NumPoints do
        Write(ActiveDSSGraphFile, Format(', %.8g', [Xarray^[i]]));
    for i := 1 to NumPoints do
        Write(ActiveDSSGraphFile, Format(', %.8g', [Yarray^[i]]));
    Writeln(ActiveDSSGraphFile);

     {???? Check min and max of curve}
end;

procedure AddNewText(X1, Y1: Double; Color: TColor; Size: Integer; S: String);
begin
    Writeln(ActiveDSSGraphFile, Format('Text, %.8g, %.8g, %d, %d, "%s"', [X1, Y1, Color, Size, S]));
    CheckMinMax(X1, Y1);
end;

procedure AddNewCircle(Xc, Yc, Radius: Double; LineColor, FColor: TColor);
begin
    Writeln(ActiveDSSGraphFile, Format('Circle, %.8g, %.8g, %.8g, %d, %d',
        [Xc, Yc, Radius, LineColor, FColor]));
    CheckMinMax(Xc, Yc);
end;

procedure AddNewMarker(X, Y: Double; Color: TColor; Symbol, Size: Byte);
begin
    Writeln(ActiveDSSGraphFile, Format('Marker, %.8g, %.8g,  %d, %d, %d', [X, Y, color, Symbol, Size]));
    CheckMinMax(X, Y);
end;

{Routines to manage DSSGraph Properties. }
{Invoke a Get_Properties to populate the Props struct first then change values}
procedure Set_Properties(var Props: TDSSGraphProperties);
begin
    ActiveGraphProps := Props;
    with ActiveGraphProps do
        Writeln(ActiveDSSGraphFile, Format('SetProp, %.8g, %8g, %.8g, %.8g, %d, %d, %d, %d',
            [Xmin, Xmax, Ymin, YMax, ChartColor, WindColor, Ord(Isometric), Ord(GridStyle)]));
end;

procedure Get_Properties(var Props: TDSSGraphProperties);
begin
    Props := ActiveGraphProps;
end;

procedure Set_XaxisLabel(s: String);
begin
    Writeln(ActiveDSSGraphFile, Format('Xlabel, "%s"', [s]));
end;

procedure Set_YaxisLabel(s: String);
begin
    Writeln(ActiveDSSGraphFile, Format('Ylabel, "%s"', [s]));
end;

procedure Set_Caption(s: String);
begin
    Writeln(ActiveDSSGraphFile, Format('Caption, "%s"', [s]));
end;

procedure Set_ChartCaption(s: String);
begin
    Writeln(ActiveDSSGraphFile, Format('ChartCaption, "%s"', [s]));
end;

procedure Set_LineWidth(Width: Integer);
begin
    Writeln(ActiveDSSGraphFile, Format('Width, %d', [Width]));
end;

procedure Set_AutoRange(PctRim: Double);
begin
    Writeln(ActiveDSSGraphFile, Format('PctRim, %.8g', [PctRim]));
end;

procedure Set_KeepAspectRatio(Value: Boolean);
begin

    Writeln(ActiveDSSGraphFile, Format('KeepAspect, %d', [Ord(Value)]));
end;

procedure Set_DataColor(clr: TColor);
begin
    Writeln(ActiveDSSGraphFile, Format('DataColor, %d', [clr]));
end;

procedure Set_TextAlignment(Option: Integer);
begin
    Writeln(ActiveDSSGraphFile, Format('TxtAlign, %d', [Option]));
end;

procedure Set_KeyClass(Value: Integer);
begin
    Writeln(ActiveDSSGraphFile, Format('KeyClass, %d', [Value]));
end;

procedure Set_Range(LoX, HiX, LoY, HiY: Double);
begin
    Writeln(ActiveDSSGraphFile, Format('Range, %.8g, %.8g, %.8g, %.8g', [LoX, HiX, LoY, HiY]));
end;

procedure Set_FontStyle(Fs: TFontStyle);
begin
    Writeln(ActiveDSSGraphFile, Format('FStyle, %d', [Ord(Fs)]));
end;

procedure Set_NoScales;
begin
    Writeln(ActiveDSSGraphFile, 'NoScales,');
end;

procedure Set_LeftJustifyTransparent(LabelIdx: Integer);
begin
    Writeln(ActiveDSSGraphFile, Format('LJust, %d', [LabelIdx]));
end;


procedure MoveTo(X, Y: Double);
begin
    Writeln(ActiveDSSGraphFile, Format('Move, %.8g, %.8g', [X, Y]));
end;

procedure DrawTo(X, Y: Double);
begin
    Writeln(ActiveDSSGraphFile, Format('Draw, %.8g, %.8g', [X, Y]));
end;

procedure Rectangle(x1, y1, x2, y2: Double);
begin
    Writeln(ActiveDSSGraphFile, Format('Rect, %.8g, %.8g, %.8g, %.8g', [x1, y1, x2, y2]));
end;

procedure EnableClickonDiagram;
begin
    Writeln(ActiveDSSGraphFile, 'ClickOn');
end;

function IsOpen(const txt: TextFile): Boolean;
const
    fmTextOpenRead = 55217;
    fmTextOpenWrite = 55218;
begin
    Result := (TTextRec(txt).Mode = fmTextOpenRead) or (TTextRec(txt).Mode = fmTextOpenWrite)
end;

procedure ShowGraph;
var
    retval: Integer;
    DSSViewFile: String;
begin

    if IsOpen(ActiveDSSGraphFile) then
    begin

        CloseFile(ActiveDSSGraphFile);
        Sysutils.FileClose(ActiveSolutionFileHdl);

        try
            if FileExists(ActiveFileName) then
            begin
                DSSViewFile := EncloseQuotes(DSSDirectory + 'DSSView.exe');
                retval := ShellExecute(0, 'open',
                    Pchar(DSSViewFile),
                    Pchar(EncloseQuotes(ActiveFileName)),
                    NIL, SW_SHOW);

                ParserVars.Add('@LastPlotFile', ActiveFileName);

                case Retval of
                    0:
                        DoSimpleMsg('System out of memory. ', 45700);
                    ERROR_BAD_FORMAT:
                        DoSimpleMsg('Graphics output file "' + ActiveFileName + '" is Invalid.', 45701);
                    ERROR_FILE_NOT_FOUND:
                        DoSimpleMsg(DSSViewfile + ' File  Not Found.' + CRLF + 'It should be in the same directory as the OpenDSS program', 45702);
                    ERROR_PATH_NOT_FOUND:
                        DoSimpleMsg('Path for DSSView program "' + DSSViewFile + '" Not Found.', 45703);
                end;
            end;
        except
            On E: Exception do
                DoErrorMsg('ShowGraph.', E.Message,
                    'Is DSSView.EXE correctly installed???', 45704);
        end;

        GlobalResult := ActiveFileName;
    end;

end;

function AddTextLabel(X, Y: Double; TextColor: TColor; Txt: String; Template: Integer): Integer;
begin
    Writeln(ActiveDSSGraphFile, Format('Label, %.8g, %.8g, %d, "%s", %d', [X, Y, Textcolor, Txt, Template]));
    Inc(TextLabelCount);
    Result := TextLabelCount;
    CheckMinMax(X, Y);
end;

procedure LockInTextLabel(idx: Integer);
begin
    Writeln(ActiveDSSGraphFile, Format('LockInLabel, %d', [idx]));
end;

procedure BoldTextLabel(idx: Integer);
begin
    Writeln(ActiveDSSGraphFile, Format('BoldLabel, %d', [idx]));
end;

procedure MarkAt(x, y: Double; Marker, Size: Byte);
begin
    Writeln(ActiveDSSGraphFile, Format('MarkAt,  %.8g, %.8g, %d, %d', [x, y, Marker, Size]));
    CheckMinMax(X, Y);
end;

procedure CenteredText15(x, y: Double; size: Integer; txt: String);
begin
    Writeln(ActiveDSSGraphFile, Format('Center,  %.8g, %.8g, %d, "%s"', [x, y, Size, txt]));
    CheckMinMax(X, Y);
end;


end.
