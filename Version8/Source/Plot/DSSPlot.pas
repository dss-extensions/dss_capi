unit DSSPlot;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
  Unit for interfacing to Plotting form
  3/29/03
  8/13/2008
}
{$M+}
{$WARN UNIT_PLATFORM OFF}

interface

uses
    Line,
    Transformer,
    Graphics,
    Arraydef,
    Classes,
    Ucomplex,
    CktElement,
    DSSCallBackRoutines;

const
    vizCURRENT = 1;
    vizVOLTAGE = 2;
    vizPOWER = 3;

type
    TPlotType = (ptAutoAddLogPlot, ptCircuitplot, ptGeneralDataPlot,
        ptGeneralCircuitPlot, ptmonitorplot, ptdaisyplot, ptMeterZones,
        ptLoadShape, ptTShape, ptPriceShape, ptProfile, ptScatterPlot,
        ptEvolutionPlot, ptEnergyPlot, ptMatrixplot, ptPhaseVoltage);
    TPlotQuantity = (pqVoltage, pqCurrent, pqPower, pqLosses, pqCapacity,
        pqNone);
    TMatrixType = (pIncMatrix, pLaplacian); // The types of matrices that can be plotted

    TDSSPlot = class(TObject)
    PRIVATE
        ActiveColorIdx: Integer;
        ColorArray: array [1 .. 17] of Integer;
        pLine: TLineObj;
        pTransf: TTransfObj;
        Bus1Idx, Bus2Idx: Integer;
        FGeneralCircuitPlotQuantity: String;
        FMaxLineThickness: Integer;

      { Main procedures for the various types of plots ... called from execute }
        procedure DoGeneralPlot;
        procedure DoAutoAddPlot;
        procedure DoTheDaisies;
        procedure DoCircuitPlot;
        procedure DoGeneralCircuitPlot;
        procedure DoMeterZonePlot;
        procedure DoMonitorPlot;
        procedure DoProfilePlot;

      { Misc support procedures }
        procedure MarkSubTransformers;
        procedure MarkTheTransformers;
        procedure MarkTheCapacitors;
        procedure MarkTheRegulators;
        procedure MarkThePVSystems;
        procedure MarkTheStorage;
        procedure MarkTheFuses;
        procedure MarkTheReclosers;
        procedure MarkTheRelays;
        procedure MarkSpecialClasses;
        procedure DoBusLabels(const Idx1, Idx2: Integer);
        procedure DoBusLabel(const Idx: Integer; const BusLabel: String);
        procedure LabelBuses;
        procedure LoadGeneralLineData;
        procedure SetColorArray;
        procedure SetMaxScale;
        procedure AddBusMarkers;

        function GetColor: Integer;
        function Thickness: Integer;
        function MaxCurrent: Double;
        function NextColor: TColor;
        function QuantityString: String;
        function Style(Code: Integer): TPenStyle;
        function GetAutoColor(Scale: Double): TColor;
        function GetMarker(Idx: Integer): Byte;
        function CoordinateSame(i1, i2: Integer): Boolean;
        function InterpolateGradientColor(Color1, Color2: TColor;
            Ratio: Double): TColor;

      { Property support }
        procedure Set_MaxLineThickness(const Value: Integer);
    PROTECTED

    PUBLIC

        PlotType: TPlotType;
        MatrixType: TMatrixType;
        MaxScale, MinScale: Double;
        Dots, Labels, ShowLoops, { applies to Meterzone plots only }
        ShowSubs: Boolean;
        Quantity: TPlotQuantity;
        ObjectName, FeederName: String;
        PlotID: String;
        ValueIndex, MarkerIdx: Integer; { For General & AutoAdd }

        PhasesToPlot: Integer; // Profile Plot
        ProfileScale: Integer; // CYMDIST or pu/km scaling

        Channels: array of Cardinal; // for Monitor Plot
        Bases: array of Double; // for Monitor Plot

        Color1, Color2, Color3: TColor;

      { Tri-color plots }
        TriColorMax, TriColorMid: Double;

        MaxScaleIsSpecified: Boolean;
        MinScaleIsSpecified: Boolean;

        DaisyBusList: TStringList;

        constructor Create;
        destructor Destroy; OVERRIDE;

        procedure Execute;
        procedure SetDefaults;
        procedure DSSVizPlot;

        procedure DoLoadShapePlot(const LoadShapeName: String);
        procedure DoTempShapePlot(const TempShapeName: String);
        procedure DoPriceShapePlot(const PriceShapeName: String);
        procedure DoDI_Plot(const CaseName: String; CaseYear: Integer;
            iRegisters: array of Integer; PeakDay: Boolean;
            const MeterName: String);
        procedure DoCompareCases(CaseName1, CaseName2, WhichFile: String;
            Reg: Integer);
        procedure DoYearlyCurvePlot(CaseNames: TStringList; WhichFile: String;
            iRegisters: array of Integer);
        procedure DoVisualizationPlot(Element: TDSSCktElement; Quantity: Integer);

        property MaxLineThickness: Integer READ FMaxLineThickness WRITE Set_MaxLineThickness;

    PUBLISHED

    end;

    TGenPlotItem = class(TObject)
    PRIVATE

    PROTECTED

    PUBLIC
        Name: String;
        Value: Double;
        constructor Create;
        destructor Destroy; OVERRIDE;
    PUBLISHED

    end;

   { List of General Plot Items }
    TGenPlotItemList = class(TList)
    PUBLIC
        destructor Destroy; OVERRIDE;
    end;

var
    DSSPlotObj: TDSSPlot;
    SinglePhLineStyle: Integer;
    ThreePhLineStyle: Integer;

implementation

uses
    DSSGraph,
    TCP_IP,
    Comobj,
    DSSClassDefs,
    DssGlobals,
    Circuit,
    Generator,
    energyMeter,
    GICLine,
    utilities,
    LoadShape,
    Tempshape,
    PriceShape,
    SysUtils,
    math,
    Controls,
    DlgPlotOptions,
    Bus,
    Monitor,
    Capacitor,
    PVSystem,
    Storage,
    RegControl,
    Fuse,
    Recloser,
    Relay;

const
    Eps = 0.002;

const
    DSSG_LINECLASS = 1;
    DSSG_CIRCLECLASS = 2;
    DSSG_TEXTCLASS = 3;
    DSSG_MARKERCLASS = 4;
    DSSG_CURVECLASS = 5;
    DSSG_SUBSTCLASS = 6;

var
    BusLabels: pStringArray;

   {
     TDSSPlot
   }

procedure AllocateBusLabels;
var
    i: Integer;
begin
    BusLabels := Allocmem(Sizeof(BusLabels^[1]) * ActiveCircuit[ActiveActor].NumBuses);
    for i := 1 to ActiveCircuit[ActiveActor].NumBuses do
        BusLabels^[i] := '';
end;

procedure FreeBusLabels;
var
    i: Integer;
begin
   { Get rid of string pointed to by each element }
    for i := 1 to ActiveCircuit[ActiveActor].NumBuses do
        BusLabels^[i] := '';
    Reallocmem(BusLabels, 0);
end;

function TDSSPlot.GetAutoColor(Scale: Double): TColor;

{ One way to get a gradient }

begin
    if Scale < TriColorMid then
        Result := Color1
    else
    if Scale < TriColorMax then
        Result := Color2
    else
        Result := Color3;

end;

function TDSSPlot.GetColor: Integer;

var
    pBus: TDSSBus;
    Factor: Double;
    i, j: Integer;

begin
    case PlotType of
        ptCircuitplot, ptdaisyplot, ptGeneralCircuitPlot:
            case Quantity of
                pqVoltage:
                begin
                    pBus := ActiveCircuit[ActiveActor].Buses^[Bus2Idx];
                    if ActiveCircuit[ActiveActor].IsSolved and (pBus.kVBase > 0.0) then
                    begin
                     { Find min phase voltage at bus - check nodes 1..3 }
                        Factor := ActiveCircuit[ActiveActor].NormalMaxVolts;
                        for i := 1 to pBus.NumNodesThisBus do
                        begin
                            j := pBus.GetNum(i);
                            if (j > 0) and (j <= 3) then
                                Factor := Min(Factor,
                                    0.001 * Cabs
                                    (ActiveCircuit[ActiveActor].Solution.NodeV^[pBus.GetRef(i)]
                                    ) / pBus.kVBase);
                        end;
                        if Factor > ActiveCircuit[ActiveActor].NormalMinVolts then
                            Result := Color1
                        else
                        if Factor > ActiveCircuit[ActiveActor].EmergMinVolts then
                            Result := Color2
                        else
                            Result := Color3;
                    end
                    else
                        Result := Color1;
                end;
                pqCurrent:
                begin
                    Result := Color1;
                    if pLine.Normamps > 0.0 then
                        if MaxCurrent > pLine.Normamps then
                            Result := Color3;
                end;
                pqPower:
                    Result := Color1;
                pqLosses:
                    Result := Color1;
                pqCapacity:
                    Result := Color1;
            else { Case Quantity }
                Result := Color1; // Default to black
                if (abs(pLine.GeneralPlotQuantity) / MaxScale) > 0.99 then
                    Result := Color2;
            end;
    else { Case Plottype }
        Result := Color1; // Default to black
    end;
end;

procedure TDSSPlot.AddBusMarkers;

var
    BusMarker: TBusMarker;
    i: Integer;
    Bus: TDSSBus;

begin

    for i := 0 to ActiveCircuit[ActiveActor].BusMarkerList.Count - 1 do
    begin
        BusMarker := ActiveCircuit[ActiveActor].BusMarkerList.Items[i];
        Bus1Idx := ActiveCircuit[ActiveActor].BusList.Find(BusMarker.BusName);
        if Bus1Idx > 0 then
            with BusMarker do
            begin
                Bus := ActiveCircuit[ActiveActor].Buses^[Bus1Idx];
                if Bus.CoordDefined then
                    AddNewMarker(Bus.x, Bus.y, AddMarkerColor, AddMarkerCode, AddMarkerSize)
                else
                    DoSimpleMsg('Bus Coordinates not defined for bus ' + Busname, 28709);

            end;
    end;

end;

function TDSSPlot.CoordinateSame(i1, i2: Integer): Boolean;

begin
    Result := FALSE;
    if (i1 = 0) or (i2 = 0) then
        Exit;
    try { Trap Divide by zero error }
        with ActiveCircuit[ActiveActor] do
            if (abs(1.0 - abs(Buses^[i1].X / Buses^[i2].X)) < Eps) and
                (abs(1.0 - abs(Buses^[i1].Y / Buses^[i2].Y)) < Eps) then
                Result := TRUE
            else
                Result := FALSE;
    except
        Result := FALSE; { Likely a divide by zero error, ignore }
    end;
end;

constructor TDSSPlot.Create;
begin
    SetDefaults;
    DaisyBusList := TStringList.Create;
   { Initialize Plotting DLL }
   // --deprecated --- DSSGraphInit(@CallBackRoutines);  // send a pointer to the DSS Callback routines struct
    PhasesToPlot := PROFILE3PH;
    ProfileScale := PROFILEPUKM;
end;

destructor TDSSPlot.Destroy;
begin

    inherited;

end;

procedure TDSSPlot.DoAutoAddPlot;
var
    Color1Save: TColor;
begin
    Color1Save := Color1;
    Dots := FALSE;
    Quantity := pqNone;
    Color1 := clBlue;
    DoCircuitPlot;
    Color1 := Color1Save;
    DoGeneralPlot;
end;

procedure TDSSPlot.DoBusLabel(const Idx: Integer; const BusLabel: String);

begin
   { Only label a bus once }
    if Idx > 0 then
        if Length(BusLabels^[Idx]) = 0 then
            case PlotType of
                ptMeterZones:
                    BusLabels^[Idx] := BusLabel + '(' + FeederName + ')';
            else
                BusLabels^[Idx] := BusLabel;
            end;
end;

procedure TDSSPlot.DoBusLabels(const Idx1, Idx2: Integer);

begin
    if CoordinateSame(Idx1, Idx2) then
    begin
      { Special label for overlapping labels }
        BusLabels^[Idx1] := ''; // Force label to change
        BusLabels^[Idx2] := '';
        DoBusLabel(Idx1,
            ActiveCircuit[ActiveActor].BusList.Get(Idx1) + '/' + ActiveCircuit[ActiveActor].BusList.Get
            (Idx2));
    end
    else
    begin
        DoBusLabel(Idx1, ActiveCircuit[ActiveActor].BusList.Get(Idx1));
        DoBusLabel(Idx2, ActiveCircuit[ActiveActor].BusList.Get(Idx2));
    end;
end;

procedure TDSSPlot.DoCircuitPlot;

var
    LineStyleType: TPenStyle;
    pGICLine: TGICLineObj;
    pGICLineClass: TGICLine;
    GICThickness: Integer;

   { ******************  Code for GICLines ************************** }
    function MaxGICCurrent: Double;
    var
        iGIC: Integer;
    begin
        pGICLine.ComputeIterminal(ActiveActor); // load element Iterminal buffer
        Result := 0.0;
        for iGIC := 1 to pGICLine.NPhases do
            if Cabs(pGICLine.Iterminal^[iGIC]) > Result then
                Result := Cabs(pGICLine.Iterminal^[iGIC]);
    end;

{ ******************  Code for GICLines ************************** }

begin

   { Draw the lines }
    pLine := ActiveCircuit[ActiveActor].Lines.First;

    while pLine <> NIL do
        with ActiveCircuit[ActiveActor] do
        begin
            if pLine.Enabled then
            begin
                pLine.Drawn := TRUE;
            // Idx1 := Buslist.Find(StripExtension(pLine.GetBus (1)));
            // Idx2 := Buslist.Find(StripExtension(pLine.GetBus (2)));
                ActiveCktElement := pLine;
                Bus1Idx := pLine.Terminals^[1].BusRef;
                Bus2Idx := pLine.Terminals^[2].BusRef;
                if Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx]
                    .CoordDefined then
                begin
                    if pLine.IsSwitch then
                        AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                            Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, clBlack, 1,
                            Style(1), Dots, 'Line.' + pLine.Name, MarkSwitches,
                            SwitchMarkerCode, NodeMarkerCode, NodeMarkerWidth)

                    else
                    if pLine.IsIsolated then
                        AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                            Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, clFuchsia, 3,
                            Style(1), Dots, 'Line.' + pLine.Name, MarkSwitches,
                            SwitchMarkerCode, NodeMarkerCode, NodeMarkerWidth)
                    else
                    begin
                        if pLine.NPhases = 1 then
                            LineStyleType := Style(SinglePhLineStyle)
                        else
                            LineStyleType := Style(ThreePhLineStyle);
                        AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                            Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, GetColor,
                            Thickness, LineStyleType, Dots, 'Line.' + pLine.Name,
                            FALSE, 0, NodeMarkerCode, NodeMarkerWidth);
                    end;
                    if Labels then
                        DoBusLabels(Bus1Idx, Bus2Idx);
                end;
            end;
            pLine := Lines.Next;
        end;

   { ******************  Code for GICLines ************************** }

    pGICLineClass := GetDSSClassPtr('GICLine') as TGICLine;
    pGICLine := pGICLineClass.ElementList.First;

    while pGICLine <> NIL do
        with ActiveCircuit[ActiveActor] do
        begin
            if pGICLine.Enabled then
            begin
            // Idx1 := Buslist.Find(StripExtension(pLine.GetBus (1)));
            // Idx2 := Buslist.Find(StripExtension(pLine.GetBus (2)));
                ActiveCktElement := pGICLine;
                Bus1Idx := pGICLine.Terminals^[1].BusRef;
                Bus2Idx := pGICLine.Terminals^[2].BusRef;
                if Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx]
                    .CoordDefined then
                begin
                    if pGICLine.NPhases = 1 then
                        LineStyleType := Style(SinglePhLineStyle)
                    else
                        LineStyleType := Style(ThreePhLineStyle);
                    GICThickness := Min(7, Round(5.0 * (MaxGICCurrent / MaxScale)));
                    AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                        Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, Color1, GICThickness,
                        LineStyleType, Dots, 'GICLine.' + pGICLine.Name, FALSE, 0,
                        NodeMarkerCode, NodeMarkerWidth);
                    if Labels then
                        DoBusLabels(Bus1Idx, Bus2Idx);
                end;
            end;
            pGICLine := pGICLineClass.ElementList.Next;
        end;

   { ******************  Code for Transformers ************************** }

    pTransf := ActiveCircuit[ActiveActor].Transformers.First;
    while pTransf <> NIL do
        with ActiveCircuit[ActiveActor] do
        begin
            if pTransf.Enabled then
            begin
                ActiveCktElement := pTransf;
                Bus1Idx := pTransf.Terminals^[1].BusRef;
                Bus2Idx := pTransf.Terminals^[2].BusRef;
                if Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx]
                    .CoordDefined then
                    AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                        Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, clDkGray, 3, Style(1),
                        Dots, 'transformer.' + pTransf.Name, FALSE, 0,
                        NodeMarkerCode, NodeMarkerWidth);
            end;
            pTransf := Transformers.Next;
        end;

      { ******************  Code for special Bus Markers ************************** }

   //   AddBusMarkers;


end;

function GenPlotItemCompare(Item1, Item2: Pointer): Integer;
var
    Test: Double;
begin
    Test := TGenPlotItem(Item1).Value - TGenPlotItem(Item2).Value;
    if Test < 0.0 then
        Result := -1
    else
    if Test > 0.0 then
        Result := 1
    else
        Result := 0;
end;

procedure TDSSPlot.DoGeneralPlot;

var
    MaxValue, MinValue, Value, Diff: Double;
    F: TextFile;
    Line, FieldName: String;
    Idx, i: Integer;
    GenPlotItems: TGenPlotItemList;
    GenPlotItem: TGenPlotItem;

begin
    GenPlotItems := NIL;
    try
        try
            AssignFile(F, ObjectName);
            Reset(F);
            Readln(F, Line); // Get FieldName
            with AuxParser[ActiveActor] do
            begin
                AutoIncrement := FALSE;
                Delimiters := ',=' + #9; { Redefine delimiters }
                CmdString := Line;
                NextParam; { Bus Name }
                for i := 1 to ValueIndex do
                    NextParam; { Skip to parameter wanted }
                FieldName := StrValue; { Get field name }
            end;

         { Find min and max }
            MaxValue := -1.0E50;
            MinValue := 1.0E50;
            GenPlotItems := TGenPlotItemList.Create;

            while not EOF(F) do
            begin
                Readln(F, Line);
                if Length(Line) > 0 then
                    with AuxParser[ActiveActor] do
                    begin
                        CmdString := Line; // Load up AuxParser
                        NextParam; { Bus Name }
                        GenPlotItem := TGenPlotItem.Create;
                        GenPlotItem.Name := StrValue; // Bus Name
                        for i := 1 to ValueIndex do
                            NextParam; // Skip to desired field
                        if Length(StrValue) > 0 then
                        begin { Ignore empty fields }
                            Value := DblValue;
                            MaxValue := Max(Value, MaxValue);
                            MinValue := Min(Value, MinValue);
                            GenPlotItem.Value := Value;
                        end;
                        GenPlotItems.Add(GenPlotItem);
                    end;
            end; { WHILE }

         { Do some sanity checking on the numbers.  Don't want to include negative numbers in autoadd plot }
            if PlotType = ptAutoAddLogPlot then
            begin
                if MinValue < 0.0 then
                    MinValue := 0.0;
                if MaxValue < 0.0 then
                    MaxValue := 0.0;
            end;

            if MaxScaleIsSpecified then
                MaxValue := MaxScale; // Override with user specified value
            if MinScaleIsSpecified then
                MinValue := MinScale; // Override with user specified value

            Diff := MaxValue - MinValue;
            if Diff = 0.0 then
                Diff := MaxValue;
            if Diff = 0.0 then
                Diff := 1.0; // Everything is zero

         // Sort min to max and plot
            GenPlotItems.Sort(GenPlotItemCompare);
         // sorts using user-written routine

            Set_ChartCaption(Format('%s, Max=%-.3g ', [FieldName, MaxValue]));
            for i := 0 to GenPlotItems.Count - 1 do
            begin
                GenPlotItem := GenPlotItems.items[i];
                Idx := ActiveCircuit[ActiveActor].BusList.Find(GenPlotItem.Name);

                if Idx > 0 then
                    with ActiveCircuit[ActiveActor].Buses^[Idx] do
                    begin
                        if CoordDefined then
                        begin
                            case PlotType of
                                ptGeneralDataPlot:
                                    AddNewMarker(X, Y,
                                        InterpolateGradientColor(Color1, Color2,
                                        IntPower
                                        ((GenPlotItem.Value - MinValue) / Diff,
                                        1)), MarkerIdx,
                                        ActiveCircuit[ActiveActor].NodeMarkerWidth);
                                ptAutoAddLogPlot:
                                    AddNewMarker(X, Y,
                                        GetAutoColor((GenPlotItem.Value - MinValue) / Diff), MarkerIdx,
                                        ActiveCircuit[ActiveActor].NodeMarkerWidth);
                            else
                            end;
                            if Labels then
                                DoBusLabel(Idx, ActiveCircuit[ActiveActor].BusList.Get(Idx));
                        end;
                    end;
            end; { WHILE }

        except
            On E: Exception do
                DoSimpleMsg('Error opening "' + ObjectName + '": ' + E.Message,
                    190);
        end;
    finally
        CloseFile(F);
        GenPlotItems.Free;
    end;
end;

procedure TDSSPlot.DoTheDaisies;

var
    pGen: TGeneratorObj;
    BusCount: pIntegerArray;
    i, j, Idx: Integer;
    Xc, Yc, Radius, Angle, StartAngle: Double;
    ActiveGraphProps: TDSSGraphProperties;

begin

    BusCount := Allocmem(Sizeof(BusCount^[1]) * ActiveCircuit[ActiveActor].NumBuses);

    if DaisyBusList.Count = 0 then
    begin
      { If Daisy Bus List not filled, then fill it with Generator Buses by default }
        pGen := ActiveCircuit[ActiveActor].Generators.First;
        while pGen <> NIL do
        begin
            if pGen.Enabled then
            begin
                DaisyBusList.Add(StripExtension(pGen.GetBus(1)));
            end;
            pGen := ActiveCircuit[ActiveActor].Generators.Next;
        end;
    end;

   { Count the number of Objects at each bus }

    for i := 0 to DaisyBusList.Count - 1 do
    begin
        Idx := ActiveCircuit[ActiveActor].BusList.Find(DaisyBusList.Strings[i]);
        if Idx > 0 then
            Inc(BusCount^[Idx]);
    end;

    Randomize;

   { Draw the generators in }
    Get_Properties(ActiveGraphProps); // Get active graph properties
    Radius := 0.005 * DaisySize *
        (ActiveGraphProps.Xmax - ActiveGraphProps.Xmin);
    for i := 1 to ActiveCircuit[ActiveActor].NumBuses do
    begin
        if (BusCount^[i] > 0) and ActiveCircuit[ActiveActor].Buses^[i].CoordDefined then
        begin
            StartAngle := TwoPi { * Random };
            Angle := (TwoPi / BusCount^[i]); // Radians
            for j := 1 to BusCount^[i] do
            begin
                Xc := ActiveCircuit[ActiveActor].Buses^[i].X + 2.0 * Radius * Cos
                    (Angle * (j - 1) + StartAngle);
                Yc := ActiveCircuit[ActiveActor].Buses^[i].Y + 2.0 * Radius * Sin
                    (Angle * (j - 1) + StartAngle);
                AddNewLine(ActiveCircuit[ActiveActor].Buses^[i].X, ActiveCircuit[ActiveActor].Buses^[i].Y,
                    Xc, Yc, clRed, 1, psSolid, FALSE, 'Gen', FALSE, 0, 0, 0);
                AddNewCircle(Xc, Yc, Radius, clRed, clYellow);
            end;
        end;
    end;

   { Put Labels on }
    if Labels then
        for i := 1 to ActiveCircuit[ActiveActor].NumBuses do
            if (BusCount^[i] > 0) and ActiveCircuit[ActiveActor].Buses^[i].CoordDefined then
                DoBusLabel(i, ActiveCircuit[ActiveActor].BusList.Get(i));

    Reallocmem(BusCount, 0); { Clean up allocated memory }

end;

procedure TDSSPlot.DoMeterZonePlot;

{ Draws feeder lines using the meter zones only
  Each feeder is drawn in a different color
  }

var
    pMeter: TEnergyMeterObj;
    hMeter, Idx1, Idx2: Integer;
    FdrColor: Integer;
    LineStyleType: TPenStyle;
    LoopLine: TLineObj;
    S: String;

   { --------------------------------------------------------------------------------- }
    procedure DrawMeterZoneLine(Clr: TColor; const Nam: String);
    begin { Local proc }
        if ActiveCircuit[ActiveActor].Buses^[Idx1].CoordDefined and ActiveCircuit[ActiveActor].Buses^[Idx2]
            .CoordDefined then
        begin
            AddNewLine(ActiveCircuit[ActiveActor].Buses^[Idx1].X, ActiveCircuit[ActiveActor].Buses^[Idx1].Y,
                ActiveCircuit[ActiveActor].Buses^[Idx2].X, ActiveCircuit[ActiveActor].Buses^[Idx2].Y, Clr,
                Thickness, LineStyleType, Dots, 'Line.' + Nam, FALSE, 0, 0, 0);
            if Labels then
                DoBusLabels(Idx1, Idx2);
        end;
    end;

begin

    hMeter := EnergyMeterClass[ActiveActor].First;
    ActiveColorIdx := 0; { Nextcolor does an Inc() }
    while hMeter > 0 do
    begin
        if (Length(ObjectName) > 0) then // look for a specific object {Else Draw Them All}
            if CompareText(ObjectName, ActiveDSSObject[ActiveActor].Name) <> 0 then
            begin
                hMeter := EnergyMeterClass[ActiveActor].Next;
                continue;
            end;

        pMeter := TEnergyMeterObj(ActiveDSSObject[ActiveActor]);

        FeederName := pMeter.Name;

        pLine := pMeter.BranchList.First;

      { Mark Meter Location }
        Idx1 := pLine.Terminals^[pMeter.MeteredTerminal].BusRef;
        Set_LineWidth(4);
        if ActiveCircuit[ActiveActor].Buses^[Idx1].CoordDefined then
            AddNewMarker(ActiveCircuit[ActiveActor].Buses^[Idx1].X,
                ActiveCircuit[ActiveActor].Buses^[Idx1].Y, clRed, 24, 3);

        if ShowLoops then
            FdrColor := Color1
        else
            FdrColor := NextColor;

        while pLine <> NIL do
        begin
            if pLine.Enabled then
            begin
                pLine.Drawn := TRUE;
                ActiveCircuit[ActiveActor].ActiveCktElement := pLine;
                Idx1 := pLine.Terminals^[1].BusRef;
                Idx2 := pLine.Terminals^[2].BusRef;
                if pLine.NPhases = 1 then
                    LineStyleType := Style(SinglePhLineStyle)
                else
                    LineStyleType := Style(ThreePhLineStyle);
                if ShowLoops and pMeter.BranchList.PresentBranch.IsLoopedHere then
                begin
                    DrawMeterZoneLine(Color3, pLine.Name);

               { Draw 2nd Line in loop in alternate color, also, if coordinates defined }
                    LoopLine := TLineObj
                        (pMeter.BranchList.PresentBranch.LoopLineObj);
                    Idx1 := LoopLine.Terminals^[1].BusRef;
                    Idx2 := LoopLine.Terminals^[2].BusRef;
                    DrawMeterZoneLine(Color3, LoopLine.Name);

                end
                else
                    DrawMeterZoneLine(FdrColor, pLine.Name); // normal show zone
            end;
            pLine := pMeter.BranchList.GoForward;
        end;
        hMeter := EnergyMeterClass[ActiveActor].Next;
    end;

    if (Length(ObjectName) > 0) then
        S := 'Meter Zone: ' + ObjectName
    else
        S := 'All Meter Zones';

    Set_ChartCaption(S);

end;

procedure TDSSPlot.Execute;

var
    Aspect, XRange: Double;
    S: String;
    DSSGraphProps: TDSSGraphProperties;
   //Width, LRim, RRim, Height, Trim, Brim: Integer;
    RangeLoX, RangeHiX, RangeLoY, RangeHiY: Double;
    Fname: String;
    i: Integer;

begin

{Init line.Drawn variable to Not Drawn}

    pLine := ActiveCircuit[ActiveActor].Lines.First;
    while Assigned(pLine) do
    begin
        pLine.Drawn := FALSE;
        pLine := ActiveCircuit[ActiveActor].Lines.Next;
    end;

    with DSSPlotObj do
        if (PlotType = ptCircuitplot) and (Quantity = pqNone) and
            (FileExists(ObjectName)) then
            PlotType := ptGeneralCircuitPlot;

   { *** Make a New DSSGraph Plot *** }
  // If MakeNewGraph(DSSDataDirectory + CircuitName_ + 'Plot.DSV') = 0 Then
 //  Begin
 //     DoSimpleMsg('Make New Plot failed in DSSPlot Execute.', 8734);
 //     Exit;
 //  End;
    try
        case PlotType of
            ptmonitorplot:
            begin
                Fname := GetOutputDirectory + CircuitName_[ActiveActor] + 'MONITOR-' + UpperCase(ObjectName);
                for i := 0 to High(Channels) do
                    Fname := Fname + Format('-ch%d', [Channels[i]]);
                if MakeNewGraph(Fname + '.DSV') > 0 then
                begin
                    DoMonitorPlot;
                    Exit;
                end
                else
                begin
                    DoSimpleMsg('Make New Plot failed for Monitor Plot.', 87341);
                    Exit;
                end;
            end;   {Monitor Plot}
            ptLoadShape:
                if MakeNewGraph(GetOutputDirectory + CircuitName_[ActiveActor] + Format('Loadshape_%s.DSV', [ObjectName])) > 0 then
                begin
                    DoLoadShapePlot(ObjectName);
                    Exit; // All we need to do here
                end
                else
                begin
                    DoSimpleMsg('Make New Plot failed for Loadshape Plot.', 87342);
                    Exit;
                end;
            ptTShape:
                if MakeNewGraph(GetOutputDirectory + CircuitName_[ActiveActor] + Format('TempShape_%s.DSV', [ObjectName])) > 0 then
                begin
                    DoTempShapePlot(ObjectName);
                    Exit; // All we need to do here
                end
                else
                begin
                    DoSimpleMsg('Make New Plot failed for TempShape Plot.', 87343);
                    Exit;
                end;
            ptPriceShape:
                if MakeNewGraph(GetOutputDirectory + CircuitName_[ActiveActor] + Format('Priceshape_%s.DSV', [ObjectName])) > 0 then
                begin
                    DoPriceShapePlot(ObjectName);
                    Exit; // All we need to do here
                end
                else
                begin
                    DoSimpleMsg('Make New Plot failed for PriceShape Plot.', 87344);
                    Exit;
                end;
            ptProfile:
                if MakeNewGraph(GetOutputDirectory + CircuitName_[ActiveActor] + Format('Profile%d.DSV', [PhasesToPlot])) > 0 then
                begin
                    DoProfilePlot;
                    Exit;
                end
                else
                begin
                    DoSimpleMsg('Make New Plot failed for Profile Plot.', 87345);
                    Exit;
                end;
        else { All other plots }

            case PlotType of
                ptAutoAddLogPlot:
                    if MakeNewGraph(GetOutputDirectory + CircuitName_[ActiveActor] + 'AutoADD.DSV') = 0 then
                    begin
                        DoSimpleMsg('Make New Plot failed for AutoADD Plot.', 8734);
                        Exit;
                    end;
                ptCircuitplot:
                begin
                    Fname := GetOutputDirectory + CircuitName_[ActiveActor];
                    case Quantity of
                        pqVoltage:
                            Fname := Fname + 'Voltage.DSV';
                        pqCurrent:
                            Fname := Fname + 'Current.DSV';
                        pqPower:
                            Fname := Fname + 'Power.DSV';
                        pqLosses:
                            Fname := Fname + 'Losses.DSV';
                        pqCapacity:
                            Fname := Fname + 'Capacity.DSV';
                        pqNone:
                            Fname := Fname + 'Circuit.DSV';
                    end;

                    if MakeNewGraph(Fname) = 0 then
                    begin
                        DoSimpleMsg('Make New Plot failed for Circuit Plot.', 87346);
                        Exit;
                    end;
                end;
                ptGeneralDataPlot:
                    if MakeNewGraph(GetOutputDirectory + CircuitName_[ActiveActor] + 'General.DSV') = 0 then
                    begin
                        DoSimpleMsg('Make New Plot failed for General Data Plot.', 87347);
                        Exit;
                    end;
                ptGeneralCircuitPlot:
                    if MakeNewGraph(GetOutputDirectory + CircuitName_[ActiveActor] + 'GeneralCircuit.DSV') = 0 then
                    begin
                        DoSimpleMsg('Make New Plot failed for GeneralCircuit Plot.', 87348);
                        Exit;
                    end;
                ptMeterZones:
                    if MakeNewGraph(GetOutputDirectory + CircuitName_[ActiveActor] + 'MeterZone.DSV') = 0 then
                    begin
                        DoSimpleMsg('Make New Plot failed for MeterZone Plot.', 87349);
                        Exit;
                    end;
                ptdaisyplot:
                    if MakeNewGraph(GetOutputDirectory + CircuitName_[ActiveActor] + 'Daisy.DSV') = 0 then
                    begin
                        DoSimpleMsg('Make New Plot failed for Daisy Plot.', 87340);
                        Exit;
                    end;
            end;
            AllocateBusLabels;
            Get_Properties(DSSGraphProps);
            with DSSGraphProps do
            begin
                GridStyle := gsNone;
                ChartColor := clWhite;
                WindColor := clWhite;
                Isometric := TRUE;
                EnableClickonDiagram;
            end;
            Set_Properties(DSSGraphProps);
            S := 'X';
            Set_XaxisLabel(s);
            S := 'Y';
            Set_YaxisLabel(s);

            Set_TextAlignment(1); { Left Justify; 2 = center; 3=right }
            Set_KeyClass(DSSG_LINECLASS); { Line for searches }
            case PlotType of
                ptAutoAddLogPlot:
                begin
                    MarkerIdx := 26;
                    Set_KeyClass(DSSG_MARKERCLASS); { Marker }
                    DoAutoAddPlot;
                    MarkSpecialClasses;
                end;
                ptCircuitplot:
                begin
                    SetMaxScale;
                    S := Format('%s:%s, max=%-6.3g', [ActiveCircuit[ActiveActor].CaseName, QuantityString, MaxScale]);
                    Set_ChartCaption(S);
                    DoCircuitPlot;
                    MarkSpecialClasses;
                end;
                ptGeneralDataPlot:
                begin
                    Dots := FALSE;
                    DoCircuitPlot;
                    Set_KeyClass(DSSG_MARKERCLASS); { Marker }
                    MarkerIdx := ActiveCircuit[ActiveActor].NodeMarkerCode; // 24;
                    DoGeneralPlot;
                    MarkSpecialClasses;
                end;
                ptGeneralCircuitPlot:
                begin
                    LoadGeneralLineData;
                    SetMaxScale;
                    S := Format('%s:%s, max=%-.3g', [ActiveCircuit[ActiveActor].CaseName, QuantityString, MaxScale]);
                    Set_ChartCaption(S);
                    DoGeneralCircuitPlot;
                    MarkSpecialClasses;
                end;
                ptMeterZones:
                begin
                    DoMeterZonePlot;
                    MarkSpecialClasses;
                end;
                ptdaisyplot:
                begin
                    S := 'Device Locations / ' + QuantityString;
                    Set_ChartCaption(S);
                    if Labels then
                    begin
                        Labels := FALSE; { Temporarily turn off }
                        DoCircuitPlot;
                        Labels := TRUE; { Turn back on to label generators }
                    end
                    else
                        DoCircuitPlot;
                    MarkSpecialClasses;
                    DoTheDaisies;
                end;

            else { Case PlotType }
         { Nada }
            end;

            LabelBuses; { Add labels on top of lines }

            FreeBusLabels;

      { Make sure both X and Y have the same scale }
      // --deprecated--   Get_PlotWindowParms(Width, LRim, RRim, Height, Trim, Brim);
      // --deprecated--   Aspect :=  (Width - LRim - RRim)/(Height - Trim - Brim);
            Aspect := 1.5; // Default aspect ratio
            Get_Properties(DSSGraphProps);
            with DSSGraphProps do
            begin
                XRange := Max((Xmax - Xmin), (Ymax - Ymin) * Aspect);
         { Add 2%Margin }
                XRange := 1.02 * XRange;
         // --deprecated--        Get_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);
                RangeLoX := (Xmin + Xmax - XRange) / 2.0; // Xmin - Mar;    {Isometric=true forces Y to have same range as X}
                RangeHiX := (Xmin + Xmax + XRange) / 2.0; // Xmin + HiX + Mar;
                RangeLoY := Ymin - 0.02 * XRange / Aspect;
                RangeHiY := RangeLoY + (XRange / Aspect);
                Set_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);

         { Keep this range for quick resetting }
                Xmin := RangeLoX;
                Xmax := RangeHiX;
                Ymin := RangeLoY;
                Ymax := RangeHiY;
                Set_Properties(DSSGraphProps);
            end;
            set_KeepAspectRatio(TRUE);

        end; { CASE }

    finally
        ShowGraph;
    end;

end;

procedure TDSSPlot.DSSVizPlot;
begin
    if not Assigned(DSSConnectObj) then // First connection
    begin
        DSSConnectObj := TDSSConnect.Create;  // Creates the connection
    end;

    DSSConnectObj.Connect;  // Connects to the server

    case PlotType of  // Classifies the plot message
        ptmonitorplot:
            DSSConnectObj.MonitorPlotMsg(ObjectName);
        ptLoadshape:
            DSSConnectObj.LoadshapePlotMsg(ObjectName);
        ptProfile:
        begin
            DSSConnectObj.ProfilePlotMsg(ObjectName, PlotID);
            PlotID := '';
        end;
        ptScatterPlot:
        begin
            DSSConnectObj.ScatterPlotMsg(PlotID);
            PlotID := '';
        end;
        ptEvolutionPlot:
            DSSConnectObj.EvolutionPlotMsg;
        ptEnergyPlot:
            DSSConnectObj.EnergyMeterPlotMsg(ObjectName);
        ptMatrixplot:
        begin
            if MatrixType = pLaplacian then
                DSSConnectObj.MatrixPlotMsg(1)
            else
                DSSConnectObj.MatrixPlotMsg(0)
        end;
        ptPhaseVoltage:
            DSSConnectObj.PhaseVoltageMsg(ObjectName);
    end;
end;

function TDSSPlot.InterpolateGradientColor(Color1, Color2: TColor;
    Ratio: Double): TColor;
const
    Redmask = $000000FF;
    GreenMask = $0000FF00;
    BlueMask = $00FF0000;
var
    R1, G1, B1, R2, G2, B2: Integer;
    RatioToUse: Double;

    function InterpByte(B1, B2: Integer): Integer;
    begin
        Result := Round(B1 + RatioToUse * (B2 - B1));
    end;

begin

    RatioToUse := Max(0.0, Min(1.0, Ratio)); // Limit to 0.0 .. 1.0

    R1 := Color1 and Redmask;
    G1 := (Color1 and GreenMask) shr 8;
    B1 := (Color1 and BlueMask) shr 16;

    R2 := Color2 and Redmask;
    G2 := (Color2 and GreenMask) shr 8;
    B2 := (Color2 and BlueMask) shr 16;

    Result := InterpByte(R1, R2) + InterpByte(G1, G2) shl 8 + InterpByte(B1, B2) shl 16;
    if Result <= 0 then
        Result := Color1;

end;

function TDSSPlot.MaxCurrent: Double;
var
    i: Integer;
begin
    pLine.ComputeIterminal(ActiveActor); // load element Iterminal buffer
    Result := 0.0;
    for i := 1 to pLine.NPhases do
        if Cabs(pLine.Iterminal^[i]) > Result then
            Result := Cabs(pLine.Iterminal^[i]);
end;

function TDSSPlot.NextColor: TColor;
begin
    Inc(ActiveColorIdx);
    if ActiveColorIdx > 17 then
        ActiveColorIdx := 1;
    Result := ColorArray[ActiveColorIdx];
end;

procedure TDSSPlot.SetColorArray;
begin
    ColorArray[1] := TColor($000000);
    ColorArray[2] := TColor($0000FF);
    ColorArray[3] := TColor($FF0000);
    ColorArray[4] := TColor($FF00FF);
    ColorArray[5] := TColor($008000);
    ColorArray[6] := TColor($00FF80);
    ColorArray[7] := TColor($4080FF);
    ColorArray[8] := TColor($21DEDA);
    ColorArray[9] := TColor($FF6AB5);
    ColorArray[10] := TColor($004080);
    ColorArray[11] := TColor($008080);
    ColorArray[12] := TColor($A00000);
    ColorArray[13] := TColor($8080FF);
    ColorArray[14] := TColor($800000);
    ColorArray[15] := TColor($7F7F7F);
    ColorArray[16] := TColor($7B0F8E);
    ColorArray[17] := TColor($8E9607);
end;

procedure TDSSPlot.SetDefaults;
begin

    MaxScale := 0.0; // Find MaxScale
    MaxScaleIsSpecified := FALSE; // indicates take the default
    MinScale := 0.0; // Find MinScale
    MinScaleIsSpecified := FALSE; // indicates take the default

    Dots := FALSE;
    Labels := FALSE;
    ShowLoops := FALSE;
    ShowSubs := FALSE;
    Quantity := pqPower;
    PlotType := ptCircuitplot;
    MarkerIdx := 24;
    ObjectName := '';

    FMaxLineThickness := 10;

    Channels := NIL;
    SetLength(Channels, 3);
    Channels[0] := 1;
    Channels[1] := 3;
    Channels[2] := 5;

    Bases := NIL;
    SetLength(Bases, 3);
    Bases[0] := 1.0;
    Bases[1] := 1.0;
    Bases[2] := 1.0;

    Color1 := clBlue;
    Color2 := clGreen;
    Color3 := clRed;

    TriColorMax := 0.85;
    TriColorMid := 0.50;

    ActiveColorIdx := 0;
    SetColorArray;

    ThreePhLineStyle := 1;
    SinglePhLineStyle := 1;

end;

function TDSSPlot.Style(Code: Integer): TPenStyle;
begin
    case Code of
        1:
            Result := psSolid;
        2:
            Result := psDash;
        3:
            Result := psDot;
        4:
            Result := psDashDot;
        5:
            Result := psDashDotDot;
        6:
            Result := psClear;
        7:
            Result := psInsideFrame;
    else
        Result := psSolid
    end;
end;

function TDSSPlot.Thickness: Integer;
begin
    case PlotType of
        ptmonitorplot:
            Result := 1;
    else
    begin
        pLine.ActiveTerminalIdx := 1; // just for good measure
        case Quantity of
            pqNone:
            begin
                if PlotType = ptGeneralCircuitPlot then
                    Result := Round
                        (8.0 * (abs(pLine.GeneralPlotQuantity) / MaxScale))
                else
                    Result := 1;
            end;
            pqVoltage:
                Result := 1;
            pqCurrent:
            begin
                if pLine.Normamps > 0.0 then
                    Result := Round(5.0 * MaxCurrent / pLine.Normamps)
                else
                    Result := 1;
            end;
            pqPower:
            begin
                Result := Round
                    (5.0 * (abs(pLine.Power[1, ActiveActor].re) * 0.001 / MaxScale)); // kW
            end;
            pqLosses:
            begin // Losses per unit length
                Result := Round
                    (5.0 * (abs(pLine.Losses[ActiveActor].re / pLine.Len) * 0.001 / MaxScale));
            end;
            pqCapacity:
            begin
                if pLine.Normamps > 0.0 then
                    Result := Round(5.0 * (1.0 - MaxCurrent / pLine.Normamps))
                else
                    Result := FMaxLineThickness;
            end;
        else
            Result := 1;
        end;
    end;

    end;

    if Result <= 0 then
        Result := 1;
    if Result > FMaxLineThickness then
        Result := FMaxLineThickness;
end;

procedure TDSSPlot.DoTempShapePlot(const TempShapeName: String);
var
    Temp_Shape: TTShapeObj;
    Xarray: pdoubleArray;
    X, Xinc: Double;
    i: Integer;
    Xsize: Integer;
    XLabel: String;
    UseXarray: Boolean;
    S: String;

begin
    Temp_Shape := TShapeClass[ActiveActor].Find(TempShapeName);
    if Temp_Shape = NIL then
    begin
        DoSimpleMsg('Tshape object not found: "' + TempShapeName + '"', 87341);
        Exit;
    end;

    UseXarray := FALSE;
    Xarray := NIL;
    Xsize := 0; // Init

    if Temp_Shape.Interval <> 0.0 then
        with Temp_Shape do
        begin // have to gen up Xarray
            UseXarray := TRUE;
            Xsize := Sizeof(Xarray^[1]) * NumPoints;
            GetMem(Xarray, Xsize); // SetLength(Xarray, Numpoints);
            X := 0.0;
            if Interval * NumPoints < 1.0 then
            begin
                Xinc := Interval * 3600.0; // Plot secs
                XLabel := 'Seconds';
            end
            else
            begin
                Xinc := Interval;
                XLabel := 'Hours';
            end;
            for i := 1 to NumPoints do
            begin
                Xarray[i] := X;
                X := X + Xinc;
            end;
        end;

   // ** already exists MakeNewGraph;
    S := 'TShape.' + TempShapeName;
    Set_Caption(S);
    S := 'TShape = ' + TempShapeName;
    Set_ChartCaption(S);
    Set_XaxisLabel(Xlabel);
    Set_YaxisLabel('Temperature');

    if UseXarray then
        AddNewCurve(Xarray, Temp_Shape.TValues, Temp_Shape.NumPoints, Color1, 1,
            psSolid, FALSE, 1, TempShapeName)
    else
        AddNewCurve(Temp_Shape.Hours, Temp_Shape.TValues, Temp_Shape.NumPoints,
            Color1, 1, psSolid, FALSE, 1, TempShapeName);

    set_KeepAspectRatio(FALSE);

    if UseXarray then
        FreeMem(Xarray, Xsize);
    Set_Autorange(2.0); // 2% rim
//***   ShowGraph; { Form Freed on close }
end;

procedure TDSSPlot.DoLoadShapePlot(const LoadShapeName: String);

var
    Load_Shape: TLoadShapeObj;
    Xarray: pdoubleArray;
    X, Xinc: Double;
    i: Integer;
    Xsize: Integer;
    XLabel: String;
    UseXarray: Boolean;
    S: String;

begin
    Load_Shape := LoadShapeClass[ActiveActor].Find(LoadShapeName);
    if Load_Shape = NIL then
    begin
        DoSimpleMsg('Loadshape object not found: "' + LoadShapeName + '"', 87341);
        Exit;
    end;

    UseXarray := FALSE;
    Xarray := NIL;
    Xsize := 0; // Init

    if Load_Shape.Interval <> 0.0 then
        with Load_Shape do
        begin // have to gen up Xarray
            UseXarray := TRUE;
            Xsize := Sizeof(Xarray^[1]) * NumPoints;
            GetMem(Xarray, Xsize); // SetLength(Xarray, Numpoints);
            X := 0.0;
            if Interval * NumPoints < 1.0 then
            begin
                Xinc := Interval * 3600.0; // Plot secs
                XLabel := 'Seconds';
            end
            else
            begin
                Xinc := Interval;
                XLabel := 'Hours';
            end;
            for i := 1 to NumPoints do
            begin
                Xarray[i] := X;
                X := X + Xinc;
            end;
        end;

   // ** already exists MakeNewGraph;
    S := 'Loadshape.' + LoadShapeName;
    Set_Caption(S);
    S := 'Loadshape = ' + LoadShapeName;
    Set_ChartCaption(S);
    Set_XaxisLabel(XLabel);
    if Load_Shape.UseActual then
        Set_YaxisLabel('kW, kvar')
    else
        Set_YaxisLabel('p.u.');

    if UseXarray then
        AddNewCurve(Xarray, Load_Shape.PMultipliers, Load_Shape.NumPoints,
            Color1, 1, psSolid, FALSE, 1, LoadShapeName)
    else
        AddNewCurve(Load_Shape.Hours, Load_Shape.PMultipliers,
            Load_Shape.NumPoints, Color1, 1, psSolid, FALSE, 1,
            LoadShapeName);

    if Assigned(Load_Shape.QMultipliers) then
    begin
        if UseXarray then
            AddNewCurve(Xarray, Load_Shape.QMultipliers, Load_Shape.NumPoints,
                Color2, 1, psSolid, FALSE, 1, LoadShapeName)
        else
            AddNewCurve(Load_Shape.Hours, Load_Shape.QMultipliers,
                Load_Shape.NumPoints, Color2, 1, psSolid, FALSE, 1,
                LoadShapeName);
    end;

    set_KeepAspectRatio(FALSE);

    if UseXarray then
        FreeMem(Xarray, Xsize);
    Set_Autorange(2.0); // 2% rim
//***   ShowGraph; { Form Freed on close }
end;

{ --------------------------------------------------------- }
procedure LoadRegisters(RegisterArray: pdoubleArray);
var
    i: Integer;
begin
    AuxParser[ActiveActor].ParseAsVector(NumEMRegisters + 1, RegisterArray);
    for i := 1 to NumEMRegisters do
        RegisterArray^[i] := RegisterArray^[i] * 0.001;
end;

{ --------------------------------------------------------- }
procedure PeakDayLoadRegisters(var F: TextFile; RegisterArray: pdoubleArray);
var
    iday, i: Integer;
    TempRegisters: array [1 .. NumEMRegisters + 1] of Double;
    S: String;
begin
    for i := 0 to NumEMRegisters do
        RegisterArray^[i] := 0.0;
    for iday := 1 to 24 do
        if not EOF(F) then
        begin
            Readln(F, S);
            AuxParser[ActiveActor].CmdString := '"' + S + '"';
            AuxParser[ActiveActor].NextParam;
            LoadRegisters(@TempRegisters);
            for i := 1 to NumEMRegisters + 1 do
                RegisterArray^[i] := Max(RegisterArray^[i], TempRegisters[i]);
        end;
end;

{ --------------------------------------------------------- }

procedure TDSSPlot.DoDI_Plot(const CaseName: String; CaseYear: Integer;
    iRegisters: array of Integer; PeakDay: Boolean; const MeterName: String);

var
    F: TextFile;
    Names: TStringList;
    S: String;
    FileName: String;
    Param: String;
    Registers1, Registers2: array [0 .. NumEMRegisters] of Double;
    i: Integer;
    ActiveGraphProps: TDSSGraphProperties;

begin
   { Plot Demand interval data from saved results DI_Totals.CSV }
   { If PeakDay=True then we only plot the peak of a 24-hr day }
    Names := TStringList.Create;
   { Open File }
    FileName := CaseName + PathDelim + 'di_yr_' + Trim(IntToStr(CaseYear)) + PathDelim + MeterName + '.CSV';
    if not FileExists(FileName) then
    begin
        DoSimpleMsg('File "' + FileName + '" does not exist.', 191);
        Exit;
    end
    else
    begin
        try
            AssignFile(F, FileName);
            Reset(F);
            Readln(F, S); // Read input line

            with AuxParser[ActiveActor] do
            begin
                CmdString := S;
                NextParam;
                Param := StrValue;
                while Length(Param) > 0 do
                begin
                    Names.Add(Param);
                    NextParam;
                    Param := AuxParser[ActiveActor].StrValue;
                end;
            end; { With }
        except
            On E: Exception do
                DoSimpleMsg('Error Reading File "' + FileName + '". ' + E.message,
                    192)
        end;

    end;

    if MakeNewGraph(GetOutputDirectory + CircuitName_[ActiveActor] + 'DIPlot.DSV') = 0 then
    begin
        DoSimpleMsg('Make New Plot failed in DSSPlot - DI plot.', 8734);
        Exit;
    end;

   { POssibly change some properties of the graph }
    Get_Properties(ActiveGraphProps);
    with ActiveGraphProps do
    begin
        ChartColor := clWhite;
        WindColor := clWhite;
    end;
    Set_Properties(ActiveGraphProps);

    S := CaseName + Format(', Yr=%d, ', [CaseYear]);
    Set_Caption(S);
    Set_XaxisLabel('Hour');
    S := 'MW, MWh or MVA';
    Set_YaxisLabel(S);

    S := 'Registers: ';
    for i := 0 to High(iRegisters) do
        S := S + Names.Strings[iRegisters[i]] + ', ';
    Set_ChartCaption(S);

   { Get started - initializer Registers 1 }
    try
        try
            if not EOF(F) then
                if PeakDay then
                    PeakDayLoadRegisters(F, @Registers1)
                else
                begin
                    Readln(F, S);
                    with AuxParser[ActiveActor] do
                    begin
                        CmdString := '"' + S + '"';
                        NextParam;
                        LoadRegisters(@Registers1);
                    end;
                end;

            while not EOF(F) do
            begin
                if PeakDay then
                    PeakDayLoadRegisters(F, @Registers2)
                else
                begin
                    Readln(F, S);
                    with AuxParser[ActiveActor] do
                    begin
                        CmdString := '"' + S + '"';
                        NextParam;
                        LoadRegisters(@Registers2);
                    end;
                end;

                ActiveColorIdx := 0;
                for i := 0 to High(iRegisters) do
                begin
                    AddNewLine(Registers1[0], Registers1[iRegisters[i]],
                        Registers2[0], Registers2[iRegisters[i]], NextColor, 1,
                        psSolid, FALSE, ' ', FALSE, 0, 0, 0);
                end;
                for i := 0 to NumEMRegisters do
                    Registers1[i] := Registers2[i];
            end;

        except
            On E: Exception do
                DoSimpleMsg('Error Reading File "' + FileName + '". ' + E.message,
                    193)
        end;
        set_KeepAspectRatio(FALSE);

        Set_Autorange(2.0); // 2% rim
//****      ShowGraph; { Form Freed on close }

    finally

        CloseFile(F);
        Names.Free;

    end;
end;

procedure TDSSPlot.DoCompareCases(CaseName1, CaseName2, WhichFile: String;
    Reg: Integer);

{ Compare a register from to cases in the Totals.CSV file, compute horiz distance,
  plot vs 1st register of totals.csv file }

var
    F: TextFile;
    S, FileName: String;
    Param, CaseName: String;
    Names: TStringList;
    Registers1, Registers2: array [0 .. NumEMRegisters] of Double;
    i, iPass, iCase, CaseYear, PrevCaseYear, ActiveColorStartThisCase,
    DiffColor: TColor;

   { Arrays to hold the two curves for diff curve }
    X, Y: array [1 .. 2, 0 .. 20] of Double;
    HorizDiff: array [0 .. 20] of Double;
    X1, Y1: Double;
    MinYear, MaxYear: Integer;
    Xinc, Yinc, LegendX, LegendY: Double;
    LabelIdx: Integer;
    SearchForMeterName: Boolean;
    FirstYear: Boolean;
    ActiveGraphProps: TDSSGraphProperties;
    DatColor: TColor;

   { * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

   { Internal Procs }

    function GetDiff(Yvalue, XValue: Double): Double;
   { Interpolation routine }
    var
        k, lastk: Integer;
    begin
        lastk := 0;
        for k := 0 to MaxYear do
        begin
            if X[2, k] > 0.0 then
            begin
                lastk := k;
                if Yvalue = 0.0 then
                begin
                    Result := 0.0;
                    Exit;
                end
                else
                if Y[2, k] = Yvalue then
                begin
                    Result := X[2, k] - XValue;
                    Exit;
                end
                else
                if Y[2, k] > Yvalue then
                begin
                    if (k = 0) then
                        Result := X[2, k] - XValue
                    else
                    if ((Y[2, k] - Y[2, k - 1]) = 0.0) then
                        Result := X[2, k - 1] - XValue
                    else
                        Result := X[2, k - 1] + (Yvalue - Y[2, k - 1]) /
                            (Y[2, k] - Y[2, k - 1]) * (X[2, k] - X[2, k - 1]) - XValue;
                    Exit;
                end;
            end;
        end;
      { If we get here, didn't find anything.  Extrapolate last two points }
        if lastk = 0 then
            Result := 0.0
        else
            Result := X[2, lastk - 1] + (Yvalue - Y[2, lastk - 1]) /
                (Y[2, lastk] - Y[2, lastk - 1]) * (X[2, lastk] - X[2, lastk - 1]) - XValue;
    end;

    procedure MakeDiffCurve;
    var
        j: Integer;
    begin
        for j := 0 to MaxYear do
        begin
            if X[1, j] > 0.0 then
                HorizDiff[j] := GetDiff(Y[1, j], X[1, j]);
        end;
    end;

    function ReadS: Boolean;
    begin
        Result := TRUE;
        if SearchForMeterName then
        begin
            repeat
                Readln(F, S);
                AuxParser[ActiveActor].CmdString := S;
                AuxParser[ActiveActor].NextParam;
            until (CompareText(WhichFile, AuxParser[ActiveActor].StrValue) = 0) or EOF(F);
            if (CompareText(WhichFile, AuxParser[ActiveActor].StrValue) = 0) then
            begin
                S := IntToStr(CaseYear) + Copy(S, Pos(',', S), 9999);
            end
            else
            begin
                Result := FALSE;
            end;
        end
        else
            Readln(F, S);
    end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
begin
   { Plot Demand interval data from saved results DI_Totals.CSV }

    Names := TStringList.Create;

   { Init holding array }
    for i := 0 to 20 do
    begin
        X[1, i] := -1.0; // signify no value at this point
        X[2, i] := -1.0;
        Y[1, i] := 0.0;
        Y[2, i] := 0.0;
    end;
    MinYear := 20;
    MaxYear := 0;

    if MakeNewGraph(GetOutputDirectory + CircuitName_[ActiveActor] + 'CompPlot.DSV') = 0 then
    begin
        DoSimpleMsg('Make New Plot failed in DSSPlot - comparison Plot.', 8734);
        Exit;
    end;


    Get_Properties(ActiveGraphProps);
    with ActiveGraphProps do
    begin
        ChartColor := clWhite;
        WindColor := clWhite;
    end;
    Set_Properties(ActiveGraphProps);

    S := 'Comparision of Yearly Curves for case(s):' + CaseName1 + ', ' +
        CaseName2;
    Set_Caption(S);

    S := 'Total Area MW';
    Set_XaxisLabel(S);
    S := 'MW, MWh or MVA';
    Set_YaxisLabel(S);

   { Loop Through Cases }
    ActiveColorStartThisCase := 0;
    CaseName := CaseName1;
    for iCase := 1 to 2 do
    begin

      // Get X values from Totals.CSV on first pass
        for iPass := 1 to 2 do
        begin
         { Loop through possible caseyears (0..20) }
            FirstYear := TRUE;
            for CaseYear := 0 to 20 do
            begin
            { Open File }
                SearchForMeterName := FALSE;
                case iPass of
                    1:
                        FileName := CaseName + PathDelim + 'di_yr_' + Trim(IntToStr(CaseYear)) + PathDelim + 'Totals.CSV';
                    2:
                        if (CompareText(WhichFile, 'Totals') = 0) or
                            (CompareText(WhichFile, 'Systemmeter') = 0) then
                        begin
                            FileName := CaseName + PathDelim + 'di_yr_' + Trim
                                (IntToStr(CaseYear)) + PathDelim + WhichFile + '.CSV';
                        end
                        else
                        begin
                            FileName := CaseName + PathDelim + 'di_yr_' + Trim
                                (IntToStr(CaseYear)) + PathDelim + 'EnergyMeterTotals.CSV';
                            SearchForMeterName := TRUE;
                        end;
                end;
                if not FileExists(FileName) then
                begin
                    continue; // Skip if it doesnt exist
                end
                else
                begin
                    try
                        MaxYear := Max(MaxYear, CaseYear);
                        MinYear := Min(MinYear, CaseYear);
                        AssignFile(F, FileName);
                        Reset(F);
                        Readln(F, S); // Read header line
                        if (iCase = 1) and FirstYear then
                        begin
                            AuxParser[ActiveActor].CmdString := S;
                            AuxParser[ActiveActor].NextParam;
                            Param := AuxParser[ActiveActor].StrValue;
                            while Length(Param) > 0 do
                            begin
                                Names.Add(Param);
                                AuxParser[ActiveActor].NextParam;
                                Param := AuxParser[ActiveActor].StrValue;
                            end;
                            S := 'Meter: ' + WhichFile + ' Register: ' +
                                Names.Strings[Reg];
                            Set_ChartCaption(S);
                        end;

                    except
                        On E: Exception do
                            DoSimpleMsg
                            ('Error Reading File "' + FileName + '". ' + E.message,
                                194)
                    end;

                end;

            { Get started - initialize Registers 1 }
                PrevCaseYear := CaseYear;
                try
                    try
                        if FirstYear then
                        begin
                            if not EOF(F) then
                            begin
                                if not ReadS then
                                begin
                                    DoSimpleMsg('Meter Not Found: "' + WhichFile + '"',
                                        1941);
                                    Exit; // Abort
                                end;
                                AuxParser[ActiveActor].CmdString := '"' + S + '"';
                                AuxParser[ActiveActor].NextParam;
                                LoadRegisters(@Registers1);
                                case iPass of
                                    1:
                                        X[iCase, CaseYear] := Registers1[7];
                                    2:
                                        Y[iCase, CaseYear] := Registers1[Reg];
                                end;
                                FirstYear := FALSE;
                            end;
                        end
                        else
                        if not EOF(F) then
                        begin // Gotta have at least 2 years to make a plot
                            ReadS;
                            AuxParser[ActiveActor].CmdString := '"' + S + '"';
                            AuxParser[ActiveActor].NextParam;
                            LoadRegisters(@Registers2);
                            case iPass of
                                1:
                                    X[iCase, CaseYear] := Registers2[7];
                                2:
                                    Y[iCase, CaseYear] := Registers2[Reg];
                            end;
                            case iPass of
                                2:
                                begin
                                    ActiveColorIdx := ActiveColorStartThisCase;
                                    AddNewLine(X[iCase, PrevCaseYear],
                                        Registers1[Reg], X[iCase, CaseYear],
                                        Registers2[Reg], NextColor, 2, psSolid, FALSE,
                                        ' ', FALSE, 0, 0, 0);
                                    MarkAt(X[iCase, CaseYear], Registers2[Reg],
                                        GetMarker(ActiveColorIdx), 1);
                                    for i := 0 to NumEMRegisters do
                                        Registers1[i] := Registers2[i];
                                end;
                            else
                            end;
                        end;

                    except
                        On E: Exception do
                            DoSimpleMsg
                            ('Error Reading File "' + FileName + '". ' + E.message,
                                195)
                    end;

                finally

                    CloseFile(F);

                end;
            end; { For CaseYear }
        end; { For iPass }
        ActiveColorStartThisCase := ActiveColorIdx;
      // Start next case where this one left off
        CaseName := CaseName2;
    end; { For CaseNames }

   { Make Diff Plot and Write output file }
    MakeDiffCurve;
    DiffColor := NextColor;
    FirstYear := TRUE;
    X1 := 0.0;
    Y1 := 0.0;

    for CaseYear := 0 to 20 do
    begin
        if X[1, CaseYear] >= 0.0 then
        begin
            if FirstYear then
            begin
                X1 := X[1, CaseYear];
                Y1 := HorizDiff[CaseYear];
                FirstYear := FALSE;
            end
            else
            begin
                AddNewLine(X1, Y1, X[1, CaseYear], HorizDiff[CaseYear], DiffColor,
                    1, psSolid, FALSE, ' ', FALSE, 0, 0, 0);
                MarkAt(X[1, CaseYear], HorizDiff[CaseYear],
                    GetMarker(ActiveColorIdx), 1);
                X1 := X[1, CaseYear];
                Y1 := HorizDiff[CaseYear];
            end;
        end;
    end;

    Set_Autorange(2.0); // 2% rim
   { Put on legend in upper left hand corner }
    Get_Properties(ActiveGraphProps);
    Xinc := 0.05 * (ActiveGraphProps.Xmax - ActiveGraphProps.Xmin);
    Yinc := 0.05 * (ActiveGraphProps.Ymax - ActiveGraphProps.Ymin);
    LegendX := ActiveGraphProps.Xmin + Xinc;
    LegendY := ActiveGraphProps.Ymax - Yinc;

    ActiveColorIdx := 0;
    DatColor := NextColor; // Next color automatically increments
    Set_DataColor(DatColor);
    LabelIdx := addTextLabel(LegendX + 0.5 * Xinc, LegendY - 0.5 * Yinc,
        DatColor, CaseName1, 0);
    LockInTextLabel(LabelIdx);

    LegendY := LegendY - Yinc;
    DatColor := NextColor; // Next color automatically increments
    Set_DataColor(DatColor);
    LabelIdx := addTextLabel(LegendX + 0.5 * Xinc, LegendY - 0.5 * Yinc,
        DatColor, CaseName2, 0);
    LockInTextLabel(LabelIdx);
    LegendY := LegendY - Yinc;
    DatColor := NextColor; // Next color automatically increments
    Set_DataColor(DatColor);
    LabelIdx := addTextLabel(LegendX + 0.5 * Xinc, LegendY - 0.5 * Yinc,
        DatColor, 'Difference', 0);
    LockInTextLabel(LabelIdx);

   { Write Output File }
    try
        FileName := CaseName2 + '-' + CaseName1 + '_Reg' + Trim(IntToStr(Reg)) + '.CSV';
        AssignFile(F, FileName);
        Rewrite(F);

        Writeln(F, '"MW Load", "' + CaseName1 + '", "MW Load", "' + CaseName2 +
            '", "Incr. Cap."');
        for CaseYear := 0 to 20 do
        begin
            if X[1, CaseYear] >= 0.0 then
                Writeln(F, Format('%-g, %-g, %-g, %-g, %-g', [X[1, CaseYear],
                    Y[1, CaseYear], X[2, CaseYear], Y[2, CaseYear],
                    HorizDiff[CaseYear]]));
        end;

        CloseFile(F);
        GlobalResult := FileName;

    except
        On E: Exception do
            DoSimpleMsg('Error writing file: "' + FileName + '". ' + E.message,
                196);
    end;

    set_KeepAspectRatio(FALSE);

//****   ShowGraph; { Form Freed on close }

    Names.Free;

end;

procedure TDSSPlot.DoYearlyCurvePlot(CaseNames: TStringList; WhichFile: String;
    iRegisters: array of Integer);

{ Plot yearly results from specified cases and registers in Totals.CSV files
  Vs Register 1 }

var
    F, Fout: TextFile;
    S, FileName, Param, CaseName: String;
    Names: TStringList;
    Registers1, Registers2: array [0 .. NumEMRegisters] of Double;
    XValue: array [0 .. 20] of Double;
    i, iPass, iX, iCase, CaseYear, ActiveColorStartThisCase: Integer;
    FirstYear: Boolean;
    LegendX, LegendY, Xinc, Yinc: Double;
    LabelIdx: Integer;
    SearchForMeterName: Boolean;
    ActiveGraphProps: TDSSGraphProperties;
    DatColor: TColor;

   { * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * * * * * }
   { Internal Procs }

    function ReadS: Boolean;
    begin
        Result := TRUE;
        if SearchForMeterName then
        begin
            repeat
                Readln(F, S);
                AuxParser[ActiveActor].CmdString := S;
                AuxParser[ActiveActor].NextParam;
            until (CompareText(WhichFile, AuxParser[ActiveActor].StrValue) = 0) or EOF(F);
            if (CompareText(WhichFile, AuxParser[ActiveActor].StrValue) = 0) then
            begin
                S := IntToStr(CaseYear) + Copy(S, Pos(',', S), 9999);
            end
            else
            begin
                Result := FALSE;
            end;
        end
        else
            Readln(F, S);
    end;

    procedure WriteFoutRecord(opt: Integer);
    var
        i: Integer;
    begin
        Write(Fout, Format('%s, %d, %.7g', [CaseName, CaseYear, XValue[iX]]));
        case opt of
            1:
                for i := 0 to High(iRegisters) do
                    Write(Fout, Format(', %.7g  ', [Registers1[iRegisters[i]]]));
            2:
                for i := 0 to High(iRegisters) do
                    Write(Fout, Format(', %.7g  ', [Registers2[iRegisters[i]]]));
        end;
        Writeln(Fout);

    end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

begin
   { Plot Demand interval data from saved results DI_Totals.CSV }

    Names := TStringList.Create;

    if MakeNewGraph(GetOutputDirectory + CircuitName_[ActiveActor] + 'YearlyPlot.DSV') = 0 then
    begin
        DoSimpleMsg('Make New Plot failed in DSSPlot -- yearly plot.', 8734);
        Exit;
    end;

    S := 'Yearly Curves for case(s)';
    for i := 0 to CaseNames.Count - 1 do
        S := S + ', ' + CaseNames.Strings[i];

    Set_Caption(s);
    Get_Properties(ActiveGraphProps);
    with ActiveGraphProps do
    begin
        ChartColor := clWhite;
        WindColor := clWhite;
    end;
    Set_Properties(ActiveGraphProps);

    S := 'Total Area MW';
    Set_XaxisLabel(S);
    S := 'MW, MWh or MVA';
    Set_YaxisLabel(S);

    try { ... Finally }

        AssignFile(Fout, 'LastYearlyCurvePlot.CSV');
        Rewrite(Fout);
        Write(Fout, 'Case, Year, TotalMW');
        if Assigned(ActiveEnergyMeterObj) then
            for i := 0 to high(iRegisters) do
                Write(Fout, Format(', "%s"',
                    [ActiveEnergyMeterObj.RegisterNames[iRegisters[i]]]))
        else
            for i := 0 to high(iRegisters) do
                Write(Fout, Format(', "Reg %d"', [iRegisters[i]]));
        Writeln(Fout);

      { Loop Through Cases }
        FirstYear := TRUE;
        ActiveColorStartThisCase := 0;
        for iCase := 0 to CaseNames.Count - 1 do
        begin
            CaseName := CaseNames.Strings[iCase];
            if DirectoryExists(CaseName) then
            // Do This in Two Passes to set the X Values at Register 7 of Totals.CSV
                for iPass := 1 to 2 do
                begin
               { Loop through possible caseyears (0..20) }
                    FirstYear := TRUE;
                    for CaseYear := 0 to 20 do
                    begin
                  { Open File }
                        SearchForMeterName := FALSE;
                        case iPass of
                            1:
                                FileName := CaseName + PathDelim + 'di_yr_' + Trim
                                    (IntToStr(CaseYear)) + PathDelim + 'Totals.CSV';
                        else
                        begin
                            if (CompareText(WhichFile, 'Totals') = 0) or
                                (CompareText(WhichFile, 'Systemmeter') = 0) then
                            begin
                                FileName := CaseName + PathDelim + 'di_yr_' + Trim
                                    (IntToStr(CaseYear)) + PathDelim + WhichFile + '.CSV';
                            end
                            else
                            begin
                                FileName := CaseName + PathDelim + 'di_yr_' + Trim
                                    (IntToStr(CaseYear)) + PathDelim +
                                    'EnergyMeterTotals.CSV';
                                SearchForMeterName := TRUE;
                            end;
                        end
                        end;

                        if not FileExists(FileName) then
                        begin
                            continue; // Skip if it doesnt exist
                        end
                        else
                        begin
                            try
                                AssignFile(F, FileName);
                                Reset(F);
                                Readln(F, S); // Read header line
                                case iPass of
                                    2:
                                        if (iCase = 0) and FirstYear then
                                        begin
                                            AuxParser[ActiveActor].CmdString := S;
                                            AuxParser[ActiveActor].NextParam;
                                            Param := AuxParser[ActiveActor].StrValue;
                                            while Length(Param) > 0 do
                                            begin
                                                Names.Add(Param);
                                                AuxParser[ActiveActor].NextParam;
                                                Param := AuxParser[ActiveActor].StrValue;
                                            end;
                                            S := 'Meter: ' + WhichFile + ', Registers: ';
                                            for i := 0 to High(iRegisters) do
                                                S := S + Names.Strings[iRegisters[i]] + ', ';
                                            Set_ChartCaption(S);
                                        end;
                                else
                           { Nada }
                                end;

                            except
                                On E: Exception do
                                    DoSimpleMsg
                                    ('Error Reading File "' + FileName + '". ' +
                                        E.message, 197)
                            end;

                        end;

                  { Get started - initialize Registers 1 }
                        try
                            try
                                if FirstYear then
                                begin
                                    if not EOF(F) then
                                    begin

                                        if not ReadS then
                                        begin // Reads S
                                            DoSimpleMsg
                                            ('Meter not found: "' + WhichFile + '"',
                                                1971);
                                            Exit;
                                        end;

                                        AuxParser[ActiveActor].CmdString := '"' + S + '"';
                                        AuxParser[ActiveActor].NextParam;
                                        LoadRegisters(@Registers1); // from auxparser
                                        iX := 0;
                                        case iPass of
                                            1:
                                                XValue[iX] := Registers1[7];
                                        else
                                            WriteFoutRecord(1);
                                        end;
                                        FirstYear := FALSE;
                                    end;
                                end
                                else
                                if not EOF(F) then
                                begin // Gotta have at least 2 years to make a plot

                                    ReadS; // Reads S  -- any errors will be caught on first pass
                                    AuxParser[ActiveActor].CmdString := '"' + S + '"';
                           // enclose in quotes to parse as array
                                    AuxParser[ActiveActor].NextParam;
                                    LoadRegisters(@Registers2); // from auxparser
                                    Inc(iX);
                                    case iPass of
                                        1:
                                            XValue[iX] := Registers2[7];
                                    else

                                        ActiveColorIdx := ActiveColorStartThisCase;
                                        for i := 0 to High(iRegisters) do
                                        begin
                                            AddNewLine(XValue[iX - 1],
                                                Registers1[iRegisters[i]], XValue[iX],
                                                Registers2[iRegisters[i]], NextColor, 2,
                                                psSolid, FALSE, ' ', FALSE, 0, 0, 0);
                                            MarkAt(XValue[iX], Registers2[iRegisters[i]],
                                                GetMarker(ActiveColorIdx), 1);
                                        end;
                                        WriteFoutRecord(2);
                                        for i := 0 to NumEMRegisters do
                                            Registers1[i] := Registers2[i];
                                    end;
                                end;

                            except
                                On E: Exception do
                                    DoSimpleMsg
                                    ('Error Reading File "' + FileName + '". ' +
                                        E.message, 198)
                            end;

                        finally

                            CloseFile(F);

                        end;
                    end; { For CaseYear }
                end; { For iPass }
            ActiveColorStartThisCase := ActiveColorIdx;
         // Start next case where this one left off
        end; { For CaseNames }

        if FirstYear then
        begin
            DoSimpleMsg('No Files Found', 199);
        end
        else
        begin
         { Put on legend in upper left hand corner }
            Get_Properties(ActiveGraphProps);
            Xinc := 0.05 * (ActiveGraphProps.Xmax - ActiveGraphProps.Xmin);
            Yinc := 0.05 * (ActiveGraphProps.Ymax - ActiveGraphProps.Ymin);
            LegendX := ActiveGraphProps.Xmin + Xinc;
            LegendY := ActiveGraphProps.Ymax - Yinc;
            ActiveColorIdx := 0;
            for iCase := 0 to CaseNames.Count - 1 do
            begin
                CaseName := CaseNames.Strings[iCase];
                if DirectoryExists(CaseName) then
                    for i := 0 to High(iRegisters) do
                    begin
                        S := CaseNames.Strings[iCase] + ', ' + Names.Strings
                            [iRegisters[i]];
                        DatColor := NextColor;
                        Set_DataColor(DatColor);
                        MarkAt(LegendX, LegendY, GetMarker(ActiveColorIdx), 1);
                        LabelIdx := addTextLabel(LegendX + 0.5 * Xinc,
                            LegendY - 0.5 * Yinc, DatColor, S,
                            0);
                        Set_LeftJustifyTransparent(LabelIdx);
                    end;
                LegendY := LegendY - Yinc;
            end;
        end;
        set_KeepAspectRatio(FALSE);

        Set_Autorange(2.0); // 2% rim
//****      ShowGraph; { Form Freed on close }

        Names.Free;

    finally

        CloseFile(Fout);

        GlobalResult := 'LastYearlyCurvePlot.CSV';

    end;

end;

function TDSSPlot.GetMarker(Idx: Integer): Byte;
begin
    repeat
        if Idx > 9 then
            Idx := Idx - 9;
    until Idx < 10;

    case Idx of
        1:
            Result := 5;
        2:
            Result := 15;
        3:
            Result := 2;
        4:
            Result := 8;
        5:
            Result := 26;
        6:
            Result := 36;
        7:
            Result := 39;
        8:
            Result := 19;
        9:
            Result := 18;
    else
        Result := 5;
    end;
end;

procedure TDSSPlot.LabelBuses;
{ Adds text to plot labeling buses }

var
    i: Integer;
begin
    for i := 1 to ActiveCircuit[ActiveActor].NumBuses do
    begin
        if Length(BusLabels^[i]) > 0 then
            if ActiveCircuit[ActiveActor].Buses^[i].CoordDefined then
                AddNewText(ActiveCircuit[ActiveActor].Buses^[i].X, ActiveCircuit[ActiveActor].Buses^[i].Y,
                    clBlack, 8, BusLabels^[i]);
    end;
end;

procedure TDSSPlot.DoMonitorPlot;

var
    Fversion, FSignature, iMode: Integer;
    hr, S: Single;
    i, Nread, RecordSize, RecordBytes: Cardinal;
    sngBuffer: array [1 .. 100] of Single;      // a big buffer
    StrBuffer: TMonitorStrBuffer;
    pStrBuffer: PAnsichar;
    time: Double;
    FirstRecord, Hours: Boolean;
    ChannelNames: array of String;
    Str: String;
    ItsAFreqScan: Boolean;
    NumberofRecords: Cardinal;
    Xarray: pDoubleArray;
    Yarray: array[0..100] of pDoubleArray;
    iCount: Integer;
    iChannel: Cardinal;

begin
   { Plot designated channels in monitor designated by ObjectName }
    if MonitorClass[ActiveActor].SetActive(ObjectName) then
    begin

        with TMonitorObj(MonitorClass[ActiveActor].GetActiveObj) do
        begin

            Save; // Save present buffer
            CloseMonitorStream(ActiveActor);

            FirstRecord := TRUE;
            Hours := TRUE;
            pStrBuffer := @StrBuffer;
            with MonitorStream do
            begin
                Seek(0, soFromBeginning); // Start at the beginning of the Stream
                Read(FSignature, Sizeof(FSignature));
                Read(Fversion, Sizeof(Fversion));
                Read(RecordSize, Sizeof(RecordSize));
                Read(iMode, Sizeof(iMode));
                Read(StrBuffer, Sizeof(StrBuffer));
            end;

            AuxParser[ActiveActor].Whitespace := '';
            AuxParser[ActiveActor].CmdString := String(pStrBuffer);
            SetLength(ChannelNames, RecordSize + 2);
            for i := 0 to RecordSize + 1 do
            begin
                AuxParser[ActiveActor].NextParam;
                ChannelNames[i] := AuxParser[ActiveActor].StrValue;
            end;
            AuxParser[ActiveActor].ResetDelims;   // restore original delimiters

            if CompareText(ChannelNames[0], 'Freq') = 0 then
                ItsAFreqScan := TRUE
            else
                ItsAFreqScan := FALSE;

         // pStr := @StrBuffer;
            RecordBytes := Sizeof(sngBuffer[1]) * RecordSize;
            NumberofRecords := (MonitorStream.Size - MonitorStream.Position) div RecordBytes;

         // Allocate arrays for plotting
            Xarray := Allocmem(Sizeof(Xarray^[1]) * NumberofRecords);
            for i := 0 to High(Channels) do
                Yarray[i] := Allocmem(Sizeof(Xarray^[1]) * NumberofRecords);

            iCount := 0;  // Loop count
            while not (MonitorStream.Position >= MonitorStream.Size) do
            begin
                with MonitorStream do
                begin
                    Read(hr, Sizeof(hr));
                    Read(S, Sizeof(S));
                    Nread := Read(sngBuffer, RecordBytes);
                end;
                if Nread < RecordBytes then
                    Break;

                Inc(iCount);

                if FirstRecord then
                begin
                    if (S > 0.0) and (S < 100.0) then
                        Hours := FALSE;
                end;
                if ItsAFreqScan then
                    time := hr // frequency value
                else
                if Hours then
                    time := hr + S / 3600.0 // in hrs
                else
                    time := hr * 3600.0 + S; // in sec

                Xarray^[iCount] := Time;

                for i := 0 to high(Channels) do
                begin
                    iChannel := Channels[i];
                    if iChannel <= RecordSize then  // check for legal channel number
                    begin
                        Yarray[i]^[iCount] := sngBuffer[iChannel] / Bases[i];
                    end;
                end;
                FirstRecord := FALSE;
            end;

            CloseMonitorStream(ActiveActor);

     // Add the curves to the plot
            ActiveColorIdx := 0;
            for i := 0 to high(Channels) do
            begin

                AddNewCurve(Xarray, Yarray[i], iCount,
                    NextColor, 2, psSolid, FALSE, 1, ChannelNames[Channels[i]]);

            end;

            if ItsAFreqScan then
                Str := 'Frequency, Hz'
            else
            if Hours then
                Str := 'Time, H'
            else
                Str := 'Time, s';
            Set_XaxisLabel(Str);
            Str := 'Mag';
            Set_YaxisLabel(Str);

            if Channels[0] <= RecordSize then
                Str := ObjectName + ': ' + ChannelNames[Channels[0] + 1];
            for i := 1 to high(Channels) do
                if Channels[i] <= RecordSize then
                    Str := Str + Format(', %s', [ChannelNames[Channels[i] + 1]]);
            Set_ChartCaption(Str);

        end; { With }

        Set_Autorange(2.0); // 2% rim
//***      ShowGraph;

  // de-Allocate arrays used for plotting
        Freemem(Xarray, Sizeof(Xarray^[1]) * NumberofRecords);
        for i := 0 to High(Channels) do
            Freemem(Yarray[i], Sizeof(Xarray^[1]) * NumberofRecords);
    end
    else
        DoSimpleMsg('Monitor "' + ObjectName + '" not found.', 200);
end;

procedure TDSSPlot.DoPriceShapePlot(const PriceShapeName: String);
var
    Price_Shape: TPriceShapeObj;
    Xarray: pdoubleArray;
    X, Xinc: Double;
    i: Integer;
    Xsize: Integer;
    XLabel: String;
    UseXarray: Boolean;
    S: String;

begin
    Price_Shape := PriceShapeClass[ActiveActor].Find(PriceShapeName);
    if Price_Shape = NIL then
    begin
        DoSimpleMsg('PriceShape object not found: "' + PriceShapeName + '"',
            87341);
        Exit;
    end;

    UseXarray := FALSE;
    Xarray := NIL;
    Xsize := 0; // Init

    if Price_Shape.Interval <> 0.0 then
        with Price_Shape do
        begin // have to gen up Xarray
            UseXarray := TRUE;
            Xsize := Sizeof(Xarray^[1]) * NumPoints;
            GetMem(Xarray, Xsize); // SetLength(Xarray, Numpoints);
            X := 0.0;
            if Interval * NumPoints < 1.0 then
            begin
                Xinc := Interval * 3600.0; // Plot secs
                XLabel := 'Seconds';
            end
            else
            begin
                Xinc := Interval;
                XLabel := 'Hours';
            end;
            for i := 1 to NumPoints do
            begin
                Xarray[i] := X;
                X := X + Xinc;
            end;
        end;

   // ** already exists MakeNewGraph;
    S := 'PriceShape.' + PriceShapeName;
    Set_Caption(S);
    S := 'PriceShape = ' + PriceShapeName;
    Set_ChartCaption(S);
    Set_XaxisLabel(XLabel);
    Set_YaxisLabel('Price');

    if UseXarray then
        AddNewCurve(Xarray, Price_Shape.PriceValues, Price_Shape.NumPoints,
            Color1, 1, psSolid, FALSE, 1, PriceShapeName)
    else
        AddNewCurve(Price_Shape.Hours, Price_Shape.PriceValues,
            Price_Shape.NumPoints, Color1, 1, psSolid, FALSE, 1,
            PriceShapeName);

    set_KeepAspectRatio(FALSE);

    if UseXarray then
        FreeMem(Xarray, Xsize);

    Set_Autorange(2.0); // 2% rim
//***   ShowGraph; { Form Freed on close }
end;

procedure TDSSPlot.DoProfilePlot;

{ Voltage profile plot. Tom Short Plot with lines }

var
    iEnergyMeter: Integer;
    ActiveEnergyMeter: TEnergyMeterObj;
    PresentCktElement: TDSSCktElement;
    Bus1, Bus2: TDSSBus;
    puV1, puV2: Double;
    iphs: Integer;
    iphs2: Integer;
    S: String;
    MyColor: TColor;
    LineType: TPenStyle;
    DSSGraphProps: TDSSGraphProperties;
    RangeLoY, RangeHiY: Double;
    DenomLL, DenomLN, LenScale, RangeScale: Double;

begin

   { New graph created before this routine is entered }
    case PhasesToPlot of
        PROFILELL, PROFILELLALL, PROFILELLPRI:
            S := 'L-L Voltage Profile';
    else
        S := 'L-N Voltage Profile';
    end;

    Set_Caption(S);
    Set_ChartCaption(S);
    if ProfileScale = PROFILE120KFT then
    begin
        Set_XaxisLabel('Distance (kft)');
        Set_YaxisLabel('120 Base Voltage');
        DenomLN := 1000.0 / 120.0;
        DenomLL := 1732.0 / 120.0;
        LenScale := 3.2809;
        RangeScale := 120.0;
    end
    else
    begin
        Set_XaxisLabel('Distance (km)');
        Set_YaxisLabel('p.u. Voltage');
        DenomLN := 1000.0;
        DenomLL := 1732.0;
        LenScale := 1.0;
        RangeScale := 1.0;
    end;

    Get_Properties(DSSGraphProps);
    with DSSGraphProps do
    begin
        GridStyle := gsDotLines;
        ChartColor := clWhite;
        WindColor := clWhite;
        Isometric := FALSE;
        EnableClickonDiagram;
    end;
    Set_Properties(DSSGraphProps);
    Set_TextAlignment(1);
    Set_KeyClass(DSSG_LINECLASS); { Line for searches }

    iEnergyMeter := EnergyMeterClass[ActiveActor].First;
    while iEnergyMeter > 0 do
    begin

        ActiveEnergyMeter := EnergyMeterClass[ActiveActor].GetActiveObj;
      { Go down each branch list and draw a line }
        PresentCktElement := ActiveEnergyMeter.BranchList.First;
        while PresentCktElement <> NIL do
        begin
            if IslineElement(PresentCktElement) then
                with ActiveCircuit[ActiveActor] do
                begin
                    ActiveCktElement := PresentCktElement;
                    Bus1 := Buses^[PresentCktElement.Terminals^[1].BusRef];
                    Bus2 := Buses^[PresentCktElement.Terminals^[2].BusRef];
               { Now determin which phase to plot }
                    if (Bus1.kVBase > 0.0) and (Bus2.kVBase > 0.0) then
                        case PhasesToPlot of
                     { 3ph only }
                            PROFILE3PH:
                                if (PresentCktElement.NPhases >= 3) and
                                    (Bus1.kVBase > 1.0) then
                                    for iphs := 1 to 3 do
                                    begin
                                        puV1 := Cabs
                                            (Solution.NodeV^[Bus1.GetRef
                                            (Bus1.FindIdx(iphs))]) / Bus1.kVBase / DenomLN;
                                        puV2 := Cabs
                                            (Solution.NodeV^[Bus2.GetRef
                                            (Bus2.FindIdx(iphs))]) / Bus2.kVBase / DenomLN;
                                        AddNewLine(Bus1.DistFromMeter * LenScale, puV1,
                                            Bus2.DistFromMeter * LenScale, puV2, ColorArray[iphs],
                                            2, psSolid, Dots,
                                            Format('%s.%s', [PresentCktElement.ParentClass.Name, PresentCktElement.Name]), FALSE, 0,
                                            NodeMarkerCode, NodeMarkerWidth);
                                    end;
                     { Plot all phases present (between 1 and 3) }
                            PROFILEALL:
                            begin
                                for iphs := 1 to 3 do
                                    if (Bus1.FindIdx(iphs) > 0) and
                                        (Bus2.FindIdx(iphs) > 0) then
                                    begin
                                        if Bus1.kVBase < 1.0 then
                                            LineType := psDot
                                        else
                                            LineType := psSolid;
                                        MyColor := ColorArray[iphs];
                                        puV1 := Cabs
                                            (Solution.NodeV^[Bus1.GetRef
                                            (Bus1.FindIdx(iphs))]) / Bus1.kVBase / DenomLN;
                                        puV2 := Cabs
                                            (Solution.NodeV^[Bus2.GetRef
                                            (Bus2.FindIdx(iphs))]) / Bus2.kVBase / DenomLN;
                                        AddNewLine(Bus1.DistFromMeter * LenScale, puV1,
                                            Bus2.DistFromMeter * LenScale, puV2, MyColor, 2,
                                            LineType, Dots,
                                            Format('%s.%s', [PresentCktElement.ParentClass.Name, PresentCktElement.Name]), FALSE, 0,
                                            NodeMarkerCode, NodeMarkerWidth);
                                    end;
                            end;
                     { Plot all phases present (between 1 and 3) for Primary only }
                            PROFILEALLPRI:
                            begin
                                if Bus1.kVBase > 1.0 then
                                    for iphs := 1 to 3 do
                                        if (Bus1.FindIdx(iphs) > 0) and
                                            (Bus2.FindIdx(iphs) > 0) then
                                        begin
                                            if Bus1.kVBase < 1.0 then
                                                LineType := psDot
                                            else
                                                LineType := psSolid;
                                            MyColor := ColorArray[iphs];
                                            puV1 := Cabs
                                                (Solution.NodeV^[Bus1.GetRef
                                                (Bus1.FindIdx(iphs))]) / Bus1.kVBase / DenomLN;
                                            puV2 := Cabs
                                                (Solution.NodeV^[Bus2.GetRef
                                                (Bus2.FindIdx(iphs))]) / Bus2.kVBase / DenomLN;
                                            AddNewLine(Bus1.DistFromMeter * LenScale, puV1,
                                                Bus2.DistFromMeter * LenScale, puV2, MyColor, 2,
                                                LineType, Dots,
                                                Format('%s.%s', [PresentCktElement.ParentClass.Name, PresentCktElement.Name]),
                                                FALSE, 0, NodeMarkerCode,
                                                NodeMarkerWidth);
                                        end;
                            end;
                            PROFILELL:
                            begin
                                if (PresentCktElement.NPhases >= 3) then
                                    for iphs := 1 to 3 do
                                    begin
                                        iphs2 := iphs + 1;
                                        if iphs2 > 3 then
                                            iphs2 := 1;
                                        if (Bus1.FindIdx(iphs) > 0) and
                                            (Bus2.FindIdx(iphs) > 0) and
                                            (Bus1.FindIdx(iphs2) > 0) and
                                            (Bus2.FindIdx(iphs2) > 0) then
                                        begin
                                            if Bus1.kVBase < 1.0 then
                                                LineType := psDot
                                            else
                                                LineType := psSolid;
                                            MyColor := ColorArray[iphs];
                                            with Solution do
                                            begin
                                                puV1 := Cabs
                                                    (CSUB
                                                    (NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))
                                                    ],
                                                    NodeV^[Bus1.GetRef
                                                    (Bus1.FindIdx(iphs2))])) / Bus1.kVBase / DenomLL;
                                                puV2 := Cabs
                                                    (CSUB
                                                    (NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))
                                                    ],
                                                    NodeV^[Bus2.GetRef
                                                    (Bus2.FindIdx(iphs2))])) / Bus2.kVBase / DenomLL;
                                            end;
                                            AddNewLine(Bus1.DistFromMeter * LenScale, puV1,
                                                Bus2.DistFromMeter * LenScale, puV2, MyColor, 2,
                                                LineType, Dots,
                                                Format('%s.%s', [PresentCktElement.ParentClass.Name, PresentCktElement.Name]),
                                                FALSE, 0, NodeMarkerCode,
                                                NodeMarkerWidth);
                                        end;
                                    end;
                            end;
                            PROFILELLALL:
                            begin
                                for iphs := 1 to 3 do
                                begin
                                    iphs2 := iphs + 1;
                                    if iphs2 > 3 then
                                        iphs2 := 1;
                                    if (Bus1.FindIdx(iphs) > 0) and
                                        (Bus2.FindIdx(iphs) > 0) and
                                        (Bus1.FindIdx(iphs2) > 0) and
                                        (Bus2.FindIdx(iphs2) > 0) then
                                    begin
                                        if Bus1.kVBase < 1.0 then
                                            LineType := psDot
                                        else
                                            LineType := psSolid;
                                        MyColor := ColorArray[iphs];
                                        with Solution do
                                        begin
                                            puV1 := Cabs
                                                (CSUB
                                                (NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))
                                                ],
                                                NodeV^[Bus1.GetRef
                                                (Bus1.FindIdx(iphs2))])) / Bus1.kVBase / DenomLL;
                                            puV2 := Cabs
                                                (CSUB
                                                (NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))
                                                ],
                                                NodeV^[Bus2.GetRef
                                                (Bus2.FindIdx(iphs2))])) / Bus2.kVBase / DenomLL;
                                        end;
                                        AddNewLine(Bus1.DistFromMeter * LenScale, puV1,
                                            Bus2.DistFromMeter * LenScale, puV2, MyColor, 2,
                                            LineType, Dots,
                                            Format('%s.%s', [PresentCktElement.ParentClass.Name, PresentCktElement.Name]), FALSE, 0,
                                            NodeMarkerCode, NodeMarkerWidth);
                                    end;
                                end;
                            end;
                            PROFILELLPRI:
                            begin
                                if Bus1.kVBase > 1.0 then
                                    for iphs := 1 to 3 do
                                    begin
                                        iphs2 := iphs + 1;
                                        if iphs2 > 3 then
                                            iphs2 := 1;
                                        if (Bus1.FindIdx(iphs) > 0) and
                                            (Bus2.FindIdx(iphs) > 0) and
                                            (Bus1.FindIdx(iphs2) > 0) and
                                            (Bus2.FindIdx(iphs2) > 0) then
                                        begin
                                            if Bus1.kVBase < 1.0 then
                                                LineType := psDot
                                            else
                                                LineType := psSolid;
                                            MyColor := ColorArray[iphs];
                                            with Solution do
                                            begin
                                                puV1 := Cabs
                                                    (CSUB
                                                    (NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))
                                                    ],
                                                    NodeV^[Bus1.GetRef
                                                    (Bus1.FindIdx(iphs2))])) / Bus1.kVBase / DenomLL;
                                                puV2 := Cabs
                                                    (CSUB
                                                    (NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))
                                                    ],
                                                    NodeV^[Bus2.GetRef
                                                    (Bus2.FindIdx(iphs2))])) / Bus2.kVBase / DenomLL;
                                            end;
                                            AddNewLine(Bus1.DistFromMeter * LenScale, puV1,
                                                Bus2.DistFromMeter * LenScale, puV2, MyColor, 2,
                                                LineType, Dots,
                                                Format('%s.%s', [PresentCktElement.ParentClass.Name, PresentCktElement.Name]),
                                                FALSE, 0, NodeMarkerCode,
                                                NodeMarkerWidth);
                                        end;
                                    end;
                            end;
                        else // plot just the selected phase
                            iphs := PhasesToPlot;
                            if (Bus1.FindIdx(iphs) > 0) and (Bus2.FindIdx(iphs) > 0) then
                            begin
                                if Bus1.kVBase < 1.0 then
                                    LineType := psDot
                                else
                                    LineType := psSolid;
                                MyColor := ColorArray[iphs];
                                puV1 := Cabs
                                    (ActiveCircuit[ActiveActor].Solution.NodeV^[Bus1.GetRef
                                    (Bus1.FindIdx(iphs))]) / Bus1.kVBase / DenomLN;
                                puV2 := Cabs
                                    (ActiveCircuit[ActiveActor].Solution.NodeV^[Bus2.GetRef
                                    (Bus2.FindIdx(iphs))]) / Bus2.kVBase / DenomLN;
                                AddNewLine(Bus1.DistFromMeter * LenScale, puV1,
                                    Bus2.DistFromMeter * LenScale, puV2, MyColor, 2, LineType,
                                    Dots,
                                    Format('%s.%s', [PresentCktElement.ParentClass.Name, PresentCktElement.Name]),
                                    FALSE,
                                    0, NodeMarkerCode, NodeMarkerWidth);
                            end;

                        end;

                end;

            PresentCktElement := ActiveEnergyMeter.BranchList.GoForward;
        end;
        iEnergyMeter := EnergyMeterClass[ActiveActor].Next;
    end;
    set_KeepAspectRatio(FALSE);
    Set_Autorange(2.0); // 2% rim
    Get_Properties(DSSGraphProps);
    with DSSGraphProps, ActiveCircuit[ActiveActor] do
    begin
      // AddNewLine(0.0, NormalMaxVolts, Xmax, NormalMaxVolts, ColorArray[1], 1, psDash, FALSE, 'Upper Limit', False, 0,0,0);
      // AddNewLine(0.0, NormalMinvolts, Xmax, NormalMinvolts, ColorArray[1], 1, psDash, FALSE, 'Lower Limit', False, 0,0,0);

      // --deprecated-- Get_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);
        RangeLoY := 0.90 * RangeScale;
        RangeHiY := 1.10 * RangeScale;
        Xmin := 0.0;
        Set_Range(Xmin, Xmax, RangeLoY, RangeHiY);

      { Keep this range for quick resetting }
      // --deprecated-- Xmin := RangeLoX;
      // --deprecated-- Xmax := RangeHiX;
        Ymin := RangeLoY;
        Ymax := RangeHiY;
        Set_LineWidth(3);
        Set_DataColor(clRed);
        Moveto(0.0, NormalMaxVolts * RangeScale);
        Drawto(Xmax, NormalMaxVolts * RangeScale);
        Moveto(0.0, NormalMinVolts * RangeScale);
        Drawto(Xmax, NormalMinVolts * RangeScale);
    end;
    Set_Properties(DSSGraphProps);
    Set_Autorange(2.0); // 2% rim
//****   ShowGraph;
end;

procedure TDSSPlot.MarkSpecialClasses;
{
   Place markers  at certain locations for special types of devices
}
begin

    if ActiveCircuit[ActiveActor].MarkTransformers then
        MarkTheTransformers;
    if ActiveCircuit[ActiveActor].MarkCapacitors then
        MarkTheCapacitors;
    if ActiveCircuit[ActiveActor].MarkRegulators then
        MarkTheRegulators;
    if ActiveCircuit[ActiveActor].MarkPVSystems then
        MarkThePVSystems;
    if ActiveCircuit[ActiveActor].MarkStorage then
        MarkTheStorage;
    if ActiveCircuit[ActiveActor].MarkFuses then
        MarkTheFuses;
    if ActiveCircuit[ActiveActor].MarkReclosers then
        MarkTheReclosers;
    if ActiveCircuit[ActiveActor].MarkRelays then
        MarkTheRelays;

    if ShowSubs then
        MarkSubTransformers;

    AddBusMarkers;

end;

procedure TDSSPlot.MarkSubTransformers;
begin
   { Mark Locations of Substation Transformers }
    pTransf := ActiveCircuit[ActiveActor].Transformers.First;
    Set_LineWidth(4);
    while pTransf <> NIL do
    begin
        if pTransf.Enabled then
            if pTransf.IsSubstation then
            begin
                Bus2Idx := pTransf.Terminals^[2].BusRef;
                if Bus2Idx > 0 then
                    if ActiveCircuit[ActiveActor].Buses^[Bus2Idx].CoordDefined then
                    begin
                        AddNewMarker(ActiveCircuit[ActiveActor].Buses^[Bus2Idx].X,
                            ActiveCircuit[ActiveActor].Buses^[Bus2Idx].Y, clRed, 36, 4);
                        if Length(pTransf.SubstationName) > 0 then
                            AddNewText(ActiveCircuit[ActiveActor].Buses^[Bus2Idx].X,
                                ActiveCircuit[ActiveActor].Buses^[Bus2Idx].Y, clBlack, 10,
                                ('  ' + pTransf.SubstationName));
                    end;
            end;
        pTransf := ActiveCircuit[ActiveActor].Transformers.Next;
    end;

end;

procedure TDSSPlot.MarkTheCapacitors;
var
    pCapacitor: TCapacitorObj;
    BusIdx: Integer;
    MyBus: TDSSBus;

begin
    pCapacitor := ActiveCircuit[ActiveActor].ShuntCapacitors.first;
    while pCapacitor <> NIL do
    begin
        if pCapacitor.Enabled then
        begin
            BusIdx := pCapacitor.Terminals^[1].BusRef;
            if BusIdx > 0 then
                with ActiveCircuit[ActiveActor] do
                begin
                    MyBus := Buses^[BusIdx];
                    if MyBus.CoordDefined then
                    begin
                        AddNewMarker(MyBus.X, MyBus.y, clRed, CapMarkerCode, CapMarkerSize);
                    end;
                end;
        end;
        pCapacitor := ActiveCircuit[ActiveActor].ShuntCapacitors.Next;
    end;
end;

procedure TDSSPlot.MarkTheFuses;
var
    pFuse: TFuseObj;
    BusIdx: Integer;
    MyBus: TDSSBus;
    FuseClass: TFuse;

begin
    FuseClass := GetDSSClassPtr('fuse') as TFuse;
    pFuse := TFuseObj(FuseClass.ElementList.first);
    while pFuse <> NIL do
    begin
        if pFuse.Enabled then
            if pFuse.ControlledElement.Drawn then
            begin

                BusIdx := pFuse.ControlledElement.Terminals^[pFuse.ElementTerminal].BusRef;
                if BusIdx > 0 then
                    with ActiveCircuit[ActiveActor] do
                    begin
                        MyBus := Buses^[BusIdx];
                        if MyBus.CoordDefined then
                        begin
                            AddNewMarker(MyBus.X, MyBus.y, clRed, FuseMarkerCode, FuseMarkerSize);
                        end;
                    end;
            end;
        pFuse := TFuseObj(FuseClass.ElementList.Next);
    end;

end;

procedure TDSSPlot.MarkTheReclosers;
var
    pRecloser: TRecloserObj;
    BusIdx: Integer;
    MyBus: TDSSBus;

begin
{ Mark only reclosers on Lines that are in the circuit}


    pRecloser := TRecloserObj(RecloserClass.ElementList.First);
    while pRecloser <> NIL do
    begin
        if pRecloser.Enabled then
        begin
            if pRecloser.ControlledElement.Drawn then
            begin
                BusIdx := pRecloser.ControlledElement.Terminals^[pRecloser.ElementTerminal].BusRef;
                if BusIdx > 0 then
                    with ActiveCircuit[ActiveActor] do
                    begin
                        MyBus := Buses^[BusIdx];
                        if MyBus.CoordDefined then
                        begin
                            AddNewMarker(MyBus.X, MyBus.y, clLime, RecloserMarkerCode, RecloserMarkerSize);
                        end;
                    end;
            end;
        end;
        pRecloser := TRecloserObj(RecloserClass.ElementList.Next);
    end;

end;

procedure TDSSPlot.MarkTheRelays;
var
    pRelay: TRelayObj;
    BusIdx: Integer;
    MyBus: TDSSBus;
    RelayClass: TRelay;

begin
    RelayClass := GetDSSClassPtr('Relay') as TRelay;
    pRelay := TRelayObj(RelayClass.ElementList.first);
    while pRelay <> NIL do
    begin
        if pRelay.Enabled then
        begin
            if pRelay.ControlledElement.Drawn then
            begin
                BusIdx := pRelay.ControlledElement.Terminals^[pRelay.ElementTerminal].BusRef;
                if BusIdx > 0 then
                    with ActiveCircuit[ActiveActor] do
                    begin
                        MyBus := Buses^[BusIdx];
                        if MyBus.CoordDefined then
                        begin
                            AddNewMarker(MyBus.X, MyBus.y, clMaroon, RelayMarkerCode, RelayMarkerSize);
                        end;
                    end;
            end;
        end;
        pRelay := TRelayObj(RelayClass.ElementList.Next);
    end;

end;

procedure TDSSPlot.MarkThePVSystems;
var
    pPVSystem: TPVSystemObj;
    BusIdx: Integer;
    MyBus: TDSSBus;

begin
    pPVSystem := ActiveCircuit[ActiveActor].PVSystems.first;
    while pPVSystem <> NIL do
    begin
        if pPVSystem.Enabled then
        begin
            BusIdx := pPVSystem.Terminals^[1].BusRef;
            if BusIdx > 0 then
                with ActiveCircuit[ActiveActor] do
                begin
                    MyBus := Buses^[BusIdx];
                    if MyBus.CoordDefined then
                    begin
                        AddNewMarker(MyBus.X, MyBus.y, clRed, PVMarkerCode, PVMarkerSize);
                    end;
                end;
        end;
        pPVSystem := ActiveCircuit[ActiveActor].PVSystems.Next;
    end;
end;

procedure TDSSPlot.MarkTheStorage;
var
    pStorage: TStorageObj;
    BusIdx: Integer;
    MyBus: TDSSBus;

begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.first;
    while pStorage <> NIL do
    begin
        if pStorage.Enabled then
        begin
            BusIdx := pStorage.Terminals^[1].BusRef;
            if BusIdx > 0 then
                with ActiveCircuit[ActiveActor] do
                begin
                    MyBus := Buses^[BusIdx];
                    if MyBus.CoordDefined then
                    begin
                        AddNewMarker(MyBus.X, MyBus.y, clRed, StoreMarkerCode, StoreMarkerSize);
                    end;
                end;
        end;
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Next;
    end;
end;

procedure TDSSPlot.MarkTheRegulators;
var
    pRegControl: TRegControlObj;
    pXfmr: TTransfObj;
    BusIdx: Integer;
    MyBus: TDSSBus;

begin
    pRegControl := ActiveCircuit[ActiveActor].RegControls.first;
    while pRegControl <> NIL do
    begin
        if pRegControl.Enabled then
        begin
            pXfmr := pRegControl.Transformer;
            BusIdx := pXfmr.Terminals^[pRegControl.TrWinding].BusRef;
            if BusIdx > 0 then
                with ActiveCircuit[ActiveActor] do
                begin
                    MyBus := Buses^[BusIdx];
                    if MyBus.CoordDefined then
                    begin
                        AddNewMarker(MyBus.X, MyBus.y, clRed, RegMarkerCode, RegMarkerSize);
                    end;
                end;
        end;
        pRegControl := ActiveCircuit[ActiveActor].RegControls.Next;
    end;

end;

procedure TDSSPlot.MarkTheTransformers;
var
    Bus1Idx: Integer;
    Bus2Idx: Integer;
    Xtr, Ytr: Double;

begin
   { Mark Locations of  Transformers }
    pTransf := ActiveCircuit[ActiveActor].Transformers.First;
    Set_LineWidth(1);
    while pTransf <> NIL do
    begin
        if pTransf.Enabled then
            if not pTransf.IsSubstation then
            begin
                Bus1Idx := pTransf.Terminals^[1].BusRef;
                Bus2Idx := pTransf.Terminals^[2].BusRef;
                if (Bus1Idx > 0) and (Bus2Idx > 0) then
                    with ActiveCircuit[ActiveActor] do
                        if Buses^[Bus1Idx].CoordDefined or Buses^[Bus2Idx]
                            .CoordDefined then
                        begin
                            if Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx]
                                .CoordDefined then
                            begin
                                Xtr := (Buses^[Bus1Idx].X + Buses^[Bus2Idx].X) / 2.0;
                                Ytr := (Buses^[Bus1Idx].Y + Buses^[Bus2Idx].Y) / 2.0;
                            end
                            else
                            if Buses^[Bus1Idx].CoordDefined then
                            begin
                                Xtr := Buses^[Bus1Idx].X;
                                Ytr := Buses^[Bus1Idx].Y;
                            end
                            else
                            begin
                                Xtr := Buses^[Bus2Idx].X;
                                Ytr := Buses^[Bus2Idx].Y;
                            end;
                            AddNewMarker(Xtr, Ytr, clRed, TransMarkerCode,
                                TransMarkerSize);
                        end;
            end;
        pTransf := ActiveCircuit[ActiveActor].Transformers.Next;
    end;

end;

{ TGenPlotItem }

constructor TGenPlotItem.Create;
begin
    SetLength(Name, 0);
end;

destructor TGenPlotItem.Destroy;
begin
    Name := '';
    inherited;
end;

{ TGenPlotItemList }

destructor TGenPlotItemList.Destroy;
var
    i: Integer;
begin

    for i := 0 to Count - 1 do
        TGenPlotItem(items[i]).Free;
    inherited;

end;

function TDSSPlot.QuantityString: String;
begin
    case Quantity of
        pqVoltage:
            Result := 'Voltage';
        pqPower:
            Result := 'Power';
        pqCurrent:
            Result := 'Current';
        pqLosses:
            Result := 'Loss Density';
        pqCapacity:
            Result := 'Capacity';
        pqNone:
        begin
            if PlotType = ptGeneralCircuitPlot then
                Result := FGeneralCircuitPlotQuantity
            else
                Result := '';
        end
    else
        Result := ''
    end;
end;

procedure TDSSPlot.SetMaxScale;

begin
    if not MaxScaleIsSpecified then
        case Quantity of
            pqVoltage:
            begin
            end;

            pqLosses:
            begin
                maxScale := 0.0;
                pLine := ActiveCircuit[ActiveActor].Lines.First;
                while pLine <> NIL do
                begin
                    if pLine.Enabled then
                        with pLine do
                        begin
                        // ----ActiveTerminalIdx := 1;
                            MaxScale := Max(MaxScale, abs(pLine.Losses[ActiveActor].re / pLine.Len))
                        end;
                    pLine := ActiveCircuit[ActiveActor].Lines.Next;
                end;
                MaxScale := MaxScale * 0.001;
            end;
            pqPower:
            begin
                maxScale := 0.0;
                pLine := ActiveCircuit[ActiveActor].Lines.First;
                while pLine <> NIL do
                begin
                    if pLine.Enabled then
                        with pLine do
                        begin
                        // ----ActiveTerminalIdx := 1;
                            MaxScale := Max(MaxScale, abs(Power[1, ActiveActor].re))
                        end;
                    pLine := ActiveCircuit[ActiveActor].Lines.Next;
                end;
                MaxScale := MaxScale * 0.001;
            end;
            pqCurrent:
            begin
            end;

            pqCapacity:
            begin
            end;

            pqNone:
            begin
                if PlotType = ptGeneralCircuitPlot then
                begin
                    pLine := ActiveCircuit[ActiveActor].Lines.First;
                    while pLine <> NIL do
                    begin
                        if pLine.Enabled then
                            with pLine do
                            begin
                              // ----ActiveTerminalIdx := 1;
                                MaxScale := Max(MaxScale, abs(GeneralPlotQuantity))
                            end;
                        pLine := ActiveCircuit[ActiveActor].Lines.Next;
                    end;

                end;
            end;
        else
        end;

end;

procedure TDSSPlot.DoGeneralCircuitPlot;
var
    LineStyleType: TPenStyle;

begin

   { Draw the lines With the thickness proportional to the data loaded in the general line data file }
    pLine := ActiveCircuit[ActiveActor].Lines.First;

    with ActiveCircuit[ActiveActor] do
        while pLine <> NIL do
        begin
            if pLine.Enabled then
            begin
                pLine.Drawn := TRUE;
                ActiveCktElement := pLine;
                Bus1Idx := pLine.Terminals^[1].BusRef;
                Bus2Idx := pLine.Terminals^[2].BusRef;
                if Buses^[Bus1Idx].CoordDefined and Buses^[Bus2Idx]
                    .CoordDefined then
                begin
                    if pLine.IsSwitch then
                        AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                            Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, TColor($0080FF), 1,
                            Style(1), Dots, Format('Line.%s', [pLine.Name]), MarkSwitches,
                            SwitchMarkerCode, NodeMarkerCode, NodeMarkerWidth)

                    else
                    if pLine.IsIsolated then
                        AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                            Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, clFuchsia, 3,
                            Style(1), Dots, Format('Line.%s', [pLine.Name]), MarkSwitches,
                            SwitchMarkerCode, NodeMarkerCode, NodeMarkerWidth)
                    else
                    begin
                        if pLine.NPhases = 1 then
                            LineStyleType := Style(SinglePhLineStyle)
                        else
                            LineStyleType := Style(ThreePhLineStyle);

                        AddNewLine(Buses^[Bus1Idx].X, Buses^[Bus1Idx].Y,
                            Buses^[Bus2Idx].X, Buses^[Bus2Idx].Y, GetColor,
                            Thickness, LineStyleType, Dots, Format('Line.%s', [pLine.Name]),
                            FALSE, 0, NodeMarkerCode, NodeMarkerWidth);
                    end;
                    if Labels then
                        DoBusLabels(Bus1Idx, Bus2Idx);
                end;
            end;
            pLine := Lines.Next;
        end;


   // AddBusMarkers; // Add default bus markers to line plot

end;

procedure TDSSPlot.LoadGeneralLineData;

var
    F: TextFile;
    Line: String;
    i: Integer;
    Param: String;
    LineNm: String;
    LineClass: TLine;
    MaxValue: Double;
    MinValue: Double;
    IsLine: Boolean;

begin

    LineClass := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('Line'));

   { Initialize General Line Quantity }
    pLine := ActiveCircuit[ActiveActor].Lines.First;

    while pLine <> NIL do
    begin
        if pLine.Enabled then
            pLine.GeneralPlotQuantity := 0.0;
        pLine := ActiveCircuit[ActiveActor].Lines.Next;
    end;

    try
        AssignFile(F, ObjectName);
        Reset(F);
        Readln(F, Line); // Get FieldName
        AuxParser[ActiveActor].CmdString := Line;
        AuxParser[ActiveActor].NextParam; { Bus Name }
        for i := 1 to ValueIndex do
            AuxParser[ActiveActor].NextParam;
        FGeneralCircuitPlotQuantity := AuxParser[ActiveActor].StrValue;

      { Find min and max }
        MaxValue := -1.0E50;
        MinValue := 1.0E50;

        while not EOF(F) do
        begin
            Readln(F, Line);
            if Length(Line) > 0 then
            begin
                AuxParser[ActiveActor].CmdString := Line;
                AuxParser[ActiveActor].NextParam; { Branch Name }
                Param := AuxParser[ActiveActor].StrValue;

            { Look for a line with this name }
                IsLine := TRUE;
                if Pos('.', Param) > 0 then
                begin
                    if CompareTextShortest(Param, 'line') = 0 then
                        LineNm := Copy(Param, Pos('.', Param) + 1, Length(Param))
                    else
                        IsLine := FALSE;

                end
                else
                    LineNm := Param;

                if IsLine then

                    if LineClass.SetActive(LineNm) then
                    begin

                        for i := 1 to ValueIndex do
                            AuxParser[ActiveActor].NextParam;
                        if Length(AuxParser[ActiveActor].StrValue) > 0 then
                        begin { Ignore empty fields }
                            with TLineObj(LineClass.GetActiveObj) do
                            begin
                                GeneralPlotQuantity := AuxParser[ActiveActor].DblValue;
                                MaxValue := Max(GeneralPlotQuantity, MaxValue);
                                MinValue := Min(GeneralPlotQuantity, MinValue);
                            end;
                        end;

                    end;
            end;
        end; { WHILE }

    finally
        CloseFile(F);
    end;

end;

procedure TDSSPlot.DoVisualizationPlot(Element: TDSSCktElement;
    Quantity: Integer);

var
    cBuffer: pComplexArray;
    Nterm, Ncond: Integer;
    kVBase1: array [1 .. 2] of Double;
    i, j, k: Integer;
    CBufferAllocated: Boolean;
    S1, S2, S, arrowLeft, arrowright: String;
    TopY, Xmx: Double;
    cResidual: Complex;
    Idx: Integer;
    xx: Double;
    ActiveGraphProps: TDSSGraphProperties;
    Fname: String;
   // RangeLoX, RangeHiX, RangeLoY, RangeHiY: Double;

   { ----------------------INTERNAL FUNCTIONS--------------------------- }
    procedure GetS;
    begin
        case Quantity of
            vizPOWER:
            begin
                S1 := Format('%-.6g + j', [cBuffer^[k].re]);
                S2 := Format('%-.6g kVA', [cBuffer^[k].im]);
            end;
            vizVOLTAGE:
            begin
                if k <= Ncond then
                    S1 := Format('%-.6g', [Cabs(cBuffer^[k]) / kVBase1[1]])
                else
                    S1 := Format('%-.6g', [Cabs(cBuffer^[k]) / kVBase1[2]]);
                S2 := Format(' /_ %8.2f', [cdang(cBuffer^[k])]);
            end
        else
            S1 := Format('%-.6g', [Cabs(cBuffer^[k])]);
            S2 := Format(' /_ %8.2f', [cdang(cBuffer^[k])]);
        end;
    end;

    procedure DrawArrow(Y: Double; const Txt1, Txt2: String;
        iopt: Integer);

    begin

        Set_FontStyle(fsBold);
        if iopt = 1 then
        begin
            Moveto(0.0, Y);
            Drawto(100.0, Y);
            CenteredText15(15.0, (Y + 2.0), 10, Txt1);
            CenteredText15(60.0, (Y + 2.0), 10, Txt2);
            CenteredText15(90.0, (Y + 2.0), 10, arrowright);

            // idx := AddTextLabel(50.0, (Y+1.0), clBlack, , 0);
        end
        else
        begin
            Moveto(Xmx, Y);
            Drawto(Xmx - 100.0, Y);
            CenteredText15(Xmx - 90.0, (Y + 2.0), 10, arrowLeft);
            CenteredText15(Xmx - 60.0, (Y + 2.0), 10, Txt1);
            CenteredText15(Xmx - 20.0, (Y + 2.0), 10, Txt2);
            // idx := AddTextLabel(Xmx-50, (Y+1.0), clBlack, Arrowleft+Txt, 0);
        end;

         // TextLabels[idx].Font.Style := [fsBold];

    end;

         { ------------------------------------------------------------------- }

begin
         { Plot Lines representing the phases and ground }

    Ncond := Element.NConds;
    Nterm := Element.Nterms;
    CBufferAllocated := FALSE;

    Element.ComputeIterminal(ActiveActor);
    Element.ComputeVTerminal(ActiveActor);

    Xmx := 300.0; // don't use Xmax -- already used
    for i := 1 to 2 do
        kVBase1[i] := 1.0;

    case Quantity of
        vizVOLTAGE:
        begin
            arrowLeft := '^ ';
            arrowright := ' ^';
        end;
    else
        arrowLeft := '<- ';
        arrowright := ' ->';
    end;


    Fname := GetOutputDirectory + CircuitName_[ActiveActor];
    case Quantity of
        vizVOLTAGE:
        begin
            FName := FName + Format('%s_%s_V.DSV', [Element.ParentClass.Name, Element.Name]);
            cBuffer := Element.Vterminal;
            for i := 1 to Min(2, Nterm) do
                kVBase1[i] := Max(1.0,
                    1000.0 * ActiveCircuit[ActiveActor].Buses^[Element.Terminals[i].BusRef]
                    .kVBase);
        end;
        vizCURRENT:
        begin
            FName := FName + Format('%s_%s_I.DSV', [Element.ParentClass.Name, Element.Name]);
            cBuffer := Element.Iterminal;
        end;
        vizPOWER:
        begin
            FName := FName + Format('%s_%s_PQ.DSV', [Element.ParentClass.Name, Element.Name]);
            cBuffer := Allocmem(Sizeof(Complex) * Element.Yorder);
            CBufferAllocated := TRUE;
            with Element do
            begin
                for i := 1 to Yorder do
                    cBuffer^[i] := CmulReal(Cmul(Vterminal^[i],
                        conjg(Iterminal^[i])), 0.001);
            end;
        end;
    end;

    if MakeNewGraph(Fname) = 0 then
    begin
        DoSimpleMsg('Make New Plot failed in DSSPlot - visualization plot.', 8734);
        Exit;
    end;

    Get_Properties(ActiveGraphProps);
    xx := 0.0;
    with ActiveGraphProps do
    begin
        ChartColor := clWhite;
        WindColor := clWhite;
        GridStyle := gsNone;
        Set_NoScales; // Set for no scales on X or Y

        S1 := Element.ParentClass.Name + '.' + UpperCase(Element.Name);
        case Quantity of
            vizVOLTAGE:
                S := S1 + ' Voltages';
            vizCURRENT:
                S := S1 + ' Currents';
            vizPOWER:
                S := S1 + ' Powers';
        end;
        Set_Caption(S);
        Set_ChartCaption(S);

         { Draw a box }
        TopY := 10.0 + (Ncond + 1) * 10.0;
        Rectangle(100.0, 10.0, Xmx - 100.0, TopY);
        Idx := addTextLabel(Xmx / 2.0, 15.0, clBlack, S1, 0);
        BoldTextLabel(Idx);

         { Draw the Ground Plane }
        Set_LineWidth(7);
        Set_DataColor(clGray);
        Moveto(0.0, 0.0);
        Drawto(Xmx, 0.0);
        Set_DataColor(clBlack);

         { Put the Quantities on The Box }
        k := 0;
        for i := 1 to Min(2, Nterm) do
        begin
            Set_LineWidth(3);
            for j := 1 to Element.NPhases do
            begin
                Inc(k);
                GetS;
                DrawArrow(TopY - j * 10.0, S1, S2, i);
            end;
            Set_LineWidth(1);
            for j := Element.NPhases + 1 to Ncond do
            begin
                Inc(k);
                GetS;
                DrawArrow(TopY - j * 10.0, S1, S2, i);
            end;

            { Add Residual Current }
            if Quantity = vizCURRENT then
            begin
                cResidual := CZERO;
                for j := 1 to Ncond do
                    Caccum(cResidual, Cnegate(cBuffer^[j + (i - 1) * Ncond]));
                S1 := Format('%-.6g', [Cabs(cResidual)]);
                S2 := Format(' /_ %8.2f', [cdang(cResidual)]);
                DrawArrow(-10.0, S1, S2, i);
            end;

            { Draw Bus and Label }
            Set_LineWidth(7);
            case i of
                1:
                    xx := -5.0;
                2:
                    xx := Xmx + 5.0;
            end;
            Moveto(xx, 5.0);
            Drawto(xx, TopY - 5.0);
            case i of
                1:
                    xx := 25;
                2:
                    xx := Xmx - 25.0;
            end;
            CenteredText15(xx, TopY, 10, UpperCase(Element.GetBus(i)));

        end;

        case Quantity of
            vizVOLTAGE:
                S := ' Voltages';
            vizCURRENT:
                S := ' Currents';
            vizPOWER:
                S := ' Powers';
        end;

        Set_Caption(S);
        Set_Autorange(5.0); // 5% rim

         (* OLD WAY
           With  ActiveGraphProps Do Begin

           Get_Range(RangeLoX, RangeHiX, RangeLoY, RangeHiY);

           {Keep this range for quick resetting}
           Xmin := RangeLoX;
           Xmax := RangeHiX;
           Ymin := RangeLoY;
           Ymax := RangeHiY;
           End;
           Set_Properties(ActiveGraphProps);
           *)
        ShowGraph;

    end;

    if CBufferAllocated then
        Reallocmem(cBuffer, 0);

end;

procedure TDSSPlot.Set_MaxLineThickness(const Value: Integer);
begin
    if (Value > 0) then
        FMaxLineThickness := Value;
end;

initialization

    DSSPlotObj := NIL; // Instantiate only if Plot command issued

    SinglePhLineStyle := 1;
    ThreePhLineStyle := 1;

finalization

    if Assigned(DSSPlotObj) then
        DSSPlotObj.Free;

end.
