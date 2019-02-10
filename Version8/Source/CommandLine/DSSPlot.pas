unit DSSPlot;

{
  Unit for
    3/29/03
}

interface

uses
    Line,
    Transformer, {Graphics,} Classes,
    CktElement;

const
    vizCURRENT = 1;
    vizVOLTAGE = 2;
    vizPOWER = 3;

type
    TPlotType = (ptAutoAddLogPlot, ptCircuitplot, ptGeneralDataPlot, ptGeneralCircuitPlot, ptmonitorplot, ptdaisyplot, ptMeterZones, ptLoadShape);
    TPlotQuantity = (pqVoltage, pqCurrent, pqPower, pqLosses, pqCapacity, pqNone);
    TColor = Integer;

    TDSSPlot = class(TObject)
    PRIVATE

    PROTECTED

    PUBLIC

        PlotType: TPlotType;
        MaxScale: Double;
        Dots,
        Labels,
        ShowLoops: Boolean;  // applies to Meterzone plots only
        Quantity: TPlotQuantity;
        ObjectName,
        FeederName: String;
        ValueIndex,
        MarkerIdx: Integer;  {For General & AutoAdd}

        Channels: array of Cardinal;  // for Monitor Plot
        Bases: array of Double;

        Color1,
        Color2,
        Color3: Integer {TColor};

        DaisyBusList: TStringList;

        ShowSubs: Boolean;
        MaxLineThickness: Integer;

       {Tri-color plots}
        TriColorMax, TriColorMid: Double;

        MaxScaleIsSpecified: Boolean;

        constructor Create;
        destructor Destroy; OVERRIDE;

        procedure Execute;
        procedure SetDefaults;

        procedure DoLoadShapePlot(const LoadShapeName: String);
        procedure DoDI_Plot(const CaseName: String; CaseYear: Integer; iRegisters: array of Integer; PeakDay: Boolean; const MeterName: String);
        procedure DoCompareCases(CaseName1, CaseName2, Whichfile: String; Reg: Integer);
        procedure DoYearlyCurvePlot(CaseNames: TStringList; Whichfile: String; iRegisters: array of Integer);
        procedure DoVisualizationPlot(Element: TDSSCktElement; Quantity: Integer);

    end;

procedure AddNewMarker(X, Y: Double; Color: TColor; Symbol, Size: Byte);
procedure ShowGraph;

var
    DSSPlotObj: TDSSPlot;
    AddMarkerColor: Integer{TColor};
    AddMarkerCode, AddMarkerSize: Integer;

implementation

procedure ShowGraph;
begin
       {Do Nothing}
end;

procedure AddNewMarker(X, Y: Double; Color: TColor; Symbol, Size: Byte);
begin
       {Do Nothing}
end;


{
TDSSPlot
}

{Var
    DssGraph:TDSSGraphFormSDL;   }

procedure AllocateBusLabels;
begin
         {Do Nothing}

end;

procedure FreeBusLabels;

begin
        {Do Nothing}

end;


constructor TDSSPlot.Create;
begin
    SetDefaults;
end;

destructor TDSSPlot.Destroy;
begin

    inherited;

end;


procedure TDSSPlot.Execute;


begin
      {Do Nothing}

end;


function InterpByte(b1, b2: Integer): Integer;
begin
         {Do Nothing}

    Result := 0;

end;


procedure TDSSPlot.SetDefaults;
begin

      {Do Nothing}

end;


procedure TDSSPlot.DoLoadShapePlot(const LoadShapeName: String);


begin
       {Do Nothing}


end;

procedure TDSSPlot.DoDI_Plot(const CaseName: String; CaseYear: Integer;
    iRegisters: array of Integer; PeakDay: Boolean; const MeterName: String);


begin
      {Do Nothing}

end;

procedure TDSSPlot.DoCompareCases(CaseName1, CaseName2, Whichfile: String;
    Reg: Integer);

{Compare a register from to cases in the Totals.CSV file, compute horiz distance,
 plot vs 1st register of totals.csv file}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
begin
      {Do Nothing}

end;

procedure TDSSPlot.DoYearlyCurvePlot(CaseNames: TStringList; Whichfile: String;
    iRegisters: array of Integer);

   {Plot yearly results from specified cases and registers in Totals.CSV files
     Vs Register 1}


begin
       {Do Nothing}


end;

procedure TDSSPlot.DoVisualizationPlot(Element: TDSSCktElement; Quantity: Integer);
begin
       {Do Nothing}
end;


initialization

    DSSPlotObj := NIL;   // Instantiate only if Plot command issued

finalization

    if Assigned(DSSPlotObj) then
        DSSPlotObj.Free;

end.
