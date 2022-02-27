unit PlotOptions;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Command,
    DSSClass;

type
{$SCOPEDENUMS ON}
    TPlotOption = (
        INVALID = 0,
        typ = 1, // type
        quantity = 2,
        max = 3,
        dots = 4,
        labels = 5,
        obj = 6, // object
        showloops = 7,
        r3 = 8,
        r2 = 9,
        c1 = 10,
        c2 = 11,
        c3 = 12,
        channels = 13,
        bases = 14,
        subs = 15,
        thickness = 16,
        buslist = 17,
        min = 18,
        __3phLinestyle = 19,
        __1phLinestyle = 20,
        phases = 21,
        profilescale = 22,
        PlotID = 23
    );
{$SCOPEDENUMS OFF}
const
    NumPlotOptions = ord(High(TPlotOption));

function DoPlotCmd(DSS: TDSSContext): Integer;

var
    PlotOption: array[1..NumPlotOptions] of String;
    PlotCommands: TCommandList;

implementation

uses
    Classes,
    fpjson,
    ArrayDef,
    DSSGlobals,
    SysUtils,
    ParserDel,
    Utilities,
    Circuit,
    StrUtils,
    TypInfo,
    DSSHelper;

type
    Opt = TPlotOption;

    TDSSPlot = class(TObject)
    PUBLIC
        PlotType: String;
        MatrixType: String;
        MaxScale, MinScale: Double;
        Dots, Labels, ShowLoops, { applies to Meterzone plots only }
        ShowSubs: Boolean;
        Quantity: String;
        ObjectName: String;
        PlotID: String;
        ValueIndex{, MarkerIdx}: Integer; { For General & AutoAdd }

        PhasesToPlot: Integer; // Profile Plot
        ProfileScale: Integer; // CYMDIST or pu/km scaling

        Channels: array of Cardinal; // for Monitor Plot
        Bases: array of Double; // for Monitor Plot

        Color1, Color2, Color3: Integer;

        // Tri-color plots
        TriColorMax, TriColorMid: Double;

        MaxScaleIsSpecified: Boolean;
        MinScaleIsSpecified: Boolean;
        DaisyBusList: TStringList;
        MaxLineThickness: Integer;
        SinglePhLineStyle: Integer;
        ThreePhLineStyle: Integer;

        constructor Create;
    end;

constructor TDSSPlot.Create;
begin
    MaxScale := 0.0; // Find MaxScale
    MaxScaleIsSpecified := FALSE; // indicates take the default
    MinScale := 0.0; // Find MinScale
    MinScaleIsSpecified := FALSE; // indicates take the default

    Dots := FALSE;
    Labels := FALSE;
    ShowLoops := FALSE;
    ShowSubs := FALSE;
    Quantity := 'Power';
    PlotType := 'CircuitPlot';
    MatrixType := '';
    // MarkerIdx := 24;
    ObjectName := '';

    MaxLineThickness := 10;

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

//    ActiveColorIdx := 0;
//    SetColorArray;

    ThreePhLineStyle := 1;
    SinglePhLineStyle := 1;
    DaisyBusList := TStringList.Create;
    PhasesToPlot := PROFILE3PH;
    ProfileScale := PROFILEPUKM;
end;

procedure DefineOptions;
var
    info: Pointer;
    i: Integer;
    name: String;
begin
    info := TypeInfo(Opt);
    for i := 1 to NumPlotOptions do
    begin
        name := ReplaceStr(GetEnumName(info, i), '__', '');
        if name = 'typ' then
            name := name + 'e'
        else if name = 'obj' then
            name := 'object';

        PlotOption[i] := name;
    end;
end;

function ColorToHTML(const c: Integer): String;
begin
    Result := (
        '#' + 
        IntToHex((c and clRed), 2) +
        IntToHex((c and clLime) shr 8, 2) +
        IntToHex((c and clBlue) shr 16, 2)
    );
end;

function DoPlotCmd(DSS: TDSSContext): Integer;
// Parse plot options and feed the callback function, if any
var
    ParamName, Param: String;
    ParamPointer, i: Integer;
    DblBuffer: array[0..50] of Double;
    NumChannels: Integer;
    DSSPlotObj: TDSSPlot;
    plotParams: TJSONObject = NIL;
    jsonDaisyBusList: TJSONArray = NIL;
    jsonBases: TJSONArray = NIL;
    jsonChannels: TJSONArray = NIL;
    plotParamsStr: String;
    Parser: TDSSParser;
    ActiveCircuit: TDSSCircuit;
begin
    Result := 0;

    if NoFormsAllowed then
    begin
        Result := 1;
        Exit;
    end;
    if (@DSS.DSSPlotCallback) = NIL then
    begin
        Result := 1;
        // If there is no callback active, just ignore the command like we did before.
        // DoSimpleMsg(DSS, 'Plotting not supported in the DSS Extensions engine. Provide a callback that implements it version', 308);
        Exit;
    end;
    
    Parser := DSS.Parser;
    ActiveCircuit := DSS.ActiveCircuit;
    DSSPlotObj := TDSSPlot.Create;

    // Get next parameter on command line
    ParamPointer := 0;
    ParamName := Uppercase(Parser.NextParam);
    Param := Uppercase(Parser.StrValue);
    while Length(Param) > 0 do
    begin
        // Interpret Parameter
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)
        else
            ParamPointer := PlotCommands.Getcommand(ParamName);

        // Check options requiring a solution and abort if no solution or circuit
        case ParamPointer of
            1:
                case Param[1] of
                    'A', 'C', 'D', 'G', 'M', 'P', 'Z':
                        if not (CompareTextShortest('pri', Param) = 0) then   // allow Price shape
                        begin
                            if not assigned(ActiveCircuit) then
                            begin
                                DoSimpleMsg(DSS, _('No circuit created.'), 24731);
                                Exit;
                            end;
                            if not assigned(ActiveCircuit.Solution) or not assigned(ActiveCircuit.Solution.NodeV) then
                            begin
                                DoSimpleMsg(DSS, _('The circuit must be solved before you can do this.'), 24732);
                                Exit;
                            end;
                        end;
                end;
        end;

        with DSSPlotObj do
            case ParamPointer of

                1:
                    case Param[1] of
                        'A':
                        begin
                            PlotType := 'AutoAddLogPlot';
                            ObjectName := DSS.CircuitName_ + 'AutoAddLog.csv';
                            ValueIndex := 2;
                        end;
                        'C':
                            PlotType := 'CircuitPlot';
                        'E':
                            if CompareTextShortest('ener', Param) = 0 then
                                PlotType := 'EnergyPlot'
                            else
                                PlotType := 'EvolutionPlot'
                            ;
                        'G':
                            PlotType := 'GeneralDataPlot';
                        'L':
                            PlotType := 'LoadShape';
                        'M':
                            if CompareTextShortest('mon', Param) = 0 then
                                PlotType := 'MonitorPlot'
                            else
                                PlotType := 'MatrixPlot'
                            ;
                        'P':
                            if CompareTextShortest('pro', Param) = 0 then
                                PlotType := 'Profile'
                            else
                            begin
                                if CompareTextShortest('phas', Param) = 0 then
                                    PlotType := 'PhaseVoltage'
                                else
                                    PlotType := 'PriceShape';
                            end;
                        'S':
                            PlotType := 'ScatterPlot'
                            ;
                        'T':
                            PlotType := 'TShape'
                            ;
                        'D':
                        begin
                            PlotType := 'DaisyPlot';
                            DaisyBusList.Clear;
                        end;
                        'Z':
                            PlotType := 'MeterZones';
                    else
                    end;
                2:
                    case Param[1] of
                        'V':
                            Quantity := 'Voltage';
                        'C':
                            case Param[2] of
                                'A':
                                    Quantity := 'Capacity';
                                'U':
                                    Quantity := 'Current';
                            end;
                        'P':
                            Quantity := 'Power';
                        'L':
                            if CompareTextShortest('los', Param) = 0 then
                                Quantity := 'Losses'
                            else
                                MatrixType := 'Laplacian'
                            ;
                        'I':
                            MatrixType := 'IncMatrix'
                            ;
                    else
                        Quantity := 'None';
                        Valueindex := Parser.IntValue;
                    end;
                3:
                begin
                    MaxScale := Parser.DblValue;
                    if MaxScale > 0.0 then
                        MaxScaleIsSpecified := TRUE    // Indicate the user wants a particular value
                    else
                        MaxScaleIsSpecified := FALSE;
                end;
                4:
                    Dots := InterpretYesNo(Param);
                5:
                    Labels := InterpretYesNo(Param);
                6:
                    ObjectName := Parser.StrValue;
                7:
                begin
                    ShowLoops := InterpretYesNo(Param);
                    if ShowLoops then
                        PlotType := 'MeterZones';
                end;
                8:
                    TriColorMax := Parser.DblValue;
                9:
                    TriColorMid := Parser.DblValue;
                10:
                    Color1 := InterpretColorName(DSS, Param);
                11:
                    Color2 := InterpretColorName(DSS, Param);
                12:
                    Color3 := InterpretColorName(DSS, Param);
                13:
                begin 
                    // Channel definitions for Plot Monitor
                    NumChannels := Parser.ParseAsVector(51, PDoubleArray(@DblBuffer));  // allow up to 50 channels
                    if NumChannels > 0 then
                    begin   // Else take the defaults
                        SetLength(Channels, NumChannels);
                        for i := 0 to NumChannels - 1 do
                            Channels[i] := Round(DblBuffer[i]);
                        SetLength(Bases, NumChannels);
                        for i := 0 to NumChannels - 1 do
                            Bases[i] := 1.0;
                    end;
                end;
                14:
                begin
                    NumChannels := Parser.ParseAsVector(51, PDoubleArray(@DblBuffer));  // allow up to 50 channels
                    if NumChannels > 0 then
                    begin
                        SetLength(Bases, NumChannels);
                        for i := 0 to NumChannels - 1 do
                            Bases[i] := DblBuffer[i];
                    end;
                end;
                15:
                    ShowSubs := InterpretYesNo(Param);
                16:
                    if Parser.IntValue > 0 then
                        MaxLineThickness := Parser.IntValue;
                17:
                    InterpretTStringListArray(DSS, Param, DaisyBusList); // read in Bus list
                18:
                begin
                    MinScale := Parser.DblValue;
                    MinScaleIsSpecified := TRUE;    // Indicate the user wants a particular value
                end;
                19:
                    ThreePhLineStyle := Parser.IntValue;
                20:
                    SinglePhLineStyle := Parser.IntValue;
                21:
                begin  // Parse off phase(s) to plot
                    PhasesToPlot := PROFILE3PH; // the default
                    if CompareTextShortest(Param, 'default') = 0 then
                        PhasesToPlot := PROFILE3PH
                    else
                    if CompareTextShortest(Param, 'all') = 0 then
                        PhasesToPlot := PROFILEALL
                    else
                    if CompareTextShortest(Param, 'primary') = 0 then
                        PhasesToPlot := PROFILEALLPRI
                    else
                    if CompareTextShortest(Param, 'll3ph') = 0 then
                        PhasesToPlot := PROFILELL
                    else
                    if CompareTextShortest(Param, 'llall') = 0 then
                        PhasesToPlot := PROFILELLALL
                    else
                    if CompareTextShortest(Param, 'llprimary') = 0 then
                        PhasesToPlot := PROFILELLPRI
                    else
                    if Length(Param) = 1 then
                        PhasesToPlot := Parser.IntValue;
                end;
                22:
                begin
                    ProfileScale := PROFILEPUKM;
                    if CompareTextShortest(Param, '120KFT') = 0 then
                        ProfileScale := PROFILE120KFT;
                end;
                23:
                    PlotID := Parser.StrValue;
            else
            end;


        ParamName := Uppercase(Parser.NextParam);
        Param := Uppercase(Parser.StrValue);
    end;

    if not ActiveCircuit.Issolved then
        DSSPlotObj.Quantity := 'None';

    with DSSPlotObj do
    try
        jsonDaisyBusList := TJSONArray.Create();
        for i := 1 to DaisyBusList.Count do
            jsonDaisyBusList.Add(DaisyBusList[i]);

        jsonChannels := TJSONArray.Create();
        for i := 0 to High(Channels) do
            jsonChannels.Add(Channels[i]);

        jsonBases := TJSONArray.Create();
        for i := 0 to High(Bases) do
            jsonBases.Add(Bases[i]);
        
        plotParams := TJSONObject.Create([
            'PlotType', PlotType,
            'MatrixType', MatrixType,
            'MaxScale', MaxScale,
            'MinScale', MinScale,
            'Dots', Dots,
            'Labels', Labels,
            'ShowLoops', ShowLoops,
            'ShowSubs', ShowSubs,
            'Quantity', Quantity,
            'ObjectName', ObjectName,
            'PlotId', PlotID,
            'ValueIndex', ValueIndex,
            'PhasesToPlot', PhasesToPlot,
            'ProfileScale', ProfileScale,
            'Channels', jsonChannels,
            'Bases', jsonBases,
            'SinglePhLineStyle', SinglePhLineStyle,
            'ThreePhLineStyle', ThreePhLineStyle,
            'Color1', ColorToHTML(Color1),
            'Color2', ColorToHTML(Color2),
            'Color3', ColorToHTML(Color3),
            'TriColorMax', TriColorMax,
            'TriColorMid', TriColorMid,
            'MaxScaleIsSpecified', MaxScaleIsSpecified,
            'MinScaleIsSpecified', MinScaleIsSpecified,
            'DaisyBusList', jsonDaisyBusList,
            'DaisySize', DSS.DaisySize,
            'MaxLineThickness', MaxLineThickness,
            'Markers', TJSONObject.Create(
            [
                'NodeMarkerCode', ActiveCircuit.NodeMarkerCode,
                'NodeMarkerWidth', ActiveCircuit.NodeMarkerWidth,
                'SwitchMarkerCode', ActiveCircuit.SwitchMarkerCode,
                'TransMarkerSize', ActiveCircuit.TransMarkerSize,
                'CapMarkerSize', ActiveCircuit.CapMarkerSize,
                'RegMarkerSize', ActiveCircuit.RegMarkerSize,
                'PVMarkerSize', ActiveCircuit.PVMarkerSize,
                'StoreMarkerSize', ActiveCircuit.StoreMarkerSize,
                'FuseMarkerSize', ActiveCircuit.FuseMarkerSize,
                'RecloserMarkerSize', ActiveCircuit.RecloserMarkerSize,
                'RelayMarkerSize', ActiveCircuit.RelayMarkerSize,
                'TransMarkerCode', ActiveCircuit.TransMarkerCode,
                'CapMarkerCode', ActiveCircuit.CapMarkerCode,
                'RegMarkerCode', ActiveCircuit.RegMarkerCode,
                'PVMarkerCode', ActiveCircuit.PVMarkerCode,
                'StoreMarkerCode', ActiveCircuit.StoreMarkerCode,
                'FuseMarkerCode', ActiveCircuit.FuseMarkerCode,
                'RecloserMarkerCode', ActiveCircuit.RecloserMarkerCode,
                'RelayMarkerCode', ActiveCircuit.RelayMarkerCode,
                'MarkSwitches', ActiveCircuit.MarkSwitches,
                'MarkTransformers', ActiveCircuit.MarkTransformers,
                'MarkCapacitors', ActiveCircuit.MarkCapacitors,
                'MarkRegulators', ActiveCircuit.MarkRegulators,
                'MarkPVSystems', ActiveCircuit.MarkPVSystems,
                'MarkStorage', ActiveCircuit.MarkStorage,
                'MarkFuses', ActiveCircuit.MarkFuses,
                'MarkReclosers', ActiveCircuit.MarkReclosers,
                'MarkRelays', ActiveCircuit.MarkRelays
            ])
        ]);
        // plotParams.CompressedJSON := True;
        plotParamsStr := plotParams.FormatJSON();
        DSS.DSSPlotCallback(DSS, PChar(plotParamsStr));
    finally
        FreeAndNil(plotParams);
        FreeAndNil(DSSPlotObj);
    end;
end;


procedure DisposeStrings;
var
    i: Integer;
begin
    for i := 1 to NumPlotOptions do
        PlotOption[i] := '';
end;


initialization
    DefineOptions;
    PlotCommands := TCommandList.Create(PlotOption, True);

finalization
    DisposeStrings;
    PlotCommands.Free;
end.
