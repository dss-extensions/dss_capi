unit GISCommands;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Command,
    Tlhelp32,
    DSSGlobals,
    Windows,
    SysUtils,
    System.Classes,
    ShellApi,
    djson,
    VCl.forms,
    Line,
    Utilities,
    ArrayDef,
    DSSForms,
    ExecHelper,
//   TCP Indy libraries
    IdBaseComponent,
    IdComponent,
    IdTCPConnection,
    IdTCPClient,
    IdThreadComponent,
    TCP_IP;

const
    NumGISOptions = 34;

function DoGISCmd: String;

function start_openDSSGIS(): Boolean;
function show_busGIS(BusName: String): String;
function Get_routeGIS(): String;
function Get_edgesGIS(): String;
function Get_distanceGIS(): String;
function Show_routeGIS(): String;
function Get_JSONrouteGIS(): String;
function WindowLR(): String;
function WindowRL(): String;
function ReSizeWindow(): String;
function GISDrawCircuit(): String;
function show_lineGIS(LineName: String): String;
function export_mapGIS(): String;
function find_treesGIS(LineName: String): String;
procedure get_line_Coords(LineName: String);
function set_map_View(myView: String): String;
function clear_map(): String;
function Draw_line_GIS(): String;
function Zoom_area_GIS(): String;
function GISPlotfile(myPath: String): String;
function show_LatLong(): String;
function GISPlotPoints(myPath: String): String;
function GISPlotPoint(const myShape: String): String;
function GISLoadBus(const myBus: String): String;
function GISShowBuffer(): String;
function GISFormat(const FormatFrom, FormatTo, Coords: String): String;
function GISBatchFormat(const FormatFrom, FormatTo, mypath: String): String;
function GISClose(): String;
function Get_distance(): String;
function GISStartSelect(): String;
function GISStopSelect(): String;
function GISGetSelect(): String;
function GISStartDrawLine(): String;
function GISStopDrawLine(): String;
function GISGetPolyline(): String;

var
    GISTCPClient: TIdTCPClient;  // ... TIdThreadComponent
    GISThreadComponent: TIdThreadComponent;
    myCoords: array of Double;
    GISOption,
    GISHelp: array[1..NumGISOptions] of String;
    GISCommandList: TCommandList;

implementation

procedure DefineOptions;

begin


    GISOption[1] := 'Start';
    GISOption[2] := 'ShowBus';
    GISOption[3] := 'FindRoute';
    GISOption[4] := 'GetRoute';
    GISOption[5] := 'RouteDistance';
    GISOption[6] := 'ShowRoute';
    GISOption[7] := 'JSONRoute';
    GISOption[8] := 'WindowDistribLR';
    GISOption[9] := 'WindowDistribRL';
    GISOption[10] := 'WindowSize';
    GISOption[11] := 'PlotCircuit';
    GISOption[12] := 'showLine';
    GISOption[13] := 'ExportMap';
    GISOption[14] := 'FindTrees';
    GISOption[15] := 'MapView';
    GISOption[16] := 'ClearMap';
    GISOption[17] := 'DrawLine';
    GISOption[18] := 'ZoomMap';
    GISOption[19] := 'PlotFile';
    GISOption[20] := 'GoTo';
    GISOption[21] := 'PlotPoints';
    GISOption[22] := 'PlotPoint';
    GISOption[23] := 'LoadBus';
    GISOption[24] := 'ShowCoords';
    GISOption[25] := 'Format';
    GISOption[26] := 'BatchFormat';
    GISOption[27] := 'Close';
    GISOption[28] := 'Distance';
    GISOption[29] := 'Select';
    GISOption[30] := 'StopSelect';
    GISOption[31] := 'GetSelect';
    GISOption[32] := 'DrawLines';
    GISOption[33] := 'StopDraw';
    GISOption[34] := 'GetPolyline';


    GISHelp[1] := 'Starts OpenDSS-GIS only if it is installed in the local machine';
    GISHelp[2] := 'Shows the bus specified on the map, however, the following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[3] := 'Finds a route between the given buses using roads and geographical information. The buses are described as an array' +
        ' as follows: GISFindRoute [b1 b2], do not include phases. The following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[4] := 'Returns the GIS coords of the route between 2 buses step by step, however, the following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. GISFindRoute has been executed at some point before this command (at least once)' + CRLF +
        '4. The model needs to have the correct GISCoords file';
    GISHelp[5] := 'Returns the distance (value units) of the last route calculated between 2 buses, however, the following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. GISFindRoute has been executed at some point before this command (at least once)' + CRLF +
        '4. The model needs to have the correct GISCoords file';
    GISHelp[6] := 'Shows the last route calculated between 2 buses in OpenDSS-GIS, however, the following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. GISFindRoute has been executed at some point before this command (at least once)' + CRLF +
        '4. The model needs to have the correct GISCoords file';
    GISHelp[7] := 'Returns the JSON script describing the last route calculated between 2 buses, however, the following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. GISFindRoute has been executed at some point before this command (at least once)' + CRLF +
        '4. The model needs to have the correct GISCoords file';
    GISHelp[8] := 'Redistributes the windows horizontally leaving OpenDSS to the left of the screen and OpenDSS-GIS to the right, however, the following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)';
    GISHelp[9] := 'Redistributes the windows horizontally leaving OpenDSS to the right of the screen and OpenDSS-GIS to the left, however, the following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)';
    GISHelp[10] := 'Resizes the OpenDSS-GIS window, the coordiantes need to be given as: Left, Top, Right, Bottom. For example:' + CRLF + CRLF +
        'GISWindowSize 0 0 800 800' + CRLF + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)';
    GISHelp[11] := 'Draws the circuit on top of the map displayed in OpenDSS-GIS. The following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[12] := 'Shows the line specified int he argument using OpenDSS-GIS. The following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[13] := 'Exports the current map view into the models folder as a PNG file. The following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[14] := 'Returns Tree/No tree if a tree intersects with the line given in the argument. The following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[15] := 'Chenges the map view in OpenDSS-GIS using one of the following arguments:' + CRLF + CRLF +
        '- Streets' + CRLF +
        '- StreetsVector' + CRLF +
        '- StreetsNight' + CRLF +
        '- Satellite' + CRLF +
        '- SatelliteLabels' + CRLF +
        '- SatelliteLabelsVector' + CRLF +
        '- DarkGrayCanvas' + CRLF +
        '- LightGrayCanvas' + CRLF +
        '- LightGrayCanvasVector' + CRLF +
        '- Navigation' + CRLF +
        '- OpenStreetMap' + CRLF + CRLF +
        'The following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[16] := 'Clears the Map by removing all the previous draws. The following conditions need to be fulfilled:' + CRLF +
        CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[17] := 'Draws a line at the given coordinates using the color and thickness (pix) specified.' + CRLF +
        'The line features can be defined using GISCoords, GISColor and GISThickness from the exective options.' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[18] := 'Zooms the map at the area specified at GISCoords.' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[19] := 'Plots the content of the file specified in the argument on top of the current map.' + CRLF +
        'With this function it is expected that the content of the file describes lines, their color and thickness.' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[20] := 'Shows the location  in the map at given the coordinates.' +
        'The coordiantes must be defined using GISCoords.' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)';
    GISHelp[21] := 'Plots the content of the file specified in the argument on top of the current map.' + CRLF +
        'This function plots the content as points, the point shape, color and size must be specified in the file.' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[22] := 'plots the shape specified in the argument in the map at given the coordinates.' +
        'The coordiantes must be defined using GISCoords, the size and color can be specified through the options GISColor and GISThickness.' + CRLF +
        'The shape can be one fo the following:' + CRLF + CRLF +
        '  Circle' + CRLF +
        '  + ' + CRLF +
        '  Diamond' + CRLF +
        '  Square' + CRLF +
        '  Triangle' + CRLF +
        '  x' + CRLF + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)';
    GISHelp[23] := 'Uploads the coordinates of the bus specified in the argument to the coordinates buffer by pushing the previous down.' +
        ' The coordinates buffer has 4 positions, the coordinates of the bus specified will be at the first 2 positions.' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[24] := 'Returns the content of the coordinates buffer.' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[25] := 'Formats the coordinates located at the first 2 places of the coordiantes buffer. The first argument indicates ' +
        'the original format and the second argument the destination format. The format can be one of the following:' + CRLF + CRLF +
        '- LatLong (latitude, Longitude - WGS84))' + CRLF +
        '- DMS (Degrees, minutes, seconds) ' + CRLF +
        '- UTM (Universal Transverse Mercator)' + CRLF +
        '- USNG' + CRLF + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[26] := 'Formats the coordinates within the file specified. The first argument indicates ' +
        'the original format and the second argument the destination format. The third argument is the path to the source file' +
        ' containing the coordinates, which should be organized in 2 columns comma separated. The format can be one of the following:' + CRLF + CRLF +
        '- LatLong (latitude, Longitude - WGS84))' + CRLF +
        '- DMS (Degrees, minutes, seconds) ' + CRLF +
        '- UTM (Universal Transverse Mercator)' + CRLF +
        '- USNG' + CRLF + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[27] := 'Closses all the instances of OpenDSS-GIS';
    GISHelp[28] := 'Returns the distance in meters between the coordinates in the buffer.' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[29] := 'Commands OpenDSS-GIS to start the selection mode for allowing users to draw an area on the map.' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[30] := 'Stops the latest select command sent to OpenDSS-GIS. Clears the map from selections and stores the selection coords in OpenDSS-GIS.' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[31] := 'Requests the boundaries of the latest selection. The boundaties are returned as XMin, YMin, XMax and YMax in WGS84 coords format.' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[32] := 'Commands OpenDSS-GIS to start line drawing mode for allowing the user to draw a polyline over the map.' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[33] := 'Stops the latest lien drawing mode in OpenDSS-GIS. Clears the map and stores the coordinates of the polyline drawn by the user (if any).' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';
    GISHelp[34] := 'Requests the coordinates of the latest polyline drawn by the user to OpenDSS-GIS. The are returned in coordiante pairs (Longitude, latitude) in WGS84 coords format.' + CRLF +
        'The following conditions need to be fulfilled:' + CRLF + CRLF +
        '1. OpenDSS-GIS must be installed' + CRLF +
        '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
        '3. The model needs to have the correct GISCoords file';

end;

function DoGISCmd: String;
var

    formatFrom,
    formatto,
    ParamName,
    Param: String;
    ParamPointer,
    i: Integer;
    DblBuffer: array[0..50] of Double;

begin

  {Get next parameter on command line}
    Result := 'Unknown GIS command;';
    ParamPointer := 0;
    ParamName := Uppercase(Parser[ActiveActor].NextParam);
    Param := Uppercase(Parser[ActiveActor].StrValue);
  {Interpret Parameter}
    if (Length(Param) <> 0) then
    begin
        ParamPointer := GISCommandList.Getcommand(Param);

    {Check options requiring a solution and abort if no solution or circuit}
        case ParamPointer of
            1:
                if start_openDSSGIS() then
                    Result := 'GIS Started succesfully'
                else
                    Result := 'Error, check if OpenDSS-GIS is running and your firewall setup';
            2:
            begin
                Parser[ActiveActor].NextParam;
                Result := show_busGIS(Parser[ActiveActor].StrValue);
            end;
            3:
                Result := Get_routeGIS();
            4:
                Result := Get_edgesGIS();
            5:
                Result := Get_distanceGIS();
            6:
                Result := Show_routeGIS();
            7:
                Result := Get_JSONrouteGIS();
            8:
            begin
                if not isDLL then
                begin
                    Result := WindowLR();
                    ControlPanel.ResizeWindow(0);
                end
                else
                    Result := 'Available only for the EXE interface'
            end;
            9:
            begin
                if not IsDLL then
                begin
                    Result := WindowRL();
                    ControlPanel.ResizeWindow(1);
                end
                else
                    Result := 'Available only for the EXE interface'
            end;
            10:
            begin
                Result := ReSizeWindow();
            end;
            11:
                Result := GISDrawCircuit;           // Draws the circuit on top of the map in DSS-GIS
            12:
            begin
                Parser[ActiveActor].NextParam;
                Result := show_lineGIS(Parser[ActiveActor].StrValue);
            end;
            13:
                Result := export_mapGIS();           // exports the current map view into the model's folder
            14:
            begin
                Parser[ActiveActor].NextParam;
                Result := find_treesGIS(Parser[ActiveActor].StrValue);
            end;
            15:
            begin
                Parser[ActiveActor].NextParam;
                Result := set_map_View(lowercase(Parser[ActiveActor].StrValue));
            end;
            16:
                Result := clear_map();
            17:
                Result := Draw_line_GIS();
            18:
                Result := Zoom_area_GIS();
            19:
            begin
                Parser[ActiveActor].NextParam;
                Result := GISPlotfile(lowercase(Parser[ActiveActor].StrValue));
            end;
            20:
            begin
                Result := show_LatLong();
            end;
            21:
            begin
                Parser[ActiveActor].NextParam;
                Result := GISPlotPoints(lowercase(Parser[ActiveActor].StrValue));
            end;
            22:
            begin
                Parser[ActiveActor].NextParam;
                Result := GISPlotPoint(lowercase(Parser[ActiveActor].StrValue));
            end;
            23:
            begin
                Parser[ActiveActor].NextParam;
                Result := GISLoadBus(lowercase(Parser[ActiveActor].StrValue));
            end;
            24:
            begin
                Result := GISShowBuffer();
            end;
            25:
            begin
                Parser[ActiveActor].NextParam;
                FormatFrom := lowercase(Parser[ActiveActor].StrValue);
                Parser[ActiveActor].NextParam;
                FormatTo := lowercase(Parser[ActiveActor].StrValue);
                Parser[ActiveActor].NextParam;
                Result := GISFormat(FormatFrom, FormatTo, Parser[ActiveActor].StrValue);
            end;
            26:
            begin
                Parser[ActiveActor].NextParam;
                FormatFrom := lowercase(Parser[ActiveActor].StrValue);
                Parser[ActiveActor].NextParam;
                FormatTo := lowercase(Parser[ActiveActor].StrValue);
                Parser[ActiveActor].NextParam;
                Result := GISBatchFormat(FormatFrom, FormatTo, Parser[ActiveActor].StrValue);
            end;
            27:
                Result := GISClose();
            28:
                Result := Get_distance();
            29:
                Result := GISStartSelect();
            30:
                Result := GISStopSelect();
            31:
                Result := GISGetSelect();
            32:
                Result := GISStartDrawLine();
            33:
                Result := GISStopDrawLine();
            34:
                Result := GISGetPolyline();
        else
        end;
    end;
end;

{*******************************************************************************
*             Starts openDSS-GIS and gets connected as client                  *
*******************************************************************************}

function start_openDSSGIS(): Boolean;
var
    myPath,
    myFolder: String;
begin
    Result := FALSE;

    if DSS_GIS_Installed then
    begin
        myPath := StringReplace(DSS_GIS_path, '\\', '\', [rfReplaceAll, rfIgnoreCase]);
        myPath := StringReplace(myPath, '"', '', [rfReplaceAll, rfIgnoreCase]);
        myFolder := ExtractFilePath(myPath);

        if not processExists('OpenDSSGIS.exe') then
        begin
     // Starts OpenDSS-GIS if is not running
            ShellExecute(0, 'open', Pchar(myPath), NIL, Pchar(myFolder), SW_SHOWNORMAL);
            sleep(5000);
            IsGISON := FALSE;
        end;
        if not IsGISON then
        begin
      // ... create TIdTCPClient
            GISTCPClient := TIdTCPClient.Create();
      // ... set properties
            GISTCPClient.Host := 'localhost';
            GISTCPClient.Port := DSSGISPort;
            GISTCPClient.ReadTimeout := 1000;
            GISThreadComponent := TIdThreadComponent.Create();
            try
                GISTCPClient.Connect;
                IsGISON := TRUE;
            except
                on E: Exception do
                begin
                    IsGISON := FALSE;
                end;
            end;
            Result := IsGISON;
        end
        else
            Result := IsGISON;
    end;

end;

{*******************************************************************************
*                            Closes OpenDSS-GIS                                *
*******************************************************************************}
function GISClose(): String;
var
    myError: Integer;
begin
    myError := ShellExecute(0, NIL, 'taskkill.exe', '/IM "OpenDSSGIS.exe" /F', 'C:\Windows\System32', SW_HIDE);
    if myError > 32 then
        Result := 'OpenDSS-GIS closed successfuly'
    else
        Result := 'An error has occurred while closing OpenDSS-GIS';
    IsGISON := FALSE;
end;

{*******************************************************************************
*                         Shows the given bus on the map                       *
*******************************************************************************}
function show_busGIS(BusName: String): String;
var
    TCPJSON: TdJSON;
    i: Integer;
    lat,
    long: Double;
    InMsg: String;
begin
    if IsGISON then
    begin
        SetActiveBus(BusName);
        if (ActiveCircuit[ActiveActor] <> NIL) then
        begin
            with ActiveCircuit[ActiveActor] do
            begin
                if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].GISCoorddefined) then
                    begin
                        lat := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
                        long := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
                        InMsg := '{"command":"showlocation","coords":{"longitude":' + floattostr(long) + ',"latitude":' + floattostr(lat) + '}}';
                        try
                            GISTCPClient.IOHandler.WriteLn(InMsg);
                            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
                            TCPJSON := TdJSON.Parse(InMsg);
                            Result := TCPJSON['showlocation'].AsString;
                        except
                            on E: Exception do
                            begin
                                IsGISON := FALSE;
                                Result := 'Error while communicating to OpenDSS-GIS';
                            end;
                        end;
                    end
                    else
                        Result := 'One or both of the GIS coordinates are incorrect or not defined';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';

end;

{*******************************************************************************
*                  Shows the given location using LatLong                      *
*******************************************************************************}
function show_LatLong(): String;
var
    TCPJSON: TdJSON;
    i: Integer;
    lat,
    long: Double;
    InMsg: String;
begin
    if IsGISON then
    begin

        lat := GISCoords^[1];
        long := GISCoords^[2];
        InMsg := '{"command":"showlocation","coords":{"longitude":' + floattostr(long) + ',"latitude":' + floattostr(lat) + '}}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['showlocation'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;

    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';

end;

{*******************************************************************************
*                 Request to calculate a route between 2 buses                 *
*******************************************************************************}

function Get_routeGIS(): String;
var
    TCPJSON: TdJSON;
    JSONCmd,
    InMsg,
    busName: String;
    TryCom,
    error: Boolean;
    i: Integer;
    lat,
    long: Double;
begin
    if IsGISON then
    begin
        error := FALSE;
        JSONCmd := '{"command":"route","coords":[';
        for i := 1 to 2 do                                                  // to extract both buses
        begin
            Parser[ActiveActor].NextParam;
            busName := Parser[ActiveActor].StrValue;
            SetActiveBus(busName);
            if (ActiveCircuit[ActiveActor] <> NIL) and not error then         // is everything fine?
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].GISCoorddefined) then
                        begin
                            lat := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
                            long := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
                            JSONCmd := JSONCmd + '{"longitude":' + floattostr(long) + ',"latitude":' + floattostr(lat) + '},';
                        end
                        else
                            error := TRUE;
                end;
            end;
        end;
        if not error then                                                 // No error so far
        begin
            JSONCmd := JSONCmd.Substring(0, length(JSONCmd) - 1) + ']}';
            TryCom := TRUE;
            i := 0;
            while TryCom do
            begin
                try
                    GISTCPClient.IOHandler.WriteLn(JSONCmd);
                    InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
                    TCPJSON := TdJSON.Parse(InMsg);
                    InMsg := TCPJSON['route'].AsString;
                    if InMsg = 'done' then                                      // Route calculated successfully
                        Trycom := FALSE
                    else
                    begin
            // If the route wasn't calculated because the server was busy, it tries up to 5 times
            // with 300 ms interval, if after that the server is still busy, return error message
                        sleep(300);
                        inc(i);
                        if i > 5 then
                            Trycom := FALSE;
                    end;
                except
                    on E: Exception do
                    begin
                        IsGISON := FALSE;
                        Trycom := FALSE;
                        Result := 'Error while communicating to OpenDSS-GIS';
                    end;
                end;
            end;
            Result := InMsg;
        end
        else
            Result := 'One or more buses have no GIS coordinates';
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'
end;

{*******************************************************************************
*  Request to coordiantes of the edges that define the last route calculated   *
*******************************************************************************}
function Get_edgesGIS(): String;
var
    Coords,
    TCPJSON: TdJSON;
    JSONCmd,
    TempStr,
    InMsg: String;

begin
    if IsGISON then
    begin
        JSONCmd := '{"command":"jsonroute"}';
        try
            GISTCPClient.IOHandler.WriteLn(JSONCmd);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 2000);
            TCPJSON := TdJSON.Parse(InMsg);
            TempStr := '[';
            for Coords in TCPJSON['jsonroute'] do
            begin
                TempStr := TempStr + Coords['latitude'].AsString + ',' + Coords['longitude'].AsString + ',';
            end;
            Result := TempStr.substring(0, (length(TempStr) - 1)) + ']';
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'
end;

{*******************************************************************************
*                Gets the distance of the last route calculated                *
*******************************************************************************}
function Get_distanceGIS(): String;
var
    TCPJSON: TdJSON;
    JSONCmd,
    TempStr,
    InMsg: String;

begin
    if IsGISON then
    begin
        JSONCmd := '{"command":"routedistance"}';
        try
            GISTCPClient.IOHandler.WriteLn(JSONCmd);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 2000);
            TCPJSON := TdJSON.Parse(InMsg);
            TempStr := TCPJSON['routedistance'].AsString + ' ' + TCPJSON['units'].AsString;
            Result := TempStr;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;

    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'
end;

{*******************************************************************************
*                 Shows on the map the last route calculated                   *
*******************************************************************************}

function Show_routeGIS(): String;
var
    TCPJSON: TdJSON;
    JSONCmd,
    TempStr,
    InMsg: String;

begin
    if IsGISON then
    begin
        JSONCmd := '{"command":"showroute"}';
        try
            GISTCPClient.IOHandler.WriteLn(JSONCmd);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 2000);
            TCPJSON := TdJSON.Parse(InMsg);
            TempStr := TCPJSON['showroute'].AsString;
            Result := TempStr;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;

    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'

end;

{*******************************************************************************
*       Exports to a file the last route calculated in JSON format             *
*******************************************************************************}

function Get_JSONrouteGIS(): String;
var
    F: TextFile;
    JSONCmd,
    FileName,
    InMsg: String;
begin
    if IsGISON then
    begin
        JSONCmd := '{"command":"jsonscript"}';
        try
            GISTCPClient.IOHandler.WriteLn(JSONCmd);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 20000);

            FileName := GetOutputDirectory + CircuitName_[ActiveActor] + 'JSONScript_route.txt';  // Explicitly define directory

            Assignfile(F, FileName);
            ReWrite(F);
            Write(F, inMsg);
            CloseFile(F);

            Result := FileName;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;

    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'

end;

{*******************************************************************************
*            Distributes the windows leaving OpenDSS on the left               *
*******************************************************************************}

function WindowLR(): String;
var
    TCPJSON: TdJSON;
    ScrSize: Integer;
    InMsg,
    TempStr,
    JSONCmd: String;
begin
    if IsGISON then
    begin
        JSONCmd := '{"command":"resizewindow","coords":{"left":' + inttostr(Screen.Width div 2) +
            ',"top":0,"right":' + inttostr(Screen.Width) + ',"bottom":' + inttostr(Screen.Height - 40) + '}}';
        try
            GISTCPClient.IOHandler.WriteLn(JSONCmd);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 2000);
            TCPJSON := TdJSON.Parse(InMsg);
            TempStr := TCPJSON['resizewindow'].AsString;
            Result := TempStr;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'

end;

{*******************************************************************************
*            Distributes the windows leaving OpenDSS to the right              *
*******************************************************************************}

function WindowRL(): String;
var
    TCPJSON: TdJSON;
    ScrSize: Integer;
    InMsg,
    TempStr,
    JSONCmd: String;
begin
    if IsGISON then
    begin
        JSONCmd := '{"command":"resizewindow","coords":{"left":0,"top":0,"right":' +
            inttostr(Screen.Width div 2) + ',"bottom":' + inttostr(Screen.Height - 40) + '}}';
        try
            GISTCPClient.IOHandler.WriteLn(JSONCmd);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 2000);
            TCPJSON := TdJSON.Parse(InMsg);
            TempStr := TCPJSON['resizewindow'].AsString;
            Result := TempStr;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'
end;

{*******************************************************************************
*    Resizes the OpenDSS-GIS window using the coordinates given by the user    *
*******************************************************************************}

function ReSizeWindow(): String;
var
    TCPJSON: TdJSON;
    j,
    ScrSize: Integer;
    InMsg,
    TempStr,
    JSONCmd: String;
    TStrArr: array of String;

begin
    if IsGISON then
    begin
        setlength(TStrArr, 4);
        TStrArr[0] := ',"top":';
        TStrArr[1] := ',"right":';
        TStrArr[2] := ',"bottom":';
        TStrArr[3] := '}}';

        JSONCmd := '{"command":"resizewindow","coords":{"left":';
        for j := 0 to High(TStrArr) do
        begin
            Parser[ActiveActor].NextParam;
            JSONCmd := JSONCmd + Parser[ActiveActor].StrValue + TStrArr[j];
        end;
        try
            GISTCPClient.IOHandler.WriteLn(JSONCmd);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 2000);
            TCPJSON := TdJSON.Parse(InMsg);
            TempStr := TCPJSON['resizewindow'].AsString;
            Result := TempStr;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'

end;

{*******************************************************************************
*      Generates the file required by DSS-GIS to draw the model on the map     *
*******************************************************************************}

function GISDrawCircuit(): String;
var
    LineElem: TLineObj;
    TxtRow,
    myBus: String;
    k: Integer;
    F: TextFile;
    InMsg,
    TempStr,
    JSONCmd: String;
    TCPJSON: TdJSON;
    Add2file: Boolean;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if IsGISON then
        begin
            with ActiveCircuit[ActiveActor] do
            begin
                if Lines.ListSize > 0 then
                begin
                    Assignfile(F, 'GIS_desc.csv');
                    ReWrite(F);
                    LineElem := Lines.First;
                    while LineElem <> NIL do
                    begin
                        TxtRow := '';
                        Add2File := TRUE;
                        for k := 1 to 2 do
                        begin
                            myBus := StripExtension(LineElem.GetBus(k));
                            DSSGlobals.SetActiveBus(myBus);
                            if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].GISCoordDefined) then
                            begin
                                TxtRow := TxtRow + floattostr(Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Long) +
                                    ',' + floattostr(Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Lat) + ',';
                            end;
                            Add2File := Add2File and (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Long <> 0) and (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Lat <> 0);
                        end;
                        if Add2File then
                            Writeln(F, TxtRow);
                        LineElem := Lines.Next;

                    end;
                    CloseFile(F);
                    JSONCmd := '{"command":"plotcircuit","path":"' +
                        OutputDirectory[ActiveActor] + 'GIS_desc.csv","color":"' + GISColor +
                        '","thickness":' + GISThickness + '}';
            // Sends the command to OpenDSS-GIS
                    try
                        GISTCPClient.IOHandler.WriteLn(JSONCmd);
                        InMsg := GISTCPClient.IOHandler.ReadLn(#10, 5000);
                        TCPJSON := TdJSON.Parse(InMsg);
                        TempStr := TCPJSON['plotcircuit'].AsString;
                        Result := TempStr;
                    except
                        on E: Exception do
                        begin
                            IsGISON := FALSE;
                            Result := 'Error while communicating to OpenDSS-GIS';
                        end;
                    end;

                end;
            end;
        end
        else
            result := 'OpenDSS-GIS is not installed or initialized'
    end;
end;

{*******************************************************************************
*                         Shows the given line on the map                       *
*******************************************************************************}
function show_lineGIS(LineName: String): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
  // First have to find the line

        if (ActiveCircuit[ActiveActor] <> NIL) then
        begin
            get_line_Coords(LineName);

            InMsg := '{"command":"showline","coords":{"long1":' + floattostr(myCoords[0]) + ',"lat1":' + floattostr(myCoords[1]) +
                ',"long2":' + floattostr(myCoords[2]) + ',"lat2":' + floattostr(myCoords[3]) + '}}';
            try
                GISTCPClient.IOHandler.WriteLn(InMsg);
                InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
                TCPJSON := TdJSON.Parse(InMsg);
                Result := TCPJSON['showline'].AsString;
            except
                on E: Exception do
                begin
                    IsGISON := FALSE;
                    Result := 'Error while communicating to OpenDSS-GIS';
                end;
            end;

        end;

    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';

end;

{*******************************************************************************
*             Exports the current map view into the models folder              *
*******************************************************************************}
function export_mapGIS(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"exportmap", "path":"' + OutputDirectory[ActiveActor] + '"}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['exportmap'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;

    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*             Commands OpenDSS-GIS to verify if there are trees                *
*                     intersecting with the given line                         *
*******************************************************************************}
function find_treesGIS(LineName: String): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
  // to be implemented

        if (ActiveCircuit[ActiveActor] <> NIL) then
        begin
            get_line_Coords(LineName);

            InMsg := '{"command":"findtrees","coords":{"long1":' + floattostr(myCoords[0]) + ',"lat1":' + floattostr(myCoords[1]) +
                ',"long2":' + floattostr(myCoords[2]) + ',"lat2":' + floattostr(myCoords[3]) + '}}';

            try
                GISTCPClient.IOHandler.WriteLn(InMsg);
                InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
                TCPJSON := TdJSON.Parse(InMsg);
                Result := TCPJSON['findtrees'].AsString;
            except
                on E: Exception do
                begin
                    IsGISON := FALSE;
                    Result := 'Error while communicating to OpenDSS-GIS';
                end;
            end;
        end;
        result := 'No';
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*             Commands OpenDSS-GIS to update the map view to the               *
*                             one given by the user                            *
*******************************************************************************}
function set_map_View(myView: String): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"mapview","mymap":"' + myView + '"}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['mapview'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*      Commands OpenDSS-GIS to remove all previous lines/draws from the map    *
*******************************************************************************}
function clear_map(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"clearmap"}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['clearmap'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*                 Draws a line in the map at the given coordinates             *
*******************************************************************************}
function Draw_line_GIS(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;

begin
    if IsGISON then
    begin

        InMsg := '{"command":"drawline","coords":{"long1":' + floattostr(GISCoords^[1]) + ',"lat1":' + floattostr(GISCoords^[2]) +
            ',"long2":' + floattostr(GISCoords^[3]) + ',"lat2":' + floattostr(GISCoords^[4]) + '},"color":"' + GISColor +
            '","thickness":' + GISThickness + '}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['drawline'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*          Zooms the map at the area described by the given coordinates        *
*******************************************************************************}
function Zoom_area_GIS(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"zoommap","coords":{"long1":' + floattostr(GISCoords^[1]) + ',"lat1":' + floattostr(GISCoords^[2]) +
            ',"long2":' + floattostr(GISCoords^[3]) + ',"lat2":' + floattostr(GISCoords^[4]) + '}}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['zoommap'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*            request the calculation of the distance between 2 points          *
*******************************************************************************}
function Get_distance(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"distance","coords":{"long1":' + floattostr(GISCoords^[1]) + ',"lat1":' + floattostr(GISCoords^[2]) +
            ',"long2":' + floattostr(GISCoords^[3]) + ',"lat2":' + floattostr(GISCoords^[4]) + '}}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['distance'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*                 Commands OpenDSS-GIS to start select mode                    *
*******************************************************************************}
function GISStartSelect(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"select"}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['select'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*              Commands OpenDSS-GIS to start line drawing mode                 *
*******************************************************************************}
function GISStartDrawLine(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"drawlines"}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['drawlines'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*            Commands OpenDSS-GIS to stop the line drawing mode                *
*******************************************************************************}
function GISStopDrawLine(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"stopdrawlines"}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['stopdrawlines'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*                  Commands OpenDSS-GIS to stop select mode                    *
*******************************************************************************}
function GISStopSelect(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"stopselect"}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['stopselect'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*      gets the boundaries for the latest selection made in OpenDSS-GIS        *
*******************************************************************************}
function GISGetSelect(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"getselect"}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['getselect'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*          gets the coords for the latest polyline drawn in OpenDSS-GIS        *
*******************************************************************************}
function GISGetPolyline(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"getpolyline"}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['getpolyline'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*       Commands OpenDSS-GIS to draw the content of a file over the map        *
*******************************************************************************}
function GISPlotfile(myPath: String): String;
var
    TxtRow,
    myBus: String;
    k: Integer;
    F: TextFile;
    InMsg,
    TempStr,
    JSONCmd: String;
    TCPJSON: TdJSON;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if IsGISON then
        begin
            JSONCmd := '{"command":"plotfromfile","path":"' +
                myPath + '"}';
      // Sends the command to OpenDSS-GIS
            try
                GISTCPClient.IOHandler.WriteLn(JSONCmd);
                InMsg := GISTCPClient.IOHandler.ReadLn(#10, 5000);
                TCPJSON := TdJSON.Parse(InMsg);
                TempStr := TCPJSON['plotfromfile'].AsString;
                Result := TempStr;
            except
                on E: Exception do
                begin
                    IsGISON := FALSE;
                    Result := 'Error while communicating to OpenDSS-GIS';
                end;
            end;
        end
        else
            result := 'OpenDSS-GIS is not installed or initialized'
    end;
end;

{*******************************************************************************
*     Commands OpenDSS-GIS to draw the points within a file over the map       *
*******************************************************************************}
function GISPlotPoints(myPath: String): String;
var
    TxtRow,
    myBus: String;
    k: Integer;
    F: TextFile;
    InMsg,
    TempStr,
    JSONCmd: String;
    TCPJSON: TdJSON;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if IsGISON then
        begin
            JSONCmd := '{"command":"plotpoints","path":"' +
                myPath + '"}';
      // Sends the command to OpenDSS-GIS
            try
                GISTCPClient.IOHandler.WriteLn(JSONCmd);
                InMsg := GISTCPClient.IOHandler.ReadLn(#10, 5000);
                TCPJSON := TdJSON.Parse(InMsg);
                TempStr := TCPJSON['plotpoints'].AsString;
                Result := TempStr;
            except
                on E: Exception do
                begin
                    IsGISON := FALSE;
                    Result := 'Error while communicating to OpenDSS-GIS';
                end;
            end;
        end
        else
            result := 'OpenDSS-GIS is not installed or initialized'
    end;
end;

{*******************************************************************************
*         Commands OpenDSS-GIS to draw a marker at specific coordinates        *
*******************************************************************************}
function GISPlotPoint(const myShape: String): String;
var
    TCPJSON: TdJSON;
    myShpCode,
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;

begin
    if IsGISON then
    begin
        case myShape[1] of            // Parse the shape specified
            'c':
                myShpCode := 0;
            '+':
                myShpCode := 1;
            'd':
                myShpCode := 2;
            's':
                myShpCode := 3;
            't':
                myShpCode := 4;
            'x':
                myShpCode := 5;
        end;
        InMsg := '{"command":"plotpoint","coords":{"long":' + floattostr(GISCoords^[1]) + ',"lat":' + floattostr(GISCoords^[2]) +
            '},"color":"' + GISColor +
            '","thickness":' + GISThickness + ',"shape":' + inttostr(myShpCode) + '}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 1000);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['plotpoint'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := FALSE;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
* Commands openDSS-GIS to convert the coords given in a file into a new format *
*******************************************************************************}
function GISBatchFormat(const FormatFrom, FormatTo, mypath: String): String;
var
    myEnd,
    myStart: Integer;
    TCPJSON: TdJSON;
    InMsg: String;
begin
    InMsg := '{"command":"batchformat","from":"' + FormatFrom + '","to":"' + FormatTo + '",' +
        '"path":"' + mypath + '"}';
    try
        GISTCPClient.IOHandler.WriteLn(InMsg);
        InMsg := GISTCPClient.IOHandler.ReadLn(#10, 1000);
        myStart := ansipos('path":"', InMsg) + 6;
        myEnd := ansipos('"}', InMsg) - 1;
        Result := InMsg.Substring(myStart, myEnd - myStart);
    except
        on E: Exception do
        begin
            IsGISON := FALSE;
            Result := 'Error while communicating to OpenDSS-GIS';
        end;
    end;
end;

{*******************************************************************************
*      Commands openDSS-GIS to convert the coords given into a new format      *
*******************************************************************************}
function GISFormat(const FormatFrom, FormatTo, Coords: String): String;
var
    TCPJSON: TdJSON;
    InMsg: String;
begin
    InMsg := '{"command":"format","from":"' + FormatFrom + '","to":"' + FormatTo + '",' +
        '"coords":"' + Coords + '"}';
    try
        GISTCPClient.IOHandler.WriteLn(InMsg);
        InMsg := GISTCPClient.IOHandler.ReadLn(#10, 1000);
        TCPJSON := TdJSON.Parse(InMsg);
        Result := TCPJSON['coords'].AsString;
    except
        on E: Exception do
        begin
            IsGISON := FALSE;
            Result := 'Error while communicating to OpenDSS-GIS';
        end;
    end;
end;

{*******************************************************************************
*          Returns a string with the content of the coordiantes buffer         *
*******************************************************************************}
function GISShowBuffer(): String;
var
    idx: Integer;
begin
    Result := '';
    for idx := 1 to 4 do
        Result := Result + floattostr(GISCoords^[idx]) + ',';
end;

{*******************************************************************************
*  Loads the bus coordiantes into the first 2 places fo the coordiantes buffer *
*  shifting it down                                                            *
*******************************************************************************}
function GISLoadBus(const myBus: String): String;
var
    myLat,
    myLong: Double;
begin
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
        begin
            DSSGlobals.SetActiveBus(StripExtension(myBus));
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) then
                begin
                    myLong := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
                    myLat := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
                end;
                GISCoords^[3] := GISCoords^[1];
                GISCoords^[4] := GISCoords^[2];
                GISCoords^[1] := myLong;
                GISCoords^[2] := myLat;
                Result := 'done'
            end
            else
                Result := 'Invalid bus name';
        end
    else
        Result := 'There is no active circuit';
end;

{*******************************************************************************
*             Loads the line Long-lat into the global array "myCoords"         *
*******************************************************************************}
procedure get_line_Coords(LineName: String);
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    myBuses: array of String;
    S,
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    setlength(myCoords, 4);
    setlength(myBuses, 2);

    S := LineName;  // Convert to Pascal String
    Found := FALSE;

    with ActiveCircuit[ActiveActor].Lines do
    begin
        ActiveSave := ActiveIndex;
        pLine := First;
        while pLine <> NIL do
        begin
            if (CompareText(pLine.Name, S) = 0) then
            begin
                ActiveCircuit[ActiveActor].ActiveCktElement := pLine;
                Found := TRUE;
                Break;
            end;
            pLine := Next;
        end;
    end;
  // Get the names of the buses for the line
    with ActiveCircuit[ActiveActor] do
    begin
        for i := 1 to 2 do
        begin
            myBuses[i - 1] := StripExtension(pLine.GetBus(i));
        end;

  // Get the coords of the buses
        for i := 0 to 1 do
        begin
            SetActiveBus(myBuses[i]);
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].GISCoorddefined) then
                begin
                    myCoords[i * 2] := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
                    myCoords[i * 2 + 1] := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
                end;
            end;
        end;
    end;

end;

procedure DisposeStrings;
var
    i: Integer;

begin
    for i := 1 to NumGISOptions do
    begin
        GISOption[i] := '';
        GISHelp[i] := '';
    end;

end;

initialization

    DefineOptions;

    GISCommandList := TCommandList.Create(GISOption);
    GISCommandList.Abbrev := TRUE;

finalization

    DisposeStrings;
    GISCommandList.Free;

end.
