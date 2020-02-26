unit TCP_IP;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2017, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    System.Win.ScktComp,
    SysUtils,
    System.JSON,
    ShellApi,
    TlHelp32,
    Windows,
    Winsock;

type
    IntegerArray1d = array of Integer;
    pIntegerArray1d = ^IntegerArray1d;
    SingleArray = array[1..100] of Single;
    pSingleArray = ^SingleArray;
    DoubleArray1d = array of Double;
    pDoubleArray1d = ^DoubleArray1d;
    DoubleArray2d = array of array of Double;
    pDoubleArray2d = ^DoubleArray2d;
    StringArray1d = array of String;
    pStringArray1d = ^StringArray1d;
    StringArray2d = array of array of String;
    pStringArray2d = ^StringArray2d;

    TDSSConnect = class(TObject)
        procedure MySocketRead(Sender: TObject; Socket: TCustomWinSocket);
        procedure MySocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
        procedure MySocketError(Sender: TObject; Socket: TCustomWinSocket;
            ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    PRIVATE
        myStr: String;
        MySocket: TClientSocket;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;
        procedure Connect;
        procedure Disconnect;
        procedure SetDefaults;
        procedure MonitorPlotMsg(ObjectName: String);
        procedure LoadshapePlotMsg(ObjectName: String);
        procedure ProfilePlotMsg(ObjectName: String);
        procedure ScatterPlotMsg;
        procedure EvolutionPlotMsg;
        procedure MatrixPlotMsg;
    end;

function flat_int2str(number: Integer): Ansistring;
function flatten2JSON(Model: String; Name: String; PlotType: String;
    Xlabel: String; X_axis: pDoubleArray2d; Ylabels: pStringArray1d;
    Y_axis: pDoubleArray2d; Phase: pIntegerArray1d; Z_axis: pDoubleArray2d;
    PD_Elements: pStringArray2d; Bus_names: pStringArray1d): Ansistring;
function StrippedOfNonAscii(const s: String): String;
function processExists(exeFileName: String): Boolean;

var
    DSSConnectObj: TDSSConnect;

implementation

uses
    Monitor,
    Loadshape,
    EnergyMeter,
    CktElement,
    Bus,
    Utilities,
    PDElement,
    Executive,
    ExecHelper,
    Load,
    uComplex,
    DSSGlobals,
    DSSClassDefs,
    Classes,
    Variants,
    Math;

type
    THeaderRec = record
        Signature: Integer;
        Version: Integer;
        RecordSize: Integer;
        Mode: Integer;
        StrBuffer: TMonitorStrBuffer;
    end;

/////////////////////////////// Common functions ///////////////////////////////
procedure ReadMonitorHeader(var HeaderRec: THeaderRec; Opt: Boolean);
var
    pMon: TMonitorObj;
begin
    pMon := ActiveCircuit.Monitors.Active;
    try
        with pmon.MonitorStream, HeaderRec do
        begin
            Seek(0, classes.soFromBeginning);
            Read(signature, Sizeof(signature));// Signature   (32 bit Integer )
            Read(version, Sizeof(version));  // Version     (32 bit Integer )
            Read(RecordSize, Sizeof(RecordSize));//RecordSize  (32 bit Integer )
            Read(Mode, Sizeof(Mode));     // Mode        (32 bit Integer )
            Read(StrBuffer, Sizeof(TMonitorStrBuffer));// String (255 char)
        end;
    finally
          // If opt is false leave monitorstream at end of header record
          // put monitor stream pointer back where it was
        if Opt then
            pmon.MonitorStream.Seek(0, soFromEnd);
    end;
end;

procedure GetMonitorTime(ObjectName: String; time: pDoubleArray2d);
var
    pMon: TMonitorObj;
    activesave: Integer;
    tempS: String;
    Found: Boolean;

    Header: THeaderRec;
    FirstCol: String;
    AllocSize: Integer;
    SngBuffer: pSingleArray;
    i, k: Integer;
    hr: Single;
    s: Single;

begin

  // Monitors.Nme Write
    if ActiveCircuit <> NIL then
    begin      // Search list of monitors in active circuit for name
        with ActiveCircuit.Monitors do
        begin
            tempS := ObjectName;  // Convert to Pascal String
            Found := FALSE;
            ActiveSave := ActiveIndex;
            pMon := First;
            while pMon <> NIL do
            begin
                if (CompareText(pMon.Name, tempS) = 0) then
                begin
                    ActiveCircuit.ActiveCktElement := pMon;
                    Found := TRUE;
                    Break;
                end;
                pMon := Next;
            end;
            if not Found then
            begin
                DoSimpleMsg('Monitor "' + tempS + '" Not Found in Active Circuit.', 5004);
                pMon := Get(ActiveSave);    // Restore active Monerator
                ActiveCircuit.ActiveCktElement := pMon;
            end;
        end;
    end;

  // Monitors.dblSecs
    if ActiveCircuit <> NIL then
    begin
        pMon := ActiveCircuit.Monitors.Active;
        if pMon.SampleCount > 0 then
        begin
            SetLength(time^, 1, pMon.SampleCount);
            ReadMonitorHeader(Header, FALSE);   // leave at beginning of data
            AuxParser.CmdString := String(Header.StrBuffer);
            AuxParser.AutoIncrement := TRUE;
            FirstCol := AuxParser.StrValue;  // Get rid of first two columns
            AuxParser.AutoIncrement := FALSE;
      // check first col to see if it is "Hour"
            if System.Sysutils.CompareText(FirstCol, 'hour') = 0 then
            begin
                AllocSize := Sizeof(SngBuffer^[1]) * Header.RecordSize;
                SngBuffer := Allocmem(AllocSize);
                k := 0;
                for i := 1 to pMon.SampleCount do
                begin
                    with pMon.MonitorStream do
                    begin
                        Read(hr, SizeOf(hr));  // Hour
                        Read(s, SizeOf(s));   // Seconds past the hour
                        Read(sngBuffer^[1], AllocSize);  // read rest of record
                    end;
                    time^[0, k] := hr * 3600 + s;
                    inc(k);
                end;
                Reallocmem(SngBuffer, 0);  // Dispose of buffer
            end
            else
            begin   // Not time solution, so return nil array
                time^ := NIL;
                pMon.MonitorStream.Seek(0, soFromEnd); // leave stream at end
            end;
        end
        else
            time^ := NIL;
    end;
end;

function flat_int2str(number: Integer): Ansistring;
var
    temp_str: array of Byte;
begin
    Setlength(temp_str, 4);
    temp_str[0] := Byte(number shr 24);
    temp_str[1] := Byte(number shr 16);
    temp_str[2] := Byte(number shr 8);
    temp_str[3] := Byte(number);
    SetString(Result, PAnsiChar(temp_str), length(temp_str));
end;

function flatten2JSON(Model: String; Name: String; PlotType: String;
    Xlabel: String; X_axis: pDoubleArray2d; Ylabels: pStringArray1d;
    Y_axis: pDoubleArray2d; Phase: pIntegerArray1d; Z_axis: pDoubleArray2d;
    PD_Elements: pStringArray2d; Bus_names: pStringArray1d): Ansistring;
var
    o: TJSONObject;
    x, x2: TJSONArray;
    y, y2: TJSONArray;
    y_labels: TJSONArray;
    ph: TJSONArray;
    z, z2: TJSONArray;
    pd_el, pd_el2: TJSONArray;
    bus_n: TJSONArray;
    i, j: Integer;
begin
    o := TJSONObject.Create;//Create the outer JSONobject which parents the others
    try
        o.AddPair(TJSONPair.Create('Model', Model));
        o.AddPair(TJSONPair.Create('Name', Name));
        o.AddPair(TJSONPair.Create('Plot Type', PlotType));
        o.AddPair(TJSONPair.Create('X Axis Label', Xlabel));

        x := TJSONArray.Create();
        o.AddPair('X-Axis', x);
        if length(X_axis^) = 0 then
        begin
            x2 := TJSONArray.Create();
            x.AddElement(x2);
        end
        else
        begin
            for i := Low(X_axis^) to High(X_axis^) do
            begin
                x2 := TJSONArray.Create();
                x.AddElement(x2);
                for j := Low(X_axis^[i]) to High(X_axis^[i]) do
                    x2.Add(X_axis^[i, j]);
            end;
        end;

        y_labels := TJSONArray.Create();
        o.AddPair('Plot name', y_labels);
        for i := Low(Ylabels^) to High(Ylabels^) do
            y_labels.Add(Ylabels^[i]);

        y := TJSONArray.Create();
        o.AddPair('Y-Axis', y);
        if length(Y_axis^) = 0 then
        begin
            y2 := TJSONArray.Create();
            y.AddElement(y2);
        end
        else
        begin
            for i := Low(Y_axis^) to High(Y_axis^) do
            begin
                y2 := TJSONArray.Create();
                y.AddElement(y2);
                for j := Low(Y_axis^[i]) to High(Y_axis^[i]) do
                    y2.Add(Y_axis^[i, j]);
            end;
        end;

        ph := TJSONArray.Create();
        o.AddPair('Phase', ph);
        for i := Low(Phase^) to High(Phase^) do
            ph.Add(Phase^[i]);

        z := TJSONArray.Create();
        o.AddPair('Z-Axis', z);
        if length(Z_axis^) = 0 then
        begin
            z2 := TJSONArray.Create();
            z.AddElement(z2);
        end
        else
        begin
            for i := Low(Z_axis^) to High(Z_axis^) do
            begin
                z2 := TJSONArray.Create();
                z.AddElement(z2);
                for j := Low(Z_axis^[i]) to High(Z_axis^[i]) do
                    z2.Add(Z_axis^[i, j]);
            end;
        end;

        pd_el := TJSONArray.Create();
        o.AddPair('PD Elements', pd_el);
        if length(PD_Elements^) = 0 then
        begin
            pd_el2 := TJSONArray.Create();
            pd_el.AddElement(pd_el2);
        end
        else
        begin
            for i := Low(PD_Elements^) to High(PD_Elements^) do
            begin
                pd_el2 := TJSONArray.Create();
                pd_el.AddElement(pd_el2);
                for j := Low(PD_Elements^[i]) to High(PD_Elements^[i]) do
                    pd_el2.Add(PD_Elements^[i, j]);
            end;
        end;

        bus_n := TJSONArray.Create();
        o.AddPair('Bus names', bus_n);
        for i := Low(Bus_names^) to High(Bus_names^) do
            bus_n.Add(Bus_names^[i]);

    finally
        Result := Ansistring(o.ToString);
        o.Free;
    end;
end;

function StrippedOfNonAscii(const s: String): String;
var
    i, Count: Integer;
begin
    SetLength(Result, Length(s));
    Count := 0;
    for i := 1 to Length(s) do
    begin
        if ((s[i] >= #32) and (s[i] <= #127)) or
            (s[i] = #10) or (s[i] = #13) then
        begin
            inc(Count);
            Result[Count] := s[i];
        end;
    end;
    SetLength(Result, Count);
end;

function processExists(exeFileName: String): Boolean;
var
    ContinueLoop: Boolean;
    FSnapshotHandle: THandle;
    FProcessEntry32: TProcessEntry32;
begin
    FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
    Result := FALSE;
    while Integer(ContinueLoop) <> 0 do
    begin
        if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
            UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
            UpperCase(ExeFileName))) then
        begin
            Result := TRUE;
        end;
        ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
    CloseHandle(FSnapshotHandle);
end;

function GetIPFromHost(var IPaddr, WSAErr: String): Boolean;
type
    Name = array[0..100] of ansiChar;
    PName = ^Name;
var
    HEnt: pHostEnt;
    HName: PName;
    WSAData: TWSAData;
    i: Integer;
begin
    Result := FALSE;
    if WSAStartup($0101, WSAData) <> 0 then
    begin
        WSAErr := 'Winsock is not responding."';
        Exit;
    end;
    IPaddr := '';
    New(HName);
    if GetHostName(HName^, SizeOf(Name)) = 0 then
    begin
    //HostName := StrPas(HName^);
        HEnt := GetHostByName(HName^);
        for i := 0 to HEnt^.h_length - 1 do
            IPaddr :=
                Concat(IPaddr,
                IntToStr(Ord(HEnt^.h_addr_list^[i])) + '.');
        SetLength(IPaddr, Length(IPaddr) - 1);
        Result := TRUE;
    end
    else
    begin
        case WSAGetLastError of
            WSANOTINITIALISED:
                WSAErr := 'WSANotInitialised';
            WSAENETDOWN:
                WSAErr := 'WSAENetDown';
            WSAEINPROGRESS:
                WSAErr := 'WSAEInProgress';
        end;
    end;
    Dispose(HName);
    WSACleanup;
end;

/////////////////////////////// Socket functions ///////////////////////////////
procedure TDSSConnect.Connect;
var
    launched: Integer;
    OldMode: DWORD;
    counter: Integer;
    IP, Err: String;
begin
  // If the socket is already connected: exits.
    if Assigned(MySocket) then
        if (MySocket.Socket.Connected = TRUE) then
            exit;

  // Socket setup
    MySocket := TClientSocket.Create(NIL);
    MySocket.ClientType := ctBlocking;
//  MySocket.OnRead:=MySocketRead;
    MySocket.OnDisconnect := MySocketDisconnect;
    MySocket.OnError := MySocketError;

    MySocket.Port := 47625; // DSS Visualization Tool port
    if GetIPFromHost(IP, Err) then
    begin
        MySocket.Address := IP; // Local IP Address
    end
    else
        DoSimpleMsg(Err, 0);
    OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
        MySocket.Open; //Activates the client
    except // Error on conection
        counter := 0; // A maximum of 15 attempts to connect
        repeat // If not connected tries to launch and connnect
            if DSS_Viz_installed then   // If the app is installed
            begin
        // and is not running
                if not processExists('DSS Visualization Tool.exe') then
                begin
            // Launches the app
                    launched := ShellExecute(0, 'open', Pchar(DSS_Viz_path), NIL, NIL, 5);
                    sleep(500);
                    if launched < 33 then // If not launched.
                        DoSimpleMsg('Error on DSS Visualization Tool.', 0);
                end;
            end
            else
                DoSimpleMsg('The DSS Visualization Tool can not be found.', 0);
            try
                MySocket.Open; //Activates the client
            except  // Error on conection
                sleep(1000);   // If error on opening waits
                inc(counter);  // And record the attempt
            end;
        until (MySocket.Socket.Connected = TRUE) or (counter > 14);
        if (MySocket.Socket.Connected = FALSE) then
            DoSimpleMsg('Connection to ' + MySocket.Address + ' port ' +
                IntToStr(MySocket.Port) + ' failed.', 0);
    end;
    SetErrorMode(OldMode);
end;

procedure TDSSConnect.Disconnect;
begin
    if (MySocket.Socket.Connected = TRUE) then
    begin
        MySocket.Close; //Disconnects the client
    end;
end;

procedure TDSSConnect.MySocketRead(Sender: TObject; Socket: TCustomWinSocket);
begin
//  myStr:=Socket.ReceiveText;     Does not work with blicking connections
//  if (myStr='Handler ready'+#13#10) then
//    TCPHandlerReady:=True;
//  DoSimpleMsg('Server msg:'+myStr,0);
end;

procedure TDSSConnect.MySocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
//  myStr:='Disconnected';
//  Socket.SendText(myStr);//Send the “Disconnected” message to the server
//  DoSimpleMsg(myStr,0);
end;

procedure TDSSConnect.MySocketError(Sender: TObject; Socket: TCustomWinSocket;
    ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
    ErrorCode := 0;
    MySocket.Active := FALSE;
//  DoSimpleMsg('Socket error',0);
end;

//////////////////////////////// MSG functions /////////////////////////////////
procedure TDSSConnect.MonitorPlotMsg(ObjectName: String);
var
    MSG: Ansistring;
    pMon: TMonitorObj;
    activesave: Integer;
    tempS: String;
    Found: Boolean;
    ListSize: Integer;
    SaveDelims: String;
    SaveWhiteSpace: String;

    time, channel, Z_axis: DoubleArray2d;
    headers, Bus_Names: StringArray1d;
    phase: IntegerArray1d;
    PD_Elements: StringArray2d;
    model_path: String;

    Header: THeaderRec;
    FirstCol: String;
    AllocSize: Integer;
    SngBuffer: pSingleArray;
    i, k, index: Integer;
    hr: Single;
    s: Single;

begin
    if (MySocket.Socket.Connected = FALSE) then
        exit;

  // Monitors.Nme Write
    if ActiveCircuit <> NIL then
    begin      // Search list of monitors in active circuit for name
        with ActiveCircuit.Monitors do
        begin
            tempS := Widestring(ObjectName);  // Convert to Pascal String
            Found := FALSE;
            ActiveSave := ActiveIndex;
            pMon := First;
            while pMon <> NIL do
            begin
                if (CompareText(pMon.Name, tempS) = 0) then
                begin
                    ActiveCircuit.ActiveCktElement := pMon;
                    Found := TRUE;
                    Break;
                end;
                pMon := Next;
            end;
            if not Found then
            begin
                DoSimpleMsg('Monitor "' + tempS + '" Not Found in Active Circuit.', 5004);
                pMon := Get(ActiveSave);    // Restore active Monerator
                ActiveCircuit.ActiveCktElement := pMon;
            end;
        end;
    end;

  // Monitors.dblSecs
    if ActiveCircuit <> NIL then
    begin
        pMon := ActiveCircuit.Monitors.Active;
        if pMon.SampleCount > 0 then
        begin
            SetLength(time, 1, pMon.SampleCount);
            ReadMonitorHeader(Header, FALSE);   // leave at beginning of data
            AuxParser.CmdString := String(Header.StrBuffer);
            AuxParser.AutoIncrement := TRUE;
            FirstCol := AuxParser.StrValue;  // Get rid of first two columns
            AuxParser.AutoIncrement := FALSE;
      // check first col to see if it is "Hour"
            if System.Sysutils.CompareText(FirstCol, 'hour') = 0 then
            begin
                AllocSize := Sizeof(SngBuffer^[1]) * Header.RecordSize;
                SngBuffer := Allocmem(AllocSize);
                k := 0;
                for i := 1 to pMon.SampleCount do
                begin
                    with pMon.MonitorStream do
                    begin
                        Read(hr, SizeOf(hr));  // Hour
                        Read(s, SizeOf(s));   // Seconds past the hour
                        Read(sngBuffer^[1], AllocSize);  // read rest of record
                    end;
                    time[0, k] := hr * 3600 + s;
                    inc(k);
                end;
                Reallocmem(SngBuffer, 0);  // Dispose of buffer
            end
            else
            begin   // Not time solution, so return nil array
                time := NIL;
                pMon.MonitorStream.Seek(0, soFromEnd); // leave stream at end
            end;
        end
        else
            time := NIL;
    end;

  // Monitors.Header
    SetLength(headers, 1);
    headers[0] := 'NONE';
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit do
        begin
            ReadMonitorHeader(Header, TRUE);
            if Header.RecordSize > 0 then
            begin
                ListSize := Header.RecordSize;
                SetLength(headers, ListSize);
                k := 0;
                SaveDelims := AuxParser.Delimiters;
                AuxParser.Delimiters := ',';
                SaveWhiteSpace := AuxParser.Whitespace;
                AuxParser.Whitespace := '';
                AuxParser.CmdString := String(Header.StrBuffer);
                AuxParser.AutoIncrement := TRUE;
                AuxParser.StrValue;  // Get rid of first two columns
                AuxParser.StrValue;
                while k < ListSize do
                begin
                    headers[k] := AuxParser.StrValue;
                    Inc(k);
                end;
                AuxParser.AutoIncrement := FALSE; // be a good citizen
                AuxParser.Delimiters := SaveDelims;
                AuxParser.Whitespace := SaveWhiteSpace;
            end;
        end;
        headers[High(headers)] := StrippedOfNonAscii(headers[High(headers)]);
    end;

  // Monitors.Channels
    if ActiveCircuit <> NIL then
    begin
        pMon := ActiveCircuit.Monitors.Active;
        if pMon.SampleCount > 0 then
        begin
            SetLength(channel, High(headers) + 1, pMon.SampleCount);
            for index := Low(headers) to High(headers) do
            begin
                ReadMonitorHeader(Header, FALSE);  // FALSE = leave at beginning of data
                AuxParser.CmdString := String(Header.StrBuffer);
                AuxParser.AutoIncrement := TRUE;
                FirstCol := AuxParser.StrValue;  // Get rid of first two columns
                AuxParser.AutoIncrement := FALSE;
                AllocSize := Sizeof(SngBuffer^[1]) * Header.RecordSize;
                SngBuffer := Allocmem(AllocSize);
                k := 0;
                for i := 1 to pMon.SampleCount do
                begin
                    with pMon.MonitorStream do
                    begin
                        Read(hr, SizeOf(hr));
                        Read(s, SizeOf(s));
                        Read(sngBuffer^[1], AllocSize);  // read rest of record
                    end;
                    channel[index, k] := sngBuffer^[index + 1];
                    inc(k);
                end;
                Reallocmem(SngBuffer, 0);  // Dispose of buffer
            end;
        end
        else
            channel := NIL;
    end;

    SetLength(Z_axis, 0, 0);
    SetLength(Bus_Names, 0);
    SetLength(PD_Elements, 0, 0);
    SetLength(phase, 0);

    model_path := StringReplace(LastFileCompiled, '\', '\\', [rfReplaceAll]);
    MSG := flatten2JSON(model_path, 'Monitor.' + ObjectName, 'xyplot', 'Time (s)', @time, @headers, @channel, @phase, @Z_axis, @PD_Elements, @Bus_Names);
    MySocket.Socket.SendText(flat_int2str(Length(MSG)));//Sends the length
    MySocket.Socket.SendText(MSG);//Send the message's content to the server
end;

procedure TDSSConnect.LoadshapePlotMsg(ObjectName: String);
var
    ActiveLSObject: TLoadshapeObj;
    MSG: Ansistring;
    sinterfal: Double;
    npts: Integer;
    i, k: Integer;
    x_label: String;
    y_labels: StringArray1d;

    time, channel, Z_axis: DoubleArray2d;
    Bus_Names: StringArray1d;
    phase: IntegerArray1d;
    PD_Elements: StringArray2d;
    model_path: String;

begin
    if (MySocket.Socket.Connected = FALSE) then
        exit;

  // LoadShapes.Name write
    ActiveLSObject := NIL;
    if ActiveCircuit <> NIL then
    begin
        if LoadshapeClass.SetActive(Widestring(ObjectName)) then
        begin
            ActiveLSObject := LoadshapeClass.ElementList.Active;
            ActiveDSSObject := ActiveLSObject;
        end
        else
        begin
            DoSimpleMsg('Loadshape "' + Widestring(ObjectName) +
                '" Not Found in Active Circuit.', 77003);
        end;
    end;

  // LoadShapes.Sinterval read
    sinterfal := 0.0;
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            sinterfal := ActiveLSObject.Interval * 3600.0;

    if sinterfal = 0.0 then
    begin
      // LoadShapes.Timearray read
        SetLength(time, 1, 1);
        time[0, 0] := 0.0;  // error condition: one element array=0
        if ActiveCircuit <> NIL then
        begin
            if ActiveLSObject <> NIL then
            begin
                if ActiveLSObject.hours <> NIL then
                begin
                    SetLength(time, 1, ActiveLSObject.NumPoints);
                    for k := 0 to ActiveLSObject.NumPoints - 1 do
                        time[0, k] := ActiveLSObject.Hours^[k + 1];
                    x_label := 'Time (h)';
                end
            end
            else
            begin
                DoSimpleMsg('No active Loadshape Object found.', 61001);
            end;
        end;
    end
    else
    begin
      // LoadShapes.Npts read
        npts := 0;
        if ActiveCircuit <> NIL then
            if ActiveLSObject <> NIL then
                npts := ActiveLSObject.NumPoints;
        SetLength(time, 1, npts);
        for i := 0 to npts - 1 do
            time[0, i] := i * sinterfal;
        x_label := 'Time (s)';
    end;

  // LoadShapes.PMult read
    if ActiveCircuit <> NIL then
    begin
        if ActiveLSObject <> NIL then
        begin
            SetLength(channel, 1, ActiveLSObject.NumPoints);
            for k := 0 to ActiveLSObject.NumPoints - 1 do
                channel[0, k] := ActiveLSObject.PMultipliers^[k + 1];
            SetLength(y_labels, 1);
            y_labels[0] := 'Mult.';
        end
        else
        begin
            DoSimpleMsg('No active Loadshape Object found.', 61001);
        end;
    end;

  // LoadShapes.QMult read
    if ActiveCircuit <> NIL then
    begin
        if ActiveLSObject <> NIL then
        begin
            if assigned(ActiveLSObject.QMultipliers) then
            begin
                SetLength(channel, 2, ActiveLSObject.NumPoints);
                for k := 0 to ActiveLSObject.NumPoints - 1 do
                    channel[1, k] := ActiveLSObject.QMultipliers^[k + 1];
                SetLength(y_labels, 2);
                y_labels[0] := 'P Mult.';
                y_labels[1] := 'Q Mult.';
            end;
        end
        else
        begin
            DoSimpleMsg('No active Loadshape Object found.', 61001);
        end;
    end;

    SetLength(Z_axis, 0, 0);
    SetLength(Bus_Names, 0);
    SetLength(phase, 0);
    SetLength(PD_Elements, 0, 0);

    model_path := StringReplace(LastFileCompiled, '\', '\\', [rfReplaceAll]);
    MSG := flatten2JSON(model_path, 'Loadshape.' + ObjectName, 'xyplot',
        x_label, @time, @y_labels, @channel, @phase, @Z_axis, @PD_Elements, @Bus_Names);
    MySocket.Socket.SendText(flat_int2str(Length(MSG)));//Sends the length
    MySocket.Socket.SendText(MSG);//Send the message's content to the server
end;

procedure TDSSConnect.ProfilePlotMsg(ObjectName: String);
var
    MSG: Ansistring;
    iEnergyMeter: Integer;
    ActiveEnergyMeter: TEnergyMeterObj;
    PresentCktElement: TDSSCktElement;
    Bus1, Bus2: TDSSbus;
    puV1, puV2: Double;
    iphs: Integer;
    Linetype: Integer;
    counter: Integer;

    x_axis, y_axis, z_axis: DoubleArray2d;
    y_labels: StringArray1d;
    Bus_Names: StringArray1d;
    phase: IntegerArray1d;
    PD_Elements: StringArray2d;
    model_path: String;

begin
    if (MySocket.Socket.Connected = FALSE) then
        exit;

    SetLength(x_axis, 0, 2);
    SetLength(y_axis, 0, 2);
    SetLength(phase, 0);
    SetLength(y_labels, 0);
    SetLength(z_axis, 1, 0);
    counter := 0;

    iEnergyMeter := EnergyMeterClass.First;
    while iEnergyMeter > 0 do
    begin
        ActiveEnergyMeter := EnergyMeterClass.GetActiveObj;
        PresentCktElement := ActiveEnergyMeter.BranchList.First;
        while PresentCktElement <> NIL do
        begin
            if IslineElement(PresentCktElement) then
                with ActiveCircuit do
                begin
                    Bus1 := Buses^[PresentCktElement.Terminals^[1].BusRef];
                    Bus2 := Buses^[PresentCktElement.Terminals^[2].BusRef];
        {Now determin which phase to plot}
                    if (Bus1.kVBase > 0.0) and (Bus2.kVBase > 0.0) then
                    begin
                        for iphs := 1 to 3 do
                            if (Bus1.FindIdx(Iphs) > 0) and (Bus2.FindIdx(Iphs) > 0) then
                            begin
                                SetLength(x_axis, Length(x_axis) + 1, 2);
                                SetLength(y_axis, Length(y_axis) + 1, 2);
                                SetLength(z_axis, 1, Length(z_axis[0]) + 1);
                                SetLength(phase, Length(phase) + 1);
                                SetLength(y_labels, Length(y_labels) + 1);
                                if Bus1.kVBase < 1.0 then
                                    Linetype := 2
                                else
                                    Linetype := 0;
                                puV1 := CABS(Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                                puV2 := CABS(Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                                x_axis[counter, 0] := Bus1.DistFromMeter;
                                x_axis[counter, 1] := Bus2.DistFromMeter;
                                y_axis[counter, 0] := puV1;
                                y_axis[counter, 1] := puV2;
                                y_labels[counter] := PresentCktElement.Name;
                                phase[counter] := iphs;
                                z_axis[0, counter] := Linetype;
                                inc(counter);
                            end;
                    end;
                end;
            PresentCktElement := ActiveEnergyMeter.BranchList.GoForward;
        end;
        iEnergyMeter := EnergyMeterClass.Next;
    end;

    SetLength(Bus_Names, 0);
    SetLength(PD_Elements, 0, 0);

    model_path := StringReplace(LastFileCompiled, '\', '\\', [rfReplaceAll]);
    MSG := flatten2JSON(model_path, 'Voltage Profile', 'profile', 'Distance (km)', @x_axis, @y_labels, @y_axis, @phase, @Z_axis, @PD_Elements, @Bus_Names);
    MySocket.Socket.SendText(flat_int2str(Length(MSG)));//Sends the length
    MySocket.Socket.SendText(MSG);//Send the message's content to the server
end;

procedure TDSSConnect.ScatterPlotMsg;
var
    MSG: Ansistring;
    NumBuses, NumPDelements, Nvalues, iV, jj, NodeIdx: Integer;
    counter, i, k: Integer;
    pBus: TDSSBus;
    BaseFactor: Double;
    voltsp: polar;
    ActivePDElement: TPDElement;

    x_axis, y_axis, z_axis: DoubleArray2d;
    y_labels: StringArray1d;
    Bus_Names: StringArray1d;
    phase: IntegerArray1d;
    PD_Elements: StringArray2d;
    model_path: String;

begin
    if (MySocket.Socket.Connected = FALSE) then
        exit;

    SetLength(Bus_Names, 0);
    SetLength(phase, 0);
    SetLength(x_axis, 1, 0);
    SetLength(y_axis, 1, 0);
    SetLength(z_axis, 0, 3);
    counter := 0;

  // Bus info
    if ActiveCircuit <> NIL then
    begin
        NumBuses := ActiveCircuit.NumBuses;  // Circuit.NumBuses
        for k := 0 to NumBuses - 1 do
        begin
            with ActiveCircuit do
            begin
                ActiveBusIndex := k + 1; // SetActiveBus_i
                if (Buses^[ActiveCircuit.ActiveBusIndex].Coorddefined) then
                begin
                    SetLength(Bus_Names, counter + 1);
                    SetLength(x_axis, 1, counter + 1);
                    SetLength(y_axis, 1, counter + 1);
                    SetLength(z_axis, counter + 1, 3);
                    Bus_Names[counter] := BusList.Get(ActiveBusIndex); // Active bus name
                    x_axis[0, counter] := Buses^[ActiveCircuit.ActiveBusIndex].x;
                    y_axis[0, counter] := Buses^[ActiveCircuit.ActiveBusIndex].y;
          ///// Get Vmag pu for each phase
                    pBus := Buses^[ActiveBusIndex];
                    Nvalues := pBus.NumNodesThisBus;
          ///z_axis[counter,0]:=0;z_axis[counter,1]:=0;z_axis[counter,2]:=0;
                    iV := 0;
                    jj := 1;
                    with pBus do
                    begin
                        if kVBase > 0.0 then
                            BaseFactor := 1000.0 * kVBase
                        else
                            BaseFactor := 1.0;
                        for i := 1 to NValues do
                        begin
              // this code so nodes come out in order from smallest to larges
                            repeat
                //Get the index of the Node that matches jj
                                NodeIdx := FindIdx(jj);
                                inc(jj)
                            until NodeIdx > 0;
              // referenced to pBus
                            Voltsp := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);
                            if iV < 4 then
                                z_axis[counter, iV] := Voltsp.mag / BaseFactor;
                            Inc(iV);
                        end;
                    end;
                    inc(counter);
                end;
            end;
        end;
    end;

  // PD Elements info
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit do
        begin
            NumPDelements := PDElements.ListSize; // PD Elements count
            ActivePDElement := PDElements.First; // First PD element
            ActiveCktElement := ActivePDElement;
        end;
        SetLength(PD_Elements, NumPDelements, 3);
        for k := 0 to NumPDelements - 1 do
        begin
            with ActiveCircuit do
            begin
                with ActivePDElement do
                begin
          // PDElements.Name read
                    PD_Elements[k, 0] := ParentClass.Name + '.' + Name;
                end;
                for i := 1 to 2 do
                begin
          // PDElements bus Names read
                    PD_Elements[k, i] := ActiveCktElement.GetBus(i);
                end;
                ActivePDElement := PDElements.Next;
                ActiveCktElement := ActivePDElement;
            end;
        end;
    end;

    SetLength(phase, 0);
    SetLength(y_labels, 3);
    y_labels[0] := 'V (p.u.) n 1';
    y_labels[1] := 'V (p.u.) n 2';
    y_labels[2] := 'V (p.u.) n 3';

    model_path := StringReplace(LastFileCompiled, '\', '\\', [rfReplaceAll]);
    MSG := flatten2JSON(model_path, 'Voltage Magnitude (geo)', 'geo_scatter', '', @x_axis, @y_labels, @y_axis, @phase, @Z_axis, @PD_Elements, @Bus_Names);
    MySocket.Socket.SendText(flat_int2str(Length(MSG)));//Sends the length
    MySocket.Socket.SendText(MSG);//Send the message's content to the server
end;

procedure TDSSConnect.EvolutionPlotMsg;
var
    MSG: Ansistring;
    ObjClass, ObjName: String;
    handle: Integer;
    load_names: StringArray1d;
    k, index, load_counter, i, counter: Integer;
    pLoad: TLoadObj;
    pMon: TMonitorObj;
    activesave: Integer;
    tempS: String;
    Found: Boolean;
    ObjectName: String;
    num_ch, init_leng: Integer;
    Header: THeaderRec;
    ListSize: Integer;
    FirstCol: String;
    AllocSize: Integer;
    SngBuffer: pSingleArray;
    hr, s: Single;
    LoadVbase: DoubleArray1d;
    tempt_string: String;
    BusRef, active_element: Integer;

    x_axis, y_axis, z_axis: DoubleArray2d;
    y_labels: StringArray1d;
    Bus_Names: StringArray1d;
    phase: IntegerArray1d;
    PD_Elements: StringArray2d;
    model_path: String;

begin
    if (MySocket.Socket.Connected = FALSE) then
        exit;

    SetLength(x_axis, 0, 0);

  //// Install new monitors
  // Loads.Allnames
    SetLength(load_names, 0);
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
            if Loads.ListSize > 0 then
            begin
                SetLength(load_names, Loads.ListSize);
                k := 0;
                pLoad := Loads.First;
                while pLoad <> NIL do
                begin
                    load_names[k] := pLoad.Name;
                    Inc(k);
                    pLoad := Loads.Next;
                end;
            end;
   // Load monitors: new monitors if they are not available
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
            if Loads.ListSize > 0 then
            begin
                SetObjectClass('Monitor');
                for k := Low(load_names) to High(load_names) do
                begin
                    if SetElementActive('moitor.v_' + load_names[k]) <> 0 then
        //IF ActiveDSSClass.SetActive('V_'+load_names[k]) THEN
                    begin  // IF object already exists.
                        DssExecutive.Command := 'edit monitor.V_' + load_names[k] +
                            ' element=load.' + load_names[k] + ' terminal=1 mode=32';
                    end
                    else
                    begin // IF object does not exist.
                        DssExecutive.Command := 'new monitor.V_' + load_names[k] +
                            ' element=load.' + load_names[k] + ' terminal=1 mode=32';
                    end;
                end;
            end;
  //// Reset monitors
    if ActiveCircuit <> NIL then
    begin
        MonitorClass.ResetAll;
    end;
  //// Solve
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.Solve;
  //// Read load monitors
  // Get Loads kVBase
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit do
        begin
            if Loads.ListSize > 0 then
                SetLength(LoadVbase, Loads.ListSize);
            begin
                for load_counter := Low(load_names) to High(load_names) do
                begin
                    SetElementActive('load.' + load_names[load_counter]);
                    BusRef := ActiveCktElement.Terminals^[1].BusRef;
                    LoadVbase[load_counter] := Buses^[BusRef].kvbase * 1000.0;
                end;
            end;
        end;
    end;
  // Simulation Time
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
            if Loads.ListSize > 0 then
            begin
                GetMonitorTime('V_' + load_names[0], @x_axis);
            end;
  // Vpu
    SetLength(y_axis, 0, 0);
    SetLength(y_labels, 0);
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit do
        begin
            if Loads.ListSize > 0 then
            begin
                for load_counter := Low(load_names) to High(load_names) do
                begin
          // Concatenates the Vpu for each load node
                    ObjectName := ('V_' + load_names[load_counter]);
          // Monitors.Nme Write
//          active_element:=SetElementActive('monitor.'+ObjectName);
                    with ActiveCircuit.Monitors do
                    begin
                        tempS := ObjectName;
                        Found := FALSE;
                        ActiveSave := ActiveIndex;
                        pMon := First;
                        while pMon <> NIL do
                        begin
                            if (CompareText(pMon.Name, tempS) = 0) then
                            begin
                                ActiveCircuit.ActiveCktElement := pMon;
                                Found := TRUE;
                                Break;
                            end;
                            pMon := Next;
                        end;
                        if not Found then
                        begin
                            DoSimpleMsg('Monitor "' + tempS +
                                '" Not Found in Active Circuit.', 5004);
                            pMon := Get(ActiveSave);    // Restore active Monerator
                            ActiveCircuit.ActiveCktElement := pMon;
                        end;
                    end;

          // Monitors.Channels Modified for magnitudes only
                    pMon := ActiveCircuit.Monitors.Active;
                    if pMon.SampleCount > 0 then
                    begin
                        ReadMonitorHeader(Header, FALSE);
                        ListSize := Header.RecordSize;
                        if ListSize > 4 then
                            num_ch := 3// Three phase loads-> terminal 1 voltage (nodes 1,2,3)
                        else
                            num_ch := 1; // Single phase loads-> terminal 1 voltage (node 1)
                        init_leng := Length(y_axis);
                        SetLength(y_axis, init_leng + num_ch, pMon.SampleCount);
                        SetLength(y_labels, init_leng + num_ch);
                        counter := 1;
            // Concatenates new channels
                        for index := init_leng to Length(y_axis) - 1 do
                        begin
              // FALSE = leave at beginning of data
                            ReadMonitorHeader(Header, FALSE);
                            AuxParser.CmdString := String(Header.StrBuffer);
                            AuxParser.AutoIncrement := TRUE;
                            FirstCol := AuxParser.StrValue;  // Get rid of first two columns
                            AuxParser.AutoIncrement := FALSE;
                            AllocSize := Sizeof(SngBuffer^[1]) * Header.RecordSize;
                            SngBuffer := Allocmem(AllocSize);
                            k := 0;
                            for i := 1 to pMon.SampleCount do
                            begin
                                with pMon.MonitorStream do
                                begin
                                    Read(hr, SizeOf(hr));
                                    Read(s, SizeOf(s));
                                    Read(sngBuffer^[1], AllocSize);  // read rest of record
                                end;
                                y_axis[index, k] := sngBuffer^[counter] / LoadVbase[load_counter];
                                inc(k);
                            end;
                            y_labels[index] := load_names[load_counter] + '.' + IntToStr(counter);
                            inc(counter);
                            Reallocmem(SngBuffer, 0);  // Dispose of buffer
                        end;
                    end
                    else
                        y_axis := NIL;
                end;
            end;
        end;
    end;

    SetLength(Bus_Names, 0);
    SetLength(phase, 0);
    SetLength(z_axis, 0, 0);
    SetLength(PD_Elements, 0, 0);

    model_path := StringReplace(LastFileCompiled, '\', '\\', [rfReplaceAll]);
    MSG := flatten2JSON(model_path, 'V (p.u.) Density Evolution - All load voltages',
        'time_evolution', 'Time (s)', @x_axis, @y_labels, @y_axis, @phase, @Z_axis, @PD_Elements, @Bus_Names);
    MySocket.Socket.SendText(flat_int2str(Length(MSG)));//Sends the length
    MySocket.Socket.SendText(MSG);//Send the message's content to the server
end;

procedure TDSSConnect.MatrixPlotMsg;
var
    MSG: Ansistring;
    ArrSize, IMIdx, i: Integer;

    x_axis, y_axis, z_axis: DoubleArray2d;
    y_labels: StringArray1d;
    Bus_Names: StringArray1d;
    phase: IntegerArray1d;
    PD_Elements: StringArray2d;
    model_path: String;

begin
    if (MySocket.Socket.Connected = FALSE) then
        exit;

    SetLength(x_axis, 0, 0);
    SetLength(y_axis, 0, 0);
    SetLength(y_labels, 0);
    SetLength(Bus_Names, 0);
    SetLength(phase, 0);
    SetLength(z_axis, 0, 0);
    SetLength(PD_Elements, 0, 0);

//  // Solution.IncMatrix
//  If ActiveCircuit <> Nil Then
//  Begin
//    with ACtiveCircuit.Solution do
//    begin
//      // Removes the 3 initial zeros and the extra index
//      // Since it starts on 0
//      ArrSize    :=  length(IncMatrix)-4;
//      Setlength(x_axis,1,ArrSize+1);
//      for IMIdx  :=  0 to ArrSize Do
//      Begin
//        x_axis[0,IMIdx]:= IncMatrix[IMIdx+3];
//      end;
//    end;
//  end;
//
//  // Solution.IncMatrixRows
//  If ActiveCircuit <> Nil Then Begin
//    with ACtiveCircuit.Solution,ActiveCircuit do
//    begin
//        ArrSize :=  length(Inc_Mat_Rows)-1;
//        Setlength(y_labels,ArrSize+1);
//        for IMIdx  :=  0 to ArrSize Do
//        Begin
//           y_labels[IMIdx] := Inc_Mat_Rows[IMIdx];
//        End;
//    end;
//  END;
//
//  // Solution.IncMatrixCols
//  If ActiveCircuit <> Nil Then Begin
//    with ActiveCircuit.Solution,ActiveCircuit  do
//    begin
//      if IncMat_Ordered then
//      begin
//        ArrSize    :=  length(Inc_Mat_Cols)-1;
//        Setlength(Bus_Names,ArrSize+1);
//        for IMIdx  :=  0 to ArrSize Do
//        Begin
//           Bus_Names[IMIdx] := Inc_Mat_Cols[IMIdx];
//        End;
//      end
//    else
//    begin
//      Setlength(Bus_Names,NumBuses);
//      FOR i := 0 to NumBuses-1 DO
//      Begin
//         Bus_Names[i] := BusList.Get(i+1);
//      End;
//    end;
//    end;
//  End;

    model_path := StringReplace(LastFileCompiled, '\', '\\', [rfReplaceAll]);
    MSG := flatten2JSON(model_path, 'Incidence matrix',
        'matrix', '', @x_axis, @y_labels, @y_axis, @phase, @Z_axis, @PD_Elements, @Bus_Names);
    MySocket.Socket.SendText(flat_int2str(Length(MSG)));//Sends the length
    MySocket.Socket.SendText(MSG);//Send the message's content to the server
end;

/////////////////////////////// Class functions ///////////////////////////////
procedure TDSSConnect.SetDefaults;
begin
    myStr := 'Default';
end;

constructor TDSSConnect.Create;
begin
    SetDefaults;
end;

destructor TDSSConnect.Destroy;
begin
    inherited;
end;

initialization
    DSSConnectObj := NIL; // Instantiate only if connect command issued

finalization
    if Assigned(DSSConnectObj) then
        DSSConnectObj.Free;

end.
