unit DDSS;

interface

function DSSI(mode: Longint; arg: Longint): Longint; CDECL;
function DSSS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure DSSV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
     {$IFDEF FPC}
    CMDForms,
     {$ELSE}
    DSSForms,
    Forms,
    ScriptFormNormal,
     {$ENDIF}
    DSSClass,
    Exechelper,
    sysUtils,
    Executive,
    Variants,
    ExecCommands,
    ExecOptions;

function DSSI(mode: Longint; arg: Longint): Longint; CDECL;
begin
    Result := 0;
    case mode of
        0:
        begin  // DSS.NumCircuits
            Result := NumCircuits;
        end;
        1:
        begin  // DSS.ClearAll
            DoClearCmd;
        end;
        2:
        begin  // DSS.ShowPanel
    {$IFDEF FPC} // TODO - what can be done here?
    {$ELSE}
            if not Assigned(MainEditFormNormal) then
            begin
                MainEditFormNormal := TMainEditFormnormal.Create(NIL);
                MainEditFormNormal.Caption := 'OpenDSS Script Form';
                MainEditFormNormal.isMainWindow := TRUE;
            end;
            MainEditFormNormal.Show;
    {$ENDIF}
        end;
        3:
        begin  // DSS.Start
            Result := 1;
        end;
        4:
        begin  // DSS.NumClasses
            Result := NumIntrinsicClasses;
        end;
        5:
        begin  // DSS.NumUserClasses
            Result := NumUserClasses;
        end;
        6:
        begin  // DSS.Reset
        {Put any code here necessary to reset for specific systems};

        end;
        7:
        begin  // DSS.Allowforms read
            if NoFormsAllowed then
                Result := 1
            else
                Result := 0;
        end;
        8:
        begin  // DSS.Allowforms write
            if arg = 0 then
                NoFormsAllowed := TRUE  // Only set to False
            else
                NoFormsAllowed := FALSE;
//     If NoFormsAllowed Then CloseDownForms;  // DSSForms
        end
    else
        Result := -1;
    end;
end;

//********************************String Type properties***************************
function DSSS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
begin
    Result := pAnsiChar(Ansistring('0')); // Default return value
    case mode of
        0:
        begin  // DSS.NewCircuit
            MakeNewCircuit(String(arg));
            Result := pAnsiChar(Ansistring('New Circuit'));
        end;
        1:
        begin  // DSS.Version
            Result := pAnsiChar(Ansistring(VersionString + '; License Status: Open '));
        end;
        2:
        begin  // DSS.DataPath read
            Result := pAnsiChar(Ansistring(DataDirectory));
        end;
        3:
        begin  // DSS.DataPath write
            SetDataPath(String(arg));
        end;
        4:
        begin  // DSS.DefaultEditor
            Result := pAnsiChar(Ansistring(DSSGlobals.DefaultEditor));
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not valid'));
    end;
end;

//********************************Variant Type properties***************************
procedure DSSV(mode: Longint; out arg: Variant); CDECL;

var
    i, k: Integer;

begin
    case mode of
        0:
        begin  // DSS.Classes
            arg := VarArrayCreate([0, NumIntrinsicClasses - 1], varOleStr);
            k := 0;
            for i := 1 to NumIntrinsicClasses do
            begin
                arg[k] := TDSSClass(DssClassList.Get(i)).Name;
                Inc(k);
            end;
        end;
        1:
        begin  // DSS.UserClasses
            if NumUserClasses > 0 then
            begin
                arg := VarArrayCreate([0, NumUserClasses - 1], varOleStr);
                k := 0;
                for i := NumIntrinsicClasses + 1 to DSSClassList.ListSize do
                begin
                    arg[k] := TDSSClass(DssClassList.Get(i)).Name;
                    Inc(k);
                end;
            end
            else
                arg := VarArrayCreate([0, 0], varOleStr);
        end
    else
        arg[0] := 'Error, parameter not valid';
    end;
end;


end.
