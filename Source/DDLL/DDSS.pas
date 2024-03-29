unit DDSS;

interface

function DSSI(mode: Longint; arg: Longint): Longint; CDECL;
function DSSS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure DSSV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
     {$IFDEF FPC_DLL}
    CmdForms,
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
    ParserDel,
    ExecCommands,
    ExecOptions;

function DSSI(mode: Longint; arg: Longint): Longint; CDECL;
begin
    Result := 0;
    case mode of
        0:
        begin  // DSS.NumCircuits
            Result := ActiveCircuit[ActiveActor].NumCircuits;
        end;
        1:
        begin  // DSS.ClearAll
            DoClearAllCmd;
        end;
        2:
        begin
    {$IFDEF FPC_DLL}
            Result := 0; // edit form not supported in FPC, but don't throw an error for trying...
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
function DSSS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
begin
    Result := Pansichar(Ansistring('0')); // Default return value
    case mode of
        0:
        begin  // DSS.NewCircuit
            MakeNewCircuit(String(arg));
            Result := Pansichar(Ansistring('New Circuit'));
        end;
        1:
        begin  // DSS.Version
            Result := Pansichar(Ansistring(VersionString + '; License Status: Open '));
        end;
        2:
        begin  // DSS.DataPath read
            Result := Pansichar(Ansistring(DataDirectory[ActiveActor]));
        end;
        3:
        begin  // DSS.DataPath write
            SetDataPath(String(arg));
        end;
        4:
        begin  // DSS.DefaultEditor
            Result := Pansichar(Ansistring(DSSGlobals.DefaultEditor));
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//********************************Variant Type properties***************************
procedure DSSV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    i: Integer;

begin
    case mode of
        0:
        begin  // DSS.Classes
            myType := 4;        // String
            setlength(myStrArray, 0);
            for i := 1 to NumIntrinsicClasses do
            begin
                WriteStr2Array(TDSSClass(DssClassList[ActiveActor].Get(i)).Name);
                WriteStr2Array(Char(0));
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        1:
        begin  // DSS.UserClasses
            myType := 4;        // String
            setlength(myStrArray, 0);
            if NumUserClasses > 0 then
            begin
                for i := NumIntrinsicClasses + 1 to DSSClassList[ActiveActor].ListSize do
                begin
                    WriteStr2Array(TDSSClass(DssClassList[ActiveActor].Get(i)).Name);
                    WriteStr2Array(Char(0));
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end
    else
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
end;


end.
