{**************************Memory mapped files Library**************************
* This library was added to OpenDSS to handle the different functions related  *
* with mapping files into memory to accelerate the data manipulation process   *
* when including this device to the simulation                                 *
*                                                                              *
* last modification: 09-12-2016                                                *
********************************************************************************
}

unit MemoryMap_lib;

interface

uses
    Classes;

function Create_Meter_Space(Init_Str: String): TBytesStream; OVERLOAD;
procedure WriteintoMemStr(Mem_Space: TBytesStream; Content: String); OVERLOAD;
procedure WriteintoMem(Mem_Space: TBytesStream; Content: Double); OVERLOAD;
procedure CloseMHandler(Mem_Space: TBytesStream; const Dest_Path: String; AppendFile: Boolean); OVERLOAD;
procedure Write_String(Mem_Space: TBytesStream; const Content: String);

implementation

uses
{$IFNDEF FPC}
    windows,
{$ENDIF}
    sysutils,
    math,
    Dialogs,
    DSSGlobals;

type
    TByteArr = array of uint8;

var
    wordBuf: Word;
// $01A0 is Header for identifying String type data
// $02A0 is Header for identifying Double type data
//******************************************************************************
// Creates a new BytesStream for the caller
// Returns the handler to handle the new memory space
//******************************************************************************
function Create_Meter_Space(Init_Str: String): TBytesStream; OVERLOAD;
var
    Mem_Space: TBytesStream;
begin
    Mem_Space := TBytesStream.Create();
{$IFNDEF FPC}
    Mem_Space.WriteData($01A0);
{$ELSE}
    wordBuf := $01A0;
    Mem_Space.Write(wordBuf, 2);
{$ENDIF}
    ;
    Write_String(Mem_Space, Init_Str);
    Result := Mem_Space;
end;
//******************************************************************************
// Writes a string into the specified BytesStream
//******************************************************************************
procedure WriteintoMemStr(Mem_Space: TBytesStream; Content: String); OVERLOAD;
begin
{$IFNDEF FPC}
    Mem_Space.WriteData($01A0);
{$ELSE}
    wordBuf := $01A0;
    Mem_Space.Write(wordBuf, 2);
{$ENDIF}
    ;
    Write_String(Mem_Space, Content);
end;
//******************************************************************************
// Writes a DBL into the specified BytesStream
//******************************************************************************
procedure WriteintoMem(Mem_Space: TBytesStream; Content: Double); OVERLOAD;
begin
{$IFNDEF FPC}
    Mem_Space.WriteData($02A0);
    Mem_Space.WriteData(Content);
{$ELSE}
    wordBuf := $02A0;
    Mem_Space.Write(wordBuf, 2);
    Mem_Space.Write(Content, sizeof(Double));
{$ENDIF}
    ;
end;
//******************************************************************************
// Saves the content of the BytesStream into the specified file path
// and destroys the ByteStream
//******************************************************************************
procedure CloseMHandler(Mem_Space: TBytesStream; const Dest_Path: String; AppendFile: Boolean); OVERLOAD;
var
    F: TextFile;
    buffer: Uint8;
    idx: Integer;
    MWrite, Fhead: Boolean;
    MType: Integer;
    MSize: Longint;
    TVariableDbl: Double;
begin

{ Open Output file; check for errors}
    try
        AssignFile(F, Dest_path);
        if AppendFile then
            Append(F)
        else
            Rewrite(F);
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error Attempting to open file: "' + Dest_path + '. ' + E.Message, 159000);
            CloseFile(F);
            Exit;
        end;
    end;

    try

        idx := 0;
        MType := 0;  // initialize to eliminate compiler warning
        MWrite := FALSE;
        Fhead := TRUE;
        MSize := Mem_Space.Size;
        while idx < MSize do
        begin
            Mem_Space.Position := idx;
            if MWrite = FALSE then       // Checks if we are writing
            begin
                Mem_Space.Read(buffer, 1);
                if buffer = $A0 then       // If not, checks the header of the next content
                begin
                    Mem_Space.Position := idx + 1;
                    Mem_Space.Read(buffer, 1);
                    if buffer < $03 then
                    begin
                        MWrite := TRUE;
                        MType := buffer;
                        inc(idx);
                    end;
                end;
            end
            else
            begin
                case MType of
                    1:
                    begin        // Is a string
                        Mem_Space.Read(buffer, 1);
                        if (buffer <> $A0) then
                        begin
                            if Fhead then
                                Fhead := FALSE;
                            if (buffer = 10) then
                            begin
                                writeln(F);
                                Fhead := TRUE;
                                inc(idx);
                            end
                            else
                            if (buffer > 0) then
                                write(F, Char(buffer));
                        end
                        else
                        begin
                            idx := idx - 1;
                            MWrite := FALSE;
                        end;
                    end;
                    2:
                    begin        // Is a Double
              {$IFNDEF FPC}
                        Mem_Space.ReadData(TVariableDbl, 8);
              {$ELSE}
                        Mem_Space.Read(TVariableDbl, sizeof(Double));
              {$ENDIF}
                        idx := idx + 7;
                        if Fhead then
                            Fhead := FALSE
                        else
                            write(F, ', ');
                        write(F, Format('%-g', [TVariableDbl]));
                        MWrite := FALSE;
                    end
                else             // Not recognized
                begin
                    idx := idx;
                end;
                end;
            end;
            inc(idx);
        end;
    finally    // make sure we close the file
        CloseFile(F);
        Mem_Space.Free;     // Get rid of stream
    end;
end;
//******************************************************************************
// Writes the incomming String into the specified BytesStream
//******************************************************************************
procedure Write_String(Mem_Space: TBytesStream; const Content: String);
var
  // Str_Sz  : Integer;
    idx: Integer;
begin

{  Str_Sz  :=  length(Content)-1;
  For idx := 0 to Str_Sz do Mem_Space.WriteData(Content[idx+1]);}
{$IFNDEF FPC}
    for idx := 1 to length(Content) do
        Mem_Space.WriteData(Content[idx]);
{$ELSE}
    for idx := 1 to length(Content) do
        Mem_Space.Write(Content[idx], Length(Content[idx])); // TODO - verify AnsiString vs. unicode
{$ENDIF}
end;

end.
