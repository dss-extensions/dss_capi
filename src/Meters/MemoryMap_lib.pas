// NOTES: 
// - Removed some unused functions for DSS-Extensions
// - The comments and names are misleading: this does NOT use memory mapping.  

// **************************Memory mapped files Library**************************
// * This library was added to OpenDSS to handle the different functions related  *
// * with mapping files into memory to accelerate the data manipulation process   *
// * when including this device to the simulation                                 *
// *                                                                              *
// * last modification: 09-12-2016                                                *
// ********************************************************************************

unit MemoryMap_lib;

interface

uses
    Classes,
    DSSClass;

function Create_Meter_Space(Init_Str: String): TBytesStream;
procedure WriteintoMemStr(Mem_Space: TBytesStream; Content: String);
procedure WriteintoMem(Mem_Space: TBytesStream; Content: Double);
procedure CloseMHandler(DSS: TDSSContext; var Mem_Space: TBytesStream; const Dest_Path: String; AppendFile: Boolean);
procedure Write_String(Mem_Space: TBytesStream; const Content: String);

implementation

uses
    sysutils,
    BufStream,
    math,
    DSSGlobals,
    Utilities,
    DSSHelper;


// $01A0 is Header for identifying String type data
// $02A0 is Header for identifying Double type data
//******************************************************************************
// Creates a new BytesStream for the caller
// Returns the handler to handle the new memory space
//******************************************************************************
function Create_Meter_Space(Init_Str: String): TBytesStream;
var
    Mem_Space: TBytesStream;
    wordBuf: Word;
begin
    Mem_Space := TBytesStream.Create();
    wordBuf := $01A0;
    Mem_Space.Write(wordBuf, 2);
    Write_String(Mem_Space, Init_Str);
    Result := Mem_Space;
end;
//******************************************************************************
// Writes a string into the specified BytesStream
//******************************************************************************
procedure WriteintoMemStr(Mem_Space: TBytesStream; Content: String);
var
    wordBuf: Word;
begin
    wordBuf := $01A0;
    Mem_Space.Write(wordBuf, 2);
    Write_String(Mem_Space, Content);
end;
//******************************************************************************
// Writes a DBL into the specified BytesStream
//******************************************************************************
procedure WriteintoMem(Mem_Space: TBytesStream; Content: Double);
var
    wordBuf: Word;
begin
    wordBuf := $02A0;
    Mem_Space.Write(wordBuf, 2);
    Mem_Space.Write(Content, sizeof(Double));
end;
//******************************************************************************
// Saves the content of the BytesStream into the specified file path
// and destroys the ByteStream
//******************************************************************************
procedure CloseMHandler(DSS: TDSSContext; var Mem_Space: TBytesStream; const Dest_Path: String; AppendFile: Boolean);
var
    F: TFileStream = nil;
    buffer: Uint8;
    idx: Integer;
    MWrite, Fhead: Boolean;
    MType: Integer;
    MSize: Longint;
    TVariableDbl: Double;
begin
    // Open Output file; check for errors
    try
        if AppendFile then
        begin
            F := TBufferedFileStream.Create(Dest_path, fmOpenReadWrite);
            F.Seek(0, soEnd);
        end
        else
        begin
            F := TBufferedFileStream.Create(Dest_path, fmCreate);
        end;
    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error Attempting to open file: "%s". %s', [Dest_path, E.Message], 159000);
            F.Free();
            FreeAndNil(Mem_Space);
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
                                fswriteln(F);
                                Fhead := TRUE;
                                inc(idx);
                            end
                            else
                            if (buffer > 0) then
                                fswrite(F, Char(buffer));
                        end
                        else
                        begin
                            idx := idx - 1;
                            MWrite := FALSE;
                        end;
                    end;
                    2:
                    begin        // Is a Double
                        Mem_Space.Read(TVariableDbl, sizeof(Double));
                        idx := idx + 7;
                        if Fhead then
                            Fhead := FALSE
                        else
                            fswrite(F, ', ');
                        fswrite(F, Format('%-g', [TVariableDbl]));
                        MWrite := FALSE;
                    end
                else             // Not recognized
                begin
                    // idx := idx;
                end;
                end;
            end;
            inc(idx);
        end;
    finally    // make sure we close the file
        FreeAndNil(F);
        FreeAndNil(Mem_Space);
    end;
end;

// Writes the incomming String into the specified BytesStream
procedure Write_String(Mem_Space: TBytesStream; const Content: String);
var
    idx: Integer;
begin
    for idx := 1 to length(Content) do
        Mem_Space.Write(Content[idx], Length(Content[idx])); // TODO - verify AnsiString vs. unicode

    Mem_Space.WriteByte(0);
end;

end.
