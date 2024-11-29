unit CAPI_Text;

interface

uses
    CAPI_Utils,
    CAPI_Types;

function Text_Get_Command(): PAnsiChar; CDECL;
procedure Text_Set_Command(const Value: PAnsiChar); CDECL;
function Text_Get_Result(): PAnsiChar; CDECL;
procedure Text_CommandBlock(const Value: PAnsiChar); CDECL;
procedure Text_CommandArray(const Value: PPAnsiChar; ValueCount: TAPISize); CDECL;

implementation

uses
    CAPI_Constants,
    Classes,
    DSSGlobals,
    Executive,
    SysUtils,
    ExecHelper,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
function Text_Get_Command(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.LastCmdLine);
end;
//------------------------------------------------------------------------------
procedure Text_Set_Command(const Value: PAnsiChar); CDECL;
var
    res: String;
    posCurrent, posNext: Integer;
    full: String;
    s: String;
    i: Integer;
    outputs: TStringList;
begin
    i := 1;
    DSSPrime.SetSolutionAbort(FALSE);  // Reset for commands entered from outside
    full := Value;
    posCurrent := 1;
    posNext := Pos(#10, full, posCurrent);
    if posNext = 0 then
    begin
        // Nothing special, handle as the original implementation
        DSSPrime.DSSExecutive.ParseCommand(full);
        Exit;
    end;

    // Collect the strings and replace GlobalResult at the end
    outputs := TStringList.Create();
    try
        while posCurrent < Length(full) do
        begin
            s := Copy(full, posCurrent, (posNext - posCurrent));
            DSSPrime.DSSExecutive.ParseCommand(s, i);
            if Length(DSSPrime.GlobalResult) <> 0 then
                outputs.Add(DSSPrime.GlobalResult);
            
            if DSSPrime.ErrorNumber <> 0 then
            begin
                //TODO: complement error message?
                Exit;
            end;
            posCurrent := posNext + 1;
            posNext := Pos(#10, full, posCurrent);
            if posNext = 0 then
                posNext := Length(full) + 1;
            inc(i);
        end;
    finally
        if outputs <> NIL then
        begin
            DSSPrime.GlobalResult := outputs.GetText();
        end;
        FreeAndNil(outputs);
    end;
end;
//------------------------------------------------------------------------------
procedure Text_CommandBlock(const Value: PAnsiChar); CDECL;
var
    posCurrent, posNext: Integer;
    full: String;
    strs: TStringList;
begin
    DSSPrime.SetSolutionAbort(FALSE);  // Reset for commands entered from outside
    full := Value;
    posCurrent := 1;
    posNext := Pos(#10, full, posCurrent);
    if posNext = 0 then
    begin
        DSSPrime.DSSExecutive.ParseCommand(full);
        Exit;
    end;
    strs := TStringList.Create();
    strs.AddText(Value);
    DSSPrime.DSSExecutive.DoRedirect(false, strs); // DoRedirect will free the stringlist.
end;
//------------------------------------------------------------------------------
procedure Text_CommandArray(const Value: PPAnsiChar; ValueCount: TAPISize); CDECL;
var
    i: Integer;
    p: PPAnsiChar;
begin
    DSSPrime.SetSolutionAbort(FALSE);  // Reset for commands entered from outside
    p := Value;
    for i := 1 to ValueCount do
    begin
        DSSPrime.DSSExecutive.ParseCommand(p^, i);  // Convert to String
        inc(p);
        if DSSPrime.ErrorNumber <> 0 then
        begin
            //TODO: complement error message?
            Exit;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Text_Get_Result(): PAnsiChar; CDECL;
begin
    if Length(DSSPrime.GlobalResult) < 1 then
        Result := NIL
    else
        Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.GlobalResult);

    // Need to implement a protocol for determining whether to go get the
    // result from a file or to issue another DSS command to get the value
    // from operations where the result is voluminous.
end;
//------------------------------------------------------------------------------
end.
