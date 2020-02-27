unit CapUserControl;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

    Interface to user-written CapControl DLL
}


{$M+}
{
  ----------------------------------------------------------
  Copyright (c) 2012, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  3-31-12
  Converted from GenUserModel.Pas

}

interface

uses
    CapControlVars,
    Dynamics,
    DSSCallBackRoutines,
    ucomplex,
    Arraydef;

type


    TCapUserControl = class(TObject)
    PRIVATE
        FHandle: NativeUint;  // Handle to DLL containing user model
        FID: Integer;    // ID of this instance of the user model
         // OK for this to be Wide String, since not passed to DLLs
        Fname: String;    // Name of the DLL file containing user model
        FuncError: Boolean;


         {These functions should only be called by the object itself}
        FNew:
        function(var CallBacks: TDSSCallBacks): Integer; STDCALL;// Make a new instance
        FDelete:
        procedure(var x: Integer); STDCALL;  // deletes specified instance
        FSelect:
        function(var x: Integer): Integer; STDCALL;    // Select active instance
        FUpdateModel:
        procedure; STDCALL; // Called when props of CapControl updated
        FSample:
        procedure; STDCALL;
        FDoPending:
        procedure(var Code, ProxyHdl: Integer); STDCALL;

        procedure Set_Name(const Value: String);
        function CheckFuncError(Addr: Pointer; FuncName: String): Pointer;
        procedure Set_Edit(const Value: String);
        function Get_Exists: Boolean;

    PROTECTED

    PUBLIC

        FEdit:
        procedure(s: pAnsichar; Maxlen: Cardinal); STDCALL; // send string to user model to handle

        procedure Select;
        procedure UpdateModel;
        procedure DoPending(const Code, ProxyHdl: Integer);
        procedure Sample;

        constructor Create;
        destructor Destroy; OVERRIDE;

        // this property loads library (if needed), sets the procedure variables, and makes a new instance
        // old reference is freed first
        // Wide string OK here
        property Name: String READ Fname WRITE Set_Name;
        property Edit: String WRITE Set_Edit;
        property Exists: Boolean READ Get_Exists;
    PUBLISHED

    end;


implementation

uses
    DSSGlobals,
  {$IFDEF FPC}
    dynlibs,
  {$ELSE}
    Windows,
  {$ENDIF}
    Sysutils;

{ TCapUserControl }

function TCapUserControl.CheckFuncError(Addr: Pointer; FuncName: String): Pointer;
begin
    if Addr = NIL then
    begin
        DoSimpleMsg('CapControl User Model Does Not Have Required Function: ' + FuncName, 569);
        FuncError := TRUE;
    end;
    Result := Addr;
end;

constructor TCapUserControl.Create;
begin

    FID := 0;
    Fhandle := 0;
    FName := '';

end;

destructor TCapUserControl.Destroy;
begin

    try
        if FID <> 0 then
            FDelete(FID);  // Clean up all memory associated with this instance
    finally
        if Fhandle <> 0 then
            FreeLibrary(FHandle);
    end;


    inherited;

end;

procedure TCapUserControl.DoPending(const Code, ProxyHdl: Integer);

// do the pending control Action
var
    varCode, varProxyHdl: Integer;
begin
    if FID <> 0 then
    begin
        varCode := Code; // Can't pass a const
        varProxyHdl := ProxyHdl;
        FDoPending(varCode, varProxyHdl);
    end;
end;

function TCapUserControl.Get_Exists: Boolean;
begin
    if FID <> 0 then
    begin
        Result := TRUE;
        Select;    {Automatically select if true}
    end
    else
        Result := FALSE;
end;


procedure TCapUserControl.Sample;
// Sample the cap control

begin

    if FID <> 0 then
        FSample;

end;

procedure TCapUserControl.Select;
begin
    Fselect(FID);
end;


procedure TCapUserControl.Set_Edit(const Value: String);
begin
    if FID <> 0 then
        FEdit(pAnsichar(Ansistring(Value)), Length(Value));
end;

procedure TCapUserControl.Set_Name(const Value: String);

begin

    {If Model already points to something, then free it}

    if FHandle <> 0 then
    begin
        if FID <> 0 then
        begin
            FDelete(FID);
            FName := '';
            FID := 0;
        end;
        FreeLibrary(FHandle);
    end;

        {If Value is blank or zero-length, bail out.}
    if (Length(Value) = 0) or (Length(TrimLeft(Value)) = 0) then
        Exit;
    if comparetext(value, 'none') = 0 then
        Exit;

    FHandle := LoadLibrary(Pchar(Value));   // Default LoadLibrary and PChar must agree in expected type
    if FHandle = 0 then
    begin // Try again with full path name
        FHandle := LoadLibrary(Pchar(DSSDirectory + Value));
    end;

    if FHandle = 0 then
        DoSimpleMsg('CapControl User Model ' + Value + ' Load Library Failed. DSS Directory = ' + DSSDirectory, 570)
    else
    begin
        FName := Value;

            // Now set up all the procedure variables
        FuncError := FALSE;
        @Fnew := CheckFuncError(GetProcAddress(FHandle, 'New'), 'New');
        if not FuncError then
            @FSelect := CheckFuncError(GetProcAddress(FHandle, 'Select'), 'Select');
        if not FuncError then
            @FSample := CheckFuncError(GetProcAddress(FHandle, 'Sample'), 'Sample');
        if not FuncError then
            @FDoPending := CheckFuncError(GetProcAddress(FHandle, 'DoPending'), 'DoPending');
        if not FuncError then
            @FEdit := CheckFuncError(GetProcAddress(FHandle, 'Edit'), 'Edit');
        if not FuncError then
            @FUpdateModel := CheckFuncError(GetProcAddress(FHandle, 'UpdateModel'), 'UpdateModel');
        if not FuncError then
            @FDelete := CheckFuncError(GetProcAddress(FHandle, 'Delete'), 'Delete');

        if FuncError then
        begin
            if not FreeLibrary(FHandle) then
                DoSimpleMsg('Error Freeing DLL: ' + Fname, 10570);  // decrement the reference count
            FID := 0;
            FHandle := 0;
            FName := '';
        end
        else
        begin
            FID := FNew(CallBackRoutines);  // Create new instance of user model
        end;
        ;
    end;
end;


procedure TCapUserControl.UpdateModel;
begin
    if FID <> 0 then
        FUpdateModel;

end;

end.
