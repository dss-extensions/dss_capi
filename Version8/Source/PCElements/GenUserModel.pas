unit GenUserModel;

{$M+}
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  7-7-10
  Modified to allow DLLS to retain ANSI string type in Edit function and Var names
  Nominally all strings passed to DLLS are ASCII to make it easier to write code in other languages
  and legacy defaults

}

interface

uses
    GeneratorVars,
    Dynamics,
    DSSCallBackRoutines,
    ucomplex,
    Arraydef;

type


    TGenUserModel = class(TObject)
    PRIVATE
        FHandle: NativeUint;  // Handle to DLL containing user model
        FID: Integer;    // ID of this instance of the user model
         // OK for this to be Wide String, since not passed to DLLs
        Fname: String;    // Name of the DLL file containing user model
        FuncError: Boolean;


         {These functions should only be called by the object itself}
        FNew:
        function(var GenVars: TGeneratorVars; var DynaData: TDynamicsRec; var CallBacks: TDSSCallBacks): Integer; STDCALL;// Make a new instance
        FDelete:
        procedure(var x: Integer); STDCALL;  // deletes specified instance
        FSelect:
        function(var x: Integer): Integer; STDCALL;    // Select active instance

        procedure Set_Name(const Value: String);
        function CheckFuncError(Addr: Pointer; FuncName: String): Pointer;

        procedure Set_Edit(const Value: String);
        function Get_Exists: Boolean;

    PROTECTED

    PUBLIC

        FEdit:
        procedure(s: pAnsiChar; Maxlen: Cardinal); STDCALL; // send string to user model to handle
        FInit:
        procedure(V, I: pComplexArray); STDCALL;   // For dynamics
        FCalc:
        procedure(V, I: pComplexArray); STDCALL; // returns Currents or sets Pshaft
        FIntegrate:
        procedure; STDCALL; // Integrates any state vars
        FUpdateModel:
        procedure; STDCALL; // Called when props of generator updated

        FActiveGeneratorVars: pTGeneratorVars;

        {Save and restore data}
        FSave:
        procedure; STDCALL;
        FRestore:
        procedure; STDCALL;

        {Monitoring functions}
        FNumVars:
        function: Integer; STDCALL;
        FGetAllVars:
        procedure(Vars: pDoubleArray); STDCALL;  // Get all vars
        FGetVariable:
        function(var I: Integer): Double; STDCALL;// Get a particular var
        FSetVariable:
        procedure(var i: Integer; var value: Double); STDCALL;
        FGetVarName:
        procedure(var VarNum: Integer; VarName: pAnsiChar; maxlen: Cardinal); STDCALL;

        // this property loads library (if needed), sets the procedure variables, and makes a new instance
        // old reference is freed first
        // Wide string OK here
        property Name: String READ Fname WRITE Set_Name;
        property Edit: String WRITE Set_Edit;  // Converted to Ansi string  in Set_Edit
        property Exists: Boolean READ Get_Exists;

        procedure Select;
        procedure Integrate;

        constructor Create(ActiveGeneratorVars: pTGeneratorVars);
        destructor Destroy; OVERRIDE;
    PUBLISHED

    end;


implementation

uses
    Generator,
    DSSGlobals,
  {$IFDEF FPC}
    dynlibs,
  {$ELSE}
    Windows,
  {$ENDIF}
    Sysutils;

{ TGenUserModel }

function TGenUserModel.CheckFuncError(Addr: Pointer; FuncName: String): Pointer;
begin
    if Addr = NIL then
    begin
        DoSimpleMsg('Generator User Model Does Not Have Required Function: ' + FuncName, 569);
        FuncError := TRUE;
    end;
    Result := Addr;
end;

constructor TGenUserModel.Create(ActiveGeneratorVars: pTGeneratorVars);
begin

    FID := 0;
    Fhandle := 0;
    FName := '';

    FActiveGeneratorVars := ActiveGeneratorVars;

end;

destructor TGenUserModel.Destroy;
begin

    if FID <> 0 then
    begin
        FDelete(FID);       // Clean up all memory associated with this instance
        FreeLibrary(FHandle);
    end;
    inherited;

end;

function TGenUserModel.Get_Exists: Boolean;
begin
    if FID <> 0 then
    begin
        Result := TRUE;
        Select;    {Automatically select if true}
    end
    else
        Result := FALSE;
end;

procedure TGenUserModel.Integrate;
begin
    FSelect(FID);
    Fintegrate;
end;

procedure TGenUserModel.Select;
begin
    Fselect(FID);
end;

procedure TGenUserModel.Set_Edit(const Value: String);
begin
    if FID <> 0 then
        FEdit(pAnsichar(Ansistring(Value)), Length(Value));
        // Else Ignore
end;

procedure TGenUserModel.Set_Name(const Value: String);


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
        DoSimpleMsg('Generator User Model ' + Value + ' Not Loaded. DSS Directory = ' + DSSDirectory, 570)
    else
    begin
        FName := Value;

            // Now set up all the procedure variables
        FuncError := FALSE;
        @Fnew := CheckFuncError(GetProcAddress(FHandle, 'New'), 'New');
        if not FuncError then
            @FSelect := CheckFuncError(GetProcAddress(FHandle, 'Select'), 'Select');
        if not FuncError then
            @FInit := CheckFuncError(GetProcAddress(FHandle, 'Init'), 'Init');
        if not FuncError then
            @FCalc := CheckFuncError(GetProcAddress(FHandle, 'Calc'), 'Calc');
        if not FuncError then
            @FIntegrate := CheckFuncError(GetProcAddress(FHandle, 'Integrate'), 'Integrate');
        if not FuncError then
            @FSave := CheckFuncError(GetProcAddress(FHandle, 'Save'), 'Save');
        if not FuncError then
            @FRestore := CheckFuncError(GetProcAddress(FHandle, 'Restore'), 'Restore');
        if not FuncError then
            @FEdit := CheckFuncError(GetProcAddress(FHandle, 'Edit'), 'Edit');
        if not FuncError then
            @FUpdateModel := CheckFuncError(GetProcAddress(FHandle, 'UpdateModel'), 'UpdateModel');
        if not FuncError then
            @FDelete := CheckFuncError(GetProcAddress(FHandle, 'Delete'), 'Delete');
        if not FuncError then
            @FNumVars := CheckFuncError(GetProcAddress(FHandle, 'NumVars'), 'NumVars');
        if not FuncError then
            @FGetAllVars := CheckFuncError(GetProcAddress(FHandle, 'GetAllVars'), 'GetAllVars');
        if not FuncError then
            @FGetVariable := CheckFuncError(GetProcAddress(FHandle, 'GetVariable'), 'GetVariable');
        if not FuncError then
            @FSetVariable := CheckFuncError(GetProcAddress(FHandle, 'SetVariable'), 'SetVariable');
        if not FuncError then
            @FGetVarName := CheckFuncError(GetProcAddress(FHandle, 'GetVarName'), 'GetVarName');

        if FuncError then
        begin
            FreeLibrary(FHandle);
            FID := 0;
            FHandle := 0;
            FName := '';
        end
        else
        begin
            FID := FNew(FActiveGeneratorVars^, ActiveCircuit[ActiveActor].Solution.Dynavars, CallBackRoutines);  // Create new instance of user model
        end;
        ;
    end;
end;


end.
