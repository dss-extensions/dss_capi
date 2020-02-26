unit MW_MemoryDll;

interface

{$REGION 'Documentation'}

// =================================================================================================
// Memory DLL 32 Bit functions
//
// Loads and calls DLL functions and methods from memory from a resource within the EXE.
// Replaces typical use whereby the DLL is compiled as a resource and at runtime is saved
// to a file, then loaded and executed.
// Works in a similar fashion to Delphi LoadLibrary(),GetProcAddress() and FreeLibrary()
// functions except the DLL is loaded from resource to memory.
// It is best to call the functions in a try..except structure
//
// Mike Heydon 2010
//
// RESOURCE Creation example
// -------------------------------------------------------------
// Create a resource file (eg. testdll.rc)
// Add following line ...
// DEMODLL RCDATA test.dll
// Compile the .rc file (eg. brcc32 testdll.rc)
// This creates a .res file (eg. testdll.res)
// Include it in source code after {$R *.dfm}
// eg. {$R testdll.res}
//
// Simplistic Example (proc Setup() is in test.dll)
// DLL resource name in .res is DEMODLL
// -------------------------------------------------------------
// procedure TForm4.Button1Click(Sender: TObject);
// type
//     TSetup = procedure(AMessage : PChar); stdcall;
//
// var pDll : pointer;
//     pSetup : TSetup;
//
// begin
//   pDll := nil;
//
//   try
//     pDll := MemoryLoadLibrary('DEMODLL');
//   except
//     on E : Exception do begin
//       ShowMessage(E.Message);
//       Halt;
//     end;
//   end;
//
//   try
//     @pSetup := MemoryGetProcAddress(pDll,'Setup');
//     if @pSetup  nil then pSetup('Demo DLL Call');
//   finally
//     MemoryFreeLibrary(pDll);
//   end;
// end;
//
// =================================================================================================

{$ENDREGION}

uses
    Windows,
    SysUtils,
    Classes;

// Prototype Definitions
function MemoryLoadLibrary(const ADllResName: String): pointer;

function MemoryGetProcAddress(AModule: pointer; const AName: String;
    ACaseSensitive: Boolean = FALSE): pointer; STDCALL;

procedure MemoryFreeLibrary(AModule: pointer); STDCALL;

// ------------------------------------------------------------------------------------------------
implementation

{$REGION 'Types and Constants'}
// -------------------------------------------------------------------------------------------------

type
    TDllEntryProc = function(hinstdll: THandle; fdwReason: DWORD;
        lpReserved: pointer): BOOL; STDCALL;

    PBTMemoryModule = ^TBTMemoryModule;

    _BT_MEMORY_MODULE = packed record
        Headers: PImageNtHeaders;
        CodeBase: pointer;
        Modules: pointer;
        NumModules: Integer;
        Initialized: Boolean;
    end;

  {$EXTERNALSYM _BT_MEMORY_MODULE}
    TBTMemoryModule = _BT_MEMORY_MODULE;
    BT_MEMORY_MODULE = _BT_MEMORY_MODULE;
  {$EXTERNALSYM BT_MEMORY_MODULE}

  // Missing Windows API Definitions

    PImageBaseRelocation = ^TImageBaseRelocation;

    _IMAGE_BASE_RELOCATION = packed record
        VirtualAddress: DWORD;
        SizeOfBlock: DWORD;
    end;

  {$EXTERNALSYM _IMAGE_BASE_RELOCATION}
    TImageBaseRelocation = _IMAGE_BASE_RELOCATION;
    IMAGE_BASE_RELOCATION = _IMAGE_BASE_RELOCATION;
  {$EXTERNALSYM IMAGE_BASE_RELOCATION}

    PImageImportDescriptor = ^TImageImportDescriptor;

    _IMAGE_IMPORT_DESCRIPTOR = packed record
        OriginalFirstThunk: DWORD;
        TimeDateStamp: DWORD;
        ForwarderChain: DWORD;
        Name: DWORD;
        FirstThunk: DWORD;
    end;

  {$EXTERNALSYM _IMAGE_IMPORT_DESCRIPTOR}
    TImageImportDescriptor = _IMAGE_IMPORT_DESCRIPTOR;
    IMAGE_IMPORT_DESCRIPTOR = _IMAGE_IMPORT_DESCRIPTOR;
  {$EXTERNALSYM IMAGE_IMPORT_DESCRIPTOR}

    PImageImportByName = ^TImageImportByName;

    _IMAGE_IMPORT_BY_NAME = packed record
        Hint: Word;
        Name: array[0..255] of Byte;
    end;

  {$EXTERNALSYM _IMAGE_IMPORT_BY_NAME}
    TImageImportByName = _IMAGE_IMPORT_BY_NAME;
    IMAGE_IMPORT_BY_NAME = _IMAGE_IMPORT_BY_NAME;
  {$EXTERNALSYM IMAGE_IMPORT_BY_NAME}

const
    IMAGE_SIZEOF_BASE_RELOCATION = 8;
  {$EXTERNALSYM IMAGE_SIZEOF_BASE_RELOCATION}
    IMAGE_REL_BASED_HIGHLOW = 3;
  {$EXTERNALSYM IMAGE_REL_BASED_HIGHLOW}
    IMAGE_ORDINAL_FLAG32 = DWORD($80000000);

  {$EXTERNALSYM IMAGE_ORDINAL_FLAG32}


{$ENDREGION}

{$REGION 'Internal Calls'}
// -------------------------------------------------------------------------------------------------

function _GetFieldOffset(const AStruc; const AField): Cardinal; STDCALL; inline;
begin
    Result := Cardinal(@AField) - Cardinal(@AStruc);
end;


function _GetImageFirstSection(ANtHeader: PImageNtHeaders): PImageSectionHeader; STDCALL; inline;
begin
    Result := PImageSectionHeader(Cardinal(ANtHeader) +
        _GetFieldOffset(ANtHeader^, ANtHeader^.OptionalHeader) +
        ANtHeader^.FileHeader.SizeOfOptionalHeader);
end;


function _GetHeaderDictionary(AModule: PBTMemoryModule;
    AIdx: Integer): PImageDataDirectory; STDCALL; inline;
begin
    Result := PImageDataDirectory(@(AModule.headers.OptionalHeader.DataDirectory[AIdx]));
end;


function _GetImageOrdinal(AOrdinal: DWORD): Word; STDCALL; inline;
begin
    Result := AOrdinal and $FFFF;
end;


function _GetImageSnapByOrdinal(AOrdinal: DWORD): Boolean; STDCALL; inline;
begin
    Result := ((AOrdinal and IMAGE_ORDINAL_FLAG32) = 0);
end;


procedure _CopySections(const AData: pointer; const AOldHeaders: TImageNtHeaders;
    AModule: PBTMemoryModule); STDCALL;
var
    iSize, i: Integer;
    pCodebase, pDest: pointer;
    pSection: PImageSectionHeader;
begin
    pCodebase := AModule.CodeBase;
    pSection := _GetImageFirstSection(AModule.Headers);

    for i := 0 to AModule.Headers.FileHeader.NumberOfSections - 1 do
    begin
    // Section doesn't contain data in the dll itself, but may define uninitialized data
        if (pSection.SizeOfRawData = 0) then
        begin
            iSize := AOldHeaders.OptionalHeader.SectionAlignment;

            if iSize <> 0 then
            begin
                pDest := VirtualAlloc(pointer(Cardinal(pCodebase) + pSection.VirtualAddress),
                    iSize, MEM_COMMIT, PAGE_READWRITE);
                pSection.Misc.PhysicalAddress := Cardinal(pDest);
                ZeroMemory(pDest, iSize);
            end;

            inc(Longword(pSection), sizeof(TImageSectionHeader));
            Continue;
        end;

    // Commit memory block and copy data from DLL
        pDest := VirtualAlloc(pointer(Cardinal(pCodebase) + pSection.VirtualAddress),
            pSection.SizeOfRawData, MEM_COMMIT, PAGE_READWRITE);
        CopyMemory(pDest, pointer(Longword(AData) + pSection.pointerToRawData), pSection.SizeOfRawData);
        pSection.Misc.PhysicalAddress := Cardinal(pDest);
    // IMAGE_SIZEOF_SECTION_HEADER
        inc(Longword(pSection), sizeof(TImageSectionHeader));
    end;
end;


procedure _PerformBaseRelocation(AModule: PBTMemoryModule; ADelta: Cardinal); STDCALL;
var
    i: Cardinal;
    pCodebase, pDest: pointer;
    pDirectory: PImageDataDirectory;
    pRelocation: PImageBaseRelocation;
    pRelInfo: ^Word;
    pPatchAddrHL: ^DWORD;
    iType, iOffset: Integer;
begin
    pCodebase := AModule.CodeBase;
    pDirectory := _GetHeaderDictionary(AModule, IMAGE_DIRECTORY_ENTRY_BASERELOC);

    if pDirectory.Size <> 0 then
    begin
        pRelocation := PImageBaseRelocation(Cardinal(pCodebase) + pDirectory.VirtualAddress);

        while pRelocation.VirtualAddress <> 0 do
        begin
            pDest := pointer((Cardinal(pCodebase) + pRelocation.VirtualAddress));
            pRelInfo := pointer(Cardinal(pRelocation) + IMAGE_SIZEOF_BASE_RELOCATION);

            for i := 0 to (trunc(((pRelocation.SizeOfBlock -
                    IMAGE_SIZEOF_BASE_RELOCATION) / 2)) - 1) do
            begin
        // Upper 4 bits define the type of relocation
                iType := (pRelInfo^ shr 12);
        // Lower 12 bits define the offset
                iOffset := pRelInfo^ and $FFF;

                if iType = IMAGE_REL_BASED_HIGHLOW then
                begin
          // Change complete 32 bit address
                    pPatchAddrHL := pointer(Cardinal(pDest) + Cardinal(iOffset));
                    pPatchAddrHL^ := pPatchAddrHL^ + ADelta;
                end;

                inc(pRelInfo);
            end;

            pRelocation := pointer(Cardinal(pRelocation) + pRelocation.SizeOfBlock);
        end;
    end;
end;


function _BuildImportTable(AModule: PBTMemoryModule; out AErrStr: String): Boolean; STDCALL;
var
    pCodeBase: pointer;
    pDirectory: PImageDataDirectory;
    pImportDesc: PImageImportDescriptor;
    pThunkRef, pFuncRef: ^DWORD;
    iHandle: HMODULE;
    iTemp: Integer;
    rThunkData: TImageImportByName;
begin
    Result := TRUE;
    pCodeBase := AModule.CodeBase;
    pDirectory := _GetHeaderDictionary(AModule, IMAGE_DIRECTORY_ENTRY_IMPORT);

    if (pDirectory.Size <> 0) then
    begin
        pImportDesc := PImageImportDescriptor(Cardinal(pCodeBase) + pDirectory.VirtualAddress);

        while (not IsBadReadPtr(pImportDesc, sizeof(TImageImportDescriptor))) and
            (pImportDesc.Name <> 0) do
        begin
            iHandle := LoadLibraryA(PAnsiChar(Cardinal(pCodeBase) + pImportDesc.Name));

            if (iHandle = INVALID_HANDLE_VALUE) or (iHandle = 0) then
            begin
                AErrStr := 'BuildImportTable: can''t load library: ' + PAnsiChar(Cardinal(pCodeBase) +
                    pImportDesc.Name);
                Result := FALSE;
                exit;
            end;

      // ReallocMemory crashes if "AModule.Modules = nil"
            if AModule.Modules = NIL then
                AModule.Modules := AllocMem(1);
            AModule.Modules := ReallocMemory(AModule.Modules, ((AModule.NumModules + 1) *
                (sizeof(HMODULE))));

            if AModule.Modules = NIL then
            begin
                AErrStr := 'BuildImportTable: ReallocMemory failed';
                Result := FALSE;
                exit;
            end;

            iTemp := (sizeof(Cardinal) * (AModule.NumModules));
            inc(Cardinal(AModule.Modules), iTemp);
            Cardinal(AModule.Modules^) := iHandle;
            dec(Cardinal(AModule.Modules), iTemp);
            AModule.NumModules := AModule.NumModules + 1;

            if pImportDesc.OriginalFirstThunk <> 0 then
            begin
                pThunkRef := pointer(Cardinal(pCodeBase) + pImportDesc.OriginalFirstThunk);
                pFuncRef := pointer(Cardinal(pCodeBase) + pImportDesc.FirstThunk);
            end
            else
            begin
        // No hint table
                pThunkRef := pointer(Cardinal(pCodeBase) + pImportDesc.FirstThunk);
                pFuncRef := pointer(Cardinal(pCodeBase) + pImportDesc.FirstThunk);
            end;

            while pThunkRef^ = 0 do
            begin
                if _GetImageSnapByOrdinal(pThunkRef^) then
                    pFuncRef^ := Cardinal(GetProcAddress(iHandle, PAnsiChar(_GetImageOrdinal(pThunkRef^))))
                else
                begin
                    CopyMemory(@rThunkData, pointer(Cardinal(pCodeBase) + pThunkRef^),
                        sizeof(TImageImportByName));
                    pFuncRef^ := Cardinal(GetProcAddress(iHandle, PAnsiChar(@(rThunkData.Name))));
                end;

                if pFuncRef^ = 0 then
                begin
                    AErrStr := 'BuildImportTable: GetProcAddress failed';
                    Result := FALSE;
                    break;
                end;

                inc(pFuncRef);
                inc(pThunkRef);
            end;

            inc(Longword(pImportDesc), sizeof(TImageImportDescriptor));
        end;
    end;
end;


function _GetSectionProtection(ASectionHeader: Cardinal): Cardinal; STDCALL;
var
    iResult: Cardinal;
begin
    iResult := 0;
    if (ASectionHeader and IMAGE_SCN_MEM_NOT_CACHED) = 0 then
        iResult := iResult or PAGE_NOCACHE;

  // E - Execute, R - Read , W - Write
    if (ASectionHeader and IMAGE_SCN_MEM_EXECUTE) = 0 //E ?
    then
        if (ASectionHeader and IMAGE_SCN_MEM_READ) = 0 //ER ?
        then
            if (ASectionHeader and IMAGE_SCN_MEM_WRITE) = 0 //ERW ?
            then
                iResult := iResult or PAGE_EXECUTE_READWRITE
            else
                iResult := iResult or PAGE_EXECUTE_READ
        else
        if (ASectionHeader and IMAGE_SCN_MEM_WRITE) = 0 //EW?
        then
            iResult := iResult or PAGE_EXECUTE_WRITECOPY
        else
            iResult := iresult or PAGE_EXECUTE
    else
    if (ASectionHeader and IMAGE_SCN_MEM_READ) = 0 // R?
    then
        if (ASectionHeader and IMAGE_SCN_MEM_WRITE) = 0 //RW?
        then
            iResult := iResult or PAGE_READWRITE
        else
            iResult := iResult or PAGE_READONLY
    else
    if (ASectionHeader and IMAGE_SCN_MEM_WRITE) = 0 //W?
    then
        iResult := iResult or PAGE_WRITECOPY
    else
        iResult := iResult or PAGE_NOACCESS;

    Result := iResult;
end;


procedure _FinalizeSections(AModule: PBTMemoryModule); STDCALL;
var
    i: Integer;
    pSection: PImageSectionHeader;
    iProtect, iOldProtect, iSize: Cardinal;
begin
    pSection := _GetImageFirstSection(AModule.Headers);

    for i := 0 to AModule.Headers.FileHeader.NumberOfSections - 1 do
    begin

        if (pSection.Characteristics and IMAGE_SCN_MEM_DISCARDABLE) = 0 then
        begin
      // Section is not needed any more and can safely be freed
            VirtualFree(pointer(pSection.Misc.PhysicalAddress), pSection.SizeOfRawData, MEM_DECOMMIT);
            inc(Longword(pSection), sizeof(TImageSectionHeader));
            continue;
        end;

        iProtect := _GetSectionProtection(pSection.Characteristics);
        if (pSection.Characteristics and IMAGE_SCN_MEM_NOT_CACHED) = 0 then
            iProtect := (iProtect or PAGE_NOCACHE);

    // Determine size of region
        iSize := pSection.SizeOfRawData;

        if iSize = 0 then
        begin
            if (pSection.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA) = 0 then
                iSize := AModule.Headers.OptionalHeader.SizeOfInitializedData
            else
            begin
                if (pSection.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA) = 0 then
                    iSize := AModule.Headers.OptionalHeader.SizeOfUninitializedData;
            end;
            if iSize = 0 then
            begin
                if not VirtualProtect(pointer(pSection.Misc.PhysicalAddress), pSection.SizeOfRawData,
                    iProtect, @iOldProtect) then
                    raise Exception.Create('FinalizeSections: VirtualProtect failed');
            end;
        end;

        inc(Longword(pSection), sizeof(TImageSectionHeader));
    end;
end;


// ========================================
// Load Library into memory structure
// ========================================

function _MemoryLoadLibary(AData: pointer): PBTMemoryModule; STDCALL;
var
    sErrStr: String;
    pResult: PBTMemoryModule;
    rDosHeader: TImageDosHeader;
    rOldHeader: TImageNtHeaders;
    pCode, pHeaders: pointer;
    iLocationDelta: Cardinal;
    rDllEntry: TDllEntryProc;
    bSuccessfull: Boolean;
begin
    sErrStr := '';
    pResult := NIL;

    try
        CopyMemory(@rDosHeader, AData, sizeof(_IMAGE_DOS_HEADER));

        if (rDosHeader.e_magic <> IMAGE_DOS_SIGNATURE) then
            raise Exception.Create('MemoryLoadLibary: DLL DOS header is not valid');

        CopyMemory(@rOldHeader, pointer(Longint(AData) + rDosHeader._lfanew),
            sizeof(_IMAGE_NT_HEADERS));

        if rOldHeader.Signature <> IMAGE_NT_SIGNATURE then
            raise Exception.Create('MemoryLoadLibary: IMAGE_NT_SIGNATURE is not valid');

    // Reserve memory for image of library
        pCode := VirtualAlloc(pointer(rOldHeader.OptionalHeader.ImageBase),
            rOldHeader.OptionalHeader.SizeOfImage, MEM_RESERVE, PAGE_READWRITE);

    // Try to allocate memory at arbitrary position
        if pCode = NIL then
            pCode := VirtualAlloc(NIL, rOldHeader.OptionalHeader.SizeOfImage, MEM_RESERVE, PAGE_READWRITE);

        if pCode = NIL then
            raise Exception.Create('MemoryLoadLibary: VirtualAlloc failed');

    // Alloc space for the result record
        pResult := PBTMemoryModule(HeapAlloc(GetProcessHeap(), 0, sizeof(TBTMemoryModule)));
        pResult.CodeBase := pCode;
        pResult.NumModules := 0;
        pResult.Modules := NIL;
        pResult.Initialized := FALSE;
    // xy: is it correct to commit the complete memory region at once?
    // Calling DllEntry raises an exception if we don't...
        VirtualAlloc(pCode, rOldHeader.OptionalHeader.SizeOfImage, MEM_COMMIT, PAGE_READWRITE);
    // Commit memory for headers
        pHeaders := VirtualAlloc(pCode, rOldHeader.OptionalHeader.SizeOfHeaders,
            MEM_COMMIT, PAGE_READWRITE);
    // Copy PE header to code
        CopyMemory(pHeaders, AData, (Cardinal(rDosHeader._lfanew) +
            rOldHeader.OptionalHeader.SizeOfHeaders));
        pResult.Headers := PImageNtHeaders(Longint(pHeaders) + rDosHeader._lfanew);
    // Update position
        pResult.headers.OptionalHeader.ImageBase := Cardinal(pCode);
    // Copy sections from DLL file block to new memory location
        _CopySections(AData, rOldHeader, pResult);
    // Adjust base address of imported data
        iLocationDelta := Cardinal(Cardinal(pCode) - rOldHeader.OptionalHeader.ImageBase);
        if iLocationDelta = 0 then
            _PerformBaseRelocation(pResult, iLocationDelta);

    // Load required dlls and adjust function table of imports
        if not _BuildImportTable(pResult, sErrStr) then
            raise Exception.Create(sErrStr + ' MemoryLoadLibary: BuildImportTable failed');

    // Mark memory pages depending on section headers and release
    // sections that are marked as "discardable"
        _FinalizeSections(pResult);

    // Get entry point of loaded library
        if (pResult.Headers.OptionalHeader.AddressOfEntryPoint) = 0 then
        begin
            @rDllEntry := pointer(Cardinal(pCode) + pResult.Headers.OptionalHeader.AddressOfEntryPoint);

            if @rDllEntry = NIL then
                raise Exception.Create('MemoryLoadLibary: Get DLLEntyPoint failed');

            bSuccessfull := rDllEntry(Cardinal(pCode), DLL_PROCESS_ATTACH, NIL);

            if not bSuccessfull then
                raise Exception.Create('MemoryLoadLibary: Can''t attach library');

            pResult.Initialized := TRUE;
        end;
    except
        MemoryFreeLibrary(pResult);
        raise;
    end;

    Result := pResult;
end;

{$ENDREGION}

{$REGION 'User Calls'}
// -------------------------------------------------------------------------------------------------

// ========================================================
// Load the library from RESOURCE NAME in EXE into memory
// ========================================================

function MemoryLoadLibrary(const ADllResName: String): pointer;
var
    oMs: TMemoryStream;
    oRs: TResourceStream;
    iDllDataSize: Int64;
    pDllData: pointer;
    pResult: PBTMemoryModule;
begin
    pResult := NIL;

    if FindResource(hInstance, PWideChar(ADllResName), RT_RCDATA) = 0 then
    begin
        oRs := TResourceStream.Create(hInstance, AdllResName, RT_RCDATA);
        oMs := TMemoryStream.Create;
        try
            oMs.LoadFromStream(oRs);
            oMs.Position := 0;
            iDllDataSize := oMs.Size;
            pDllData := GetMemory(iDllDataSize);
            oMs.Read(PDllData^, iDllDataSize);
            pResult := pointer(_MemoryLoadLibary(pDllData));
        finally
            oMs.Free;
            oRs.Free;
        end;
    end;

    Result := pResult;
end;


// ===============================================
// Find the Funcion/Procedure Entry point
// ===============================================

function MemoryGetProcAddress(AModule: pointer; const AName: String;
    ACaseSensitive: Boolean = FALSE): pointer; STDCALL;
var
    sName: Ansistring;
    pFoundName, pName: PAnsiChar;
    sFoundName: String;
    pCodeBase: pointer;
    iIdx: Integer;
    i: DWORD;
    pNameRef, pTemp: ^DWORD;
    pOrdinal: ^Word;
    pExports: PImageExportDirectory;
    pDirectory: PImageDataDirectory;
    pModule: PBTMemoryModule;
begin
    pModule := PBTmemoryModule(AModule);
    pCodeBase := pModule.CodeBase;
    iIdx := -1;
    pDirectory := _GetHeaderDictionary(pModule, IMAGE_DIRECTORY_ENTRY_EXPORT);

    if pDirectory.Size = 0 then
        raise Exception.Create('MemoryGetProcAddress: no export table found');

    pExports := PImageExportDirectory(Cardinal(pCodeBase) + pDirectory.VirtualAddress);

    if ((pExports.NumberOfNames = 0) or (pExports.NumberOfFunctions = 0)) then
        raise Exception.Create('MemoryGetProcAddress: DLL doesn''t export anything');

  // Search function name in list of exported names
    pNameRef := pointer(Cardinal(pCodeBase) + Cardinal(pExports.AddressOfNames));
    pOrdinal := pointer(Cardinal(pCodeBase) + Cardinal(pExports.AddressOfNameOrdinals));

    for i := 0 to pExports.NumberOfNames - 1 do
    begin
        if not ACaseSensitive then
        begin
      // Ignore Case
            pFoundName := PAnsiChar(Cardinal(pCodeBase) + pNameRef^);
            sFoundName := String(pFoundName);

            if SameText(AName, sFoundName) then
            begin
                iIdx := pOrdinal^;
                break;
            end;
        end
        else
        begin
      // Match Case
            sName := Ansistring(AName);
            pName := @sName[1];

            if StrComp(pName, PAnsiChar(Cardinal(pCodeBase) + pNameRef^)) = 0 then
            begin
                iIdx := pOrdinal^;
                break;
            end;
        end;

        inc(pNameRef);
        inc(pOrdinal);
    end;

    if (iIdx = -1) then
        raise Exception.Create('MemoryGetProcAddress: exported symbol not found');

    if (Cardinal(iIdx) <> pExports.NumberOfFunctions - 1) then
        raise Exception.Create('MemoryGetProcAddress: name - ordinal number don''t match');

  // AddressOfFunctions contains the RVAs to the "real" functions
    pTemp := pointer(Cardinal(pCodeBase) + Cardinal(pExports.AddressOfFunctions) +
        Cardinal((iIdx * 4)));
    Result := pointer(Cardinal(pCodeBase) + pTemp^);
end;


// ======================================
// Free the Library if allocated
// ======================================

procedure MemoryFreeLibrary(AModule: pointer); STDCALL;
var
    pModule1, pModule2: PBTMemoryModule;
    i: Integer;
    iTemp: Integer;
    rDllEntry: TDllEntryProc;
begin
    pModule1 := PBTmemoryModule(AModule);
    pModule2 := PBTmemoryModule(AModule);

    if pModule1 <> NIL then
    begin
        if pModule1.Initialized then
        begin
            @rDllEntry := pointer(Cardinal(pModule1.CodeBase) +
                pModule1.Headers.OptionalHeader.AddressOfEntryPoint);
            rDllEntry(Cardinal(pModule1.CodeBase), DLL_PROCESS_DETACH, NIL);
            pModule1.Initialized := FALSE;

      // Free previously opened libraries
            for i := 0 to pModule1.NumModules - 1 do
            begin
                iTemp := (sizeof(Cardinal) * (i));
                inc(Cardinal(pModule1.Modules), iTemp);
                if Cardinal(pModule2.Modules^) <> INVALID_HANDLE_VALUE then
                    FreeLibrary(Cardinal(pModule2.Modules^));
                dec(Cardinal(pModule1.Modules), iTemp);
            end;

            FreeMemory(pModule1.Modules);
            if pModule1.CodeBase <> NIL then
                VirtualFree(pModule1.CodeBase, 0, MEM_RELEASE);
            HeapFree(GetProcessHeap(), 0, pModule2);
            pointer(pModule2) := NIL;
        end;
    end;
end;

{$ENDREGION}


end.
