
unit DSSClass;
{
    ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
 Base Class for all DSS collection classes.
 Keeps track of objects of each class, dispatches edits, etc
}

interface

USES
    PointerList, Command,  Arraydef, {$IFDEF DSS_CAPI_HASHLIST}Contnrs{$ELSE}Hashlist{$ENDIF};

TYPE
   // Collection of all DSS Classes
    TDSSClasses = class(Tobject)
   private
     PROCEDURE Set_New(Value:Pointer);

   public
        constructor Create;
     destructor Destroy; override;

     Property New :pointer Write Set_New;

   End;

   // Base for all collection classes
    TDSSClass = class(TObject)
     private

          Procedure Set_Active(value:Integer);
        function Get_ElementCount: Integer;
        function Get_First: Integer;
        function Get_Next: Integer;

          Procedure ResynchElementNameList;

    PROTECTED
        Class_Name: String;
        ActiveElement: Integer;   // index of present ActiveElement
        CommandList: TCommandlist;
        ActiveProperty: Integer;
        ElementNameList:{$IFDEF DSS_CAPI_HASHLIST}TFPHashList;{$ELSE}THashList;{$ENDIF}


        function AddObjectToList(Obj: Pointer): Integer;  // Used by NewObject
        function Get_FirstPropertyName: String;
        function Get_NextPropertyName: String;
        function MakeLike(const ObjName: String): Integer; VIRTUAL;

        procedure CountProperties;  // Add no. of intrinsic properties
        procedure AllocatePropertyArrays;
        procedure DefineProperties;  // Add Properties of this class to propName
        function ClassEdit(const ActiveObj: Pointer; const ParamPointer: Integer): Integer;

    PUBLIC
        NumProperties: Integer;
        PropertyName,
        PropertyHelp: pStringArray;
        PropertyIdxMap,
        RevPropertyIdxMap: pIntegerArray;    // maps property to internal command number

        DSSClassType: Integer;


        ElementList: TPointerList;
        ElementNamesOutOfSynch: Boolean;     // When device gets renamed

        Saved: Boolean;

        constructor Create;
        destructor Destroy; OVERRIDE;

         {Helper routine for building Property strings}
        procedure AddProperty(const PropName: String; CmdMapIndex: Integer; const HelpString: String);
        procedure ReallocateElementNameList;

        function Edit(ActorID: Integer): Integer; VIRTUAL;      // uses global parser
        function Init(Handle: Integer; ActorID: Integer): Integer; VIRTUAL;
        function NewObject(const ObjName: String): Integer; VIRTUAL;

        function SetActive(const ObjName: String): Boolean; VIRTUAL;
        function GetActiveObj: Pointer; // Get address of active obj of this class
        function Find(const ObjName: String): Pointer; VIRTUAL;  // Find an obj of this class by name

        function PropertyIndex(const Prop: String): Integer;
        property FirstPropertyName: String READ Get_FirstPropertyName;
        property NextPropertyName: String READ Get_NextPropertyName;

        property Active: Integer READ ActiveElement WRITE Set_Active;
        property ElementCount: Integer READ Get_ElementCount;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Name: String READ Class_Name;
    end;
         
var
    DSSClasses: TDSSClasses;


implementation


USES DSSGlobals, SysUtils, DSSObject, ParserDel, CktElement;

{--------------------------------------------------------------}
{ DSSClasses Implementation
{--------------------------------------------------------------}
Constructor TDSSClasses.Create;

Begin
     Inherited Create;
End;

{--------------------------------------------------------------}
Destructor TDSSClasses.Destroy;
Begin
     Inherited Destroy;
End;

{--------------------------------------------------------------}
procedure TDSSClasses.Set_New(Value: Pointer);

begin
    DSSClassList[ActiveActor].New := Value; // Add to pointer list
    ActiveDSSClass[ActiveActor] := Value;   // Declare to be active
    ClassNames[ActiveActor].Add(ActiveDSSClass[ActiveActor].Name); // Add to classname list
end;

{--------------------------------------------------------------}
{  DSSClass Implementation
{--------------------------------------------------------------}
Constructor TDSSClass.Create;

BEGIN
    Inherited Create;
    ElementList := TPointerList.Create(20);  // Init size and increment
    PropertyName := nil;
    PropertyHelp := Nil;
    PropertyIdxMap  := Nil;
    RevPropertyIdxMap := Nil;

    ActiveElement := 0;
    ActiveProperty := 0;


    ElementNameList := {$IFDEF DSS_CAPI_HASHLIST}TFPHashList.Create();{$ELSE}THashList.Create(100);{$ENDIF}
    ElementNamesOutOfSynch := FALSE;

END;

{--------------------------------------------------------------}
Destructor TDSSClass.Destroy;

VAR
   i:INTEGER;

BEGIN
    // Get rid of space occupied by strings
    For i := 1 to NumProperties DO PropertyName[i] := '';
    For i := 1 to NumProperties DO PropertyHelp[i] := '';
    Reallocmem(PropertyName, 0);
    Reallocmem(PropertyHelp, 0);
    Reallocmem(PropertyIdxMap, 0);
    Reallocmem(RevPropertyIdxMap, 0);
    ElementList.Free;
    ElementNameList.Free;
    CommandList.Free;
    Inherited Destroy;
END;


{--------------------------------------------------------------}
Function TDSSClass.NewObject(const ObjName:String):Integer;
BEGIN
    Result := 0;
    DoErrorMsg('Reached base class of TDSSClass for device "' + ObjName + '"',
        'N/A',
        'Should be overridden.', 780);
END;

procedure TDSSClass.Set_Active(value: Integer);
begin
    if (Value > 0) and (Value <= ElementList.ListSize) then
    begin
        ActiveElement := Value;
        ActiveDSSObject[ActiveActor] := ElementList.Get(ActiveElement);
         // Make sure Active Ckt Element agrees if is a ckt element
         // So COM interface will work
        if ActiveDSSObject[ActiveActor] is TDSSCktElement then
            ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(ActiveDSSObject[ActiveActor]);
    end;
end;

function TDSSClass.Edit(ActorID: Integer): Integer;
begin
    Result := 0;
    DoSimpleMsg('virtual function TDSSClass.Edit called.  Should be overriden.', 781);
end;


function TDSSClass.Init(Handle: Integer; ActorID: Integer): Integer;
begin
    Result := 0;
    DoSimpleMsg('virtual function TDSSClass.Init called.  Should be overriden.', 782);
end;

Function TDSSClass.AddObjectToList(Obj:Pointer):Integer;
BEGIN
    ElementList.New := Obj; // Stuff it in this collection's element list
{$IFNDEF DSS_CAPI_HASHLIST}
    ElementNameList.Add(TDSSObject(Obj).Name);
    If Cardinal(ElementList.ListSize) > 2* ElementNameList.InitialAllocation Then ReallocateElementNameList;
{$ELSE}    
    ElementNameList.Add(LowerCase(TDSSObject(Obj).Name), Pointer(ElementList.ListSize));
{$ENDIF}
    ActiveElement := ElementList.ListSize;
    Result := ActiveElement; // Return index of object in list
END;

Function TDSSClass.SetActive(const ObjName:String):Boolean;
VAR
    idx: Integer;

BEGIN
    Result := False;
    // Faster to look in hash list 7/7/03
    If ElementNamesOutOfSynch Then ResynchElementNameList;
    {$IFDEF DSS_CAPI_HASHLIST}
    idx := LongInt(ElementNameList.Find(LowerCase(ObjName)));
    {$ELSE}
    idx := ElementNameList.Find(ObjName);
    {$ENDIF}
    
    If idx>0 Then
    Begin
        ActiveElement := idx;
        ActiveDSSObject[ActiveActor] := ElementList.get(idx);
        Result := TRUE;
    End;

END;

Function TDSSClass.Find(const ObjName:String):Pointer;
VAR
    idx: Integer;

BEGIN
    Result := Nil;
    If ElementNamesOutOfSynch Then ResynchElementNameList;
    // Faster to look in hash list 7/7/03
    {$IFDEF DSS_CAPI_HASHLIST}
    idx := LongInt(ElementNameList.Find(LowerCase(ObjName)));
    {$ELSE}
    idx := ElementNameList.Find(ObjName);
    {$ENDIF}
    
    If idx>0 Then
    Begin
        ActiveElement := idx;
        Result := ElementList.get(idx);
    End;
END;

function TDSSClass.GetActiveObj: Pointer; // Get address of active obj of this class
begin
    ActiveElement := ElementList.ActiveIndex;
    if ActiveElement > 0 then
        Result := ElementList.Get(ActiveElement)
    else
        Result := NIL;
end;

function TDSSClass.Get_FirstPropertyName: String;
begin
    ActiveProperty := 0;
    Result := Get_NextPropertyName;
end;

function TDSSClass.Get_NextPropertyName: String;
begin
    Inc(ActiveProperty);
    if ActiveProperty <= NumProperties then
        Result := PropertyName^[ActiveProperty]
    else
        Result := '';
end;

function TDSSClass.PropertyIndex(const Prop: String): Integer;
// find property value by string

var
    i: Integer;
begin

    Result := 0;  // Default result if not found
    for i := 1 to NumProperties do
    begin
        if CompareText(Prop, PropertyName[i]) = 0 then
        begin
            Result := PropertyIdxMap[i];
            Break;
        end;
    end;
end;

procedure TDSSClass.CountProperties;
begin
    NumProperties := NumProperties + 1;
end;

procedure TDSSClass.DefineProperties;
begin
    ActiveProperty := ActiveProperty + 1;
    PropertyName^[ActiveProperty] := 'like';
    PropertyHelp^[ActiveProperty] := 'Make like another object, e.g.:' + CRLF + CRLF +
        'New Capacitor.C2 like=c1  ...';
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDSSClass.ClassEdit(const ActiveObj: Pointer; const ParamPointer: Integer): Integer;


begin
  // continue parsing with contents of Parser

    Result := 0;
    if ParamPointer > 0 then
        with TDSSObject(ActiveObj) do
        begin

            case ParamPointer of
                1:
                    MakeLike(Parser[ActiveActor].StrValue);    // Like command (virtual)
            end;

        end;
end;

function TDSSClass.MakeLike(const ObjName: String): Integer;
begin
    Result := 0;
    DoSimpleMsg('virtual function TDSSClass.MakeLike called.  Should be overriden.', 784);
end;

function TDSSClass.Get_ElementCount: Integer;
begin
    Result := ElementList.ListSize;
end;

function TDSSClass.Get_First: Integer;
begin
    if ElementList.ListSize = 0 then
        Result := 0

    else
    begin
        ActiveElement := 1;
        ActiveDSSObject[ActiveActor] := ElementList.First;
      // Make sure Active Ckt Element agrees if is a ckt element
      // So COM interface will work
        if ActiveDSSObject[ActiveActor] is TDSSCktElement then
            ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(ActiveDSSObject[ActiveActor]);
        Result := ActiveElement;
    end;
end;

function TDSSClass.Get_Next: Integer;
begin
    Inc(ActiveElement);
    if ActiveElement > ElementList.ListSize then
        Result := 0
    else
    begin
        ActiveDSSObject[ActiveActor] := ElementList.Next;
      // Make sure Active Ckt Element agrees if is a ckt element
      // So COM interface will work
        if ActiveDSSObject[ActiveActor] is TDSSCktElement then
            ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(ActiveDSSObject[ActiveActor]);
        Result := ActiveElement;
    end;

end;

procedure TDSSClass.AddProperty(const PropName: String; CmdMapIndex: Integer; const HelpString: String);

begin
    Inc(ActiveProperty);
    PropertyName[ActiveProperty] := PropName;
    PropertyHelp[ActiveProperty] := HelpString;
    PropertyIdxMap[ActiveProperty] := CmdMapIndex;   // Maps to internal object property index
    RevPropertyIdxMap[CmdMapIndex] := ActiveProperty;
end;

procedure TDSSClass.AllocatePropertyArrays;
var
    i:Integer;
begin
    PropertyName := Allocmem(SizeOf(PropertyName^[1]) * NumProperties);
    PropertyHelp := Allocmem(SizeOf(PropertyHelp^[1]) * NumProperties);
    PropertyIdxMap := Allocmem(SizeOf(PropertyIdxMap^[1]) * NumProperties);
    RevPropertyIdxMap := Allocmem(SizeOf(RevPropertyIdxMap^[1]) * NumProperties);
    ActiveProperty := 0;    // initialize for AddPropert
     {initialize PropertyIdxMap to take care of legacy items}
    for i := 1 to NumProperties do
        PropertyIDXMap^[i] := i;
    for i := 1 to NumProperties do
        RevPropertyIDXMap^[i] := i;
end;

procedure TDSSClass.ReallocateElementNameList;
var
    i: Integer;

begin
  {Reallocate the device name list to improve the performance of searches}
    ElementNameList.Free; // Throw away the old one.
    ElementNameList := {$IFDEF DSS_CAPI_HASHLIST}TFPHashList.Create();{$ELSE}THashList.Create(2*ElementList.ListSize);{$ENDIF} // make a new one

    // Do this using the Names of the Elements rather than the old list because it might be
    // messed up if an element gets renamed

    {$IFDEF DSS_CAPI_HASHLIST}
    For i := 1 to ElementList.ListSize Do ElementNameList.Add(LowerCase(TDSSObject(ElementList.Get(i)).Name), Pointer(i));
    {$ELSE}
    For i := 1 to ElementList.ListSize Do ElementNameList.Add(TDSSObject(ElementList.Get(i)).Name);
    {$ENDIF}

end;

procedure TDSSClass.ResynchElementNameList;
begin

    ReallocateElementNameList;
    ElementNamesOutOfSynch := FALSE;

end;

end.
