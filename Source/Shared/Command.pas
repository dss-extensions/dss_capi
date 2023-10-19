unit Command;
 {
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}


{$M+}

interface

Uses ArrayDef, Hashlist;

TYPE
   TCommandList = class(TObject)
      private
         CommandList:THashList;
         AbbrevAllowed:Boolean;
    function Get_NumCommands: Integer;
      protected

      public
        constructor Create(Commands: pStringArray; n:Integer); overload;
        constructor Create(Commands: Array of String); overload;
        destructor Destroy; override;
        Procedure AddCommand(const cmd:String);
        Function Getcommand(Const Cmd:String):Integer;
        Function CheckifValid():Boolean;
        Function Get(i:Integer):String;
        Property Abbrev:Boolean  read AbbrevAllowed write AbbrevAllowed;
        Property NumCommands:Integer Read Get_NumCommands;
        Property IsValidPtr:Boolean read CheckifValid;
      published

   end;


implementation

uses sysutils;

constructor TCommandList.Create(Commands:pStringArray; n:Integer);
VAR  i:Integer;
BEGIN
     Inherited Create;
     CommandList := THashList.Create(n);
     For i := 1 to n DO BEGIN
         CommandList.Add(Commands^[i]);
     END;
     
     AbbrevAllowed := True;
END;

constructor TCommandList.Create(Commands:Array of String);
VAR  i:Integer;
BEGIN
     Inherited Create;
     CommandList := THashList.Create(High(Commands)+1);
     For i := 0 to High(Commands) DO BEGIN
         CommandList.Add(Commands[i]);
     END;
     
     AbbrevAllowed := True;
END;

destructor TCommandList.Destroy;

BEGIN
     FreeAndNil (CommandList);
     Inherited Destroy;
END;

Procedure TCommandList.AddCommand(const cmd:String);
BEGIN
     CommandList.Add(Cmd);
END;

Function TCommandList.Getcommand(Const Cmd:String):Integer;

BEGIN

     Result := CommandList.Find(Cmd);
{If no match found on whole command, check for abbrev}
{This routine will generally be faster if one enters the whole command}
     If Result=0 THEN
       IF AbbrevAllowed THEN Result := CommandList.FindAbbrev(Cmd);

END;


function TCommandList.Get(i: Integer): String;
begin
     Result := CommandList.Get(i);
end;

function TCommandList.Get_NumCommands: Integer;
begin
     Result := CommandList.ListSize;
end;

Function TCommandList.CheckifValid():Boolean;
// Used to verify if the object is valid or has been cleared by someone else
// Practical when destroying common Lists while working in parallel
Begin
  Result  :=  CommandList.IsValidPtr;
End;

end.
