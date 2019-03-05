unit MainUnit;

{ Definition of DLL interface functions for user-written Generator models in the DSS }

{Dynamics algorithm revised 7-18-04 to go to Thevinen equivalent}

interface

uses
    Ucomplex,
    Arraydef,
    Dynamics,
    IndMach012Model,
    GeneratorVars;


  {Imports Generator Variables Structures and DSS structures from Dynamics}

    {Note: everything is passed by reference (as a pointer), so it is possible to change the values in
     the structures (imported in Dynamics) in the main program.  This is dangerous so be careful.}

function New(var GenVars: TGeneratorVars; var DynaData: TDynamicsRec; var CallBacks: TDSSCallBacks): Integer; STDCALL; // Make a new instance
procedure Delete(var ID: Integer); STDCALL;  // deletes specified instance
function Select(var ID: Integer): Integer; STDCALL;    // Select active instance

procedure Init(V, I: pComplexArray); STDCALL;
                  {Initialize model.  Called when entering Dynamics mode.
                   V,I should contain results of most recent power flow solution.}
procedure Calc(V, I: pComplexArray); STDCALL;
                  {Main routine for performing the model calculations.  For "usermodel", this
                   function basically computes I given V.  For "shaftmodel", uses V and I
                   to calculate Pshaft, speed, etc. in dynamic data structures}
procedure Integrate; STDCALL; // Integrates any state vars
                  {Called to integrate state variables. User model is responsible for its own
                   integration. Check IterationFlag to determine if this
                   is a predictor or corrector step  }
procedure Edit(s: pAnsichar; Maxlen: Cardinal); STDCALL;
                  {called when DSS encounters user-supplied data string.  This module
                   is reponsible for interpreting whatever format this user-written modeli
                   is designed for.}
procedure UpdateModel; STDCALL;
                  {This is called when DSS needs to update the data that is computed
                   from user-supplied data forms.  }
procedure Save; STDCALL;
                  {Save the model to a file (of the programmer's choice) so that the state
                   data, if any can be restored for a  restart.}
procedure Restore; STDCALL;
                  {Reverse the Save command}

    {The user may return a number of double-precision values for monitoring}
function NumVars: Integer; STDCALL;   // Number of variables that can be returned for monitoring
procedure GetAllVars(Vars: pDoubleArray); STDCALL;
                  {Called by DSS monitoring elements.  Returns values of all monitoring variables in
                   an array of doubles.  The DSS will allocate "Vars" to the appropriate size.  This
                   routine will use Vars as a pointer to the array.}
function GetVariable(var i: Integer): Double; STDCALL;   // returns a i-th variable value  only
procedure SetVariable(var i: Integer; var value: Double); STDCALL;
                  {DSS allows users to set variables of user-written models directly.
                   Whatever variables that are exposed can be set if this routine handles it}
procedure GetVarName(var VarNum: Integer; VarName: pAnsiChar; maxlen: Cardinal); STDCALL;
                   {Returns name of a specific variable as a pointer to a string.
                    Set VarName= pointer to the first character in a null-terminated string}

implementation

uses
    Classes,
    Sysutils,
    ParserDel,
    Command,
    MathUtil;


// Keeping track of the models
var
    ModelList: Tlist;


    PropertyName: array[1..NumProperties] of String;

{-------------------------------------------------------------------------------------------------------------}
{DLL Interface Routines}
{-------------------------------------------------------------------------------------------------------------}

function New(var GenVars: TGeneratorVars; var DynaData: TDynamicsRec; var CallBacks: TDSSCallBacks): Integer; STDCALL;// Make a new instance
begin
    ActiveModel := TIndMach012Model.Create(GenVars, DynaData, CallBacks);
    Result := ModelList.Add(ActiveModel) + 1;
end;

procedure Delete(var ID: Integer); STDCALL;  // deletes specified instance
begin
    if ID <= ModelList.Count then
    begin
        ActiveModel := ModelList.Items[ID - 1];
        if ActiveModel <> NIL then
            ActiveModel.Free;
        ActiveModel := NIL;
        ModelList.Items[ID - 1] := NIL;
    end;
end;

procedure DisposeModels;
var
    i: Integer;
begin
    for i := 1 to ModelList.Count do
    begin
        ActiveModel := ModelList.Items[i - 1];
        ActiveModel.Free;
    end;
end;


function Select(var ID: Integer): Integer; STDCALL;    // Select active instance
begin
    Result := 0;
    if ID <= ModelList.Count then
    begin
        ActiveModel := ModelList.Items[ID - 1];
        if ActiveModel <> NIL then
            Result := ID;
    end;
end;


procedure Init(V, I: pComplexArray); STDCALL;   // For Dynamics Mode

var
    V012, I012: TSymCompArray;
begin

    if ActiveModel <> NIL then
    begin
        Phase2SymComp(V, @V012);    // Phase to Sym Components
        Phase2SymComp(I, @I012);

        ActiveModel.Init(V012, I012);
    end;

end;

procedure Calc(V, I: pComplexArray); STDCALL; // returns voltage or torque
{
 Perform calculations related to model
   Typically, Electrical model will compute I given V
   Machine dynamics may change andy of the GenVars (typically shaft power)
   You can change any of the variables, including the solution time variables, which could be dangerous.
   BE CAREFUL!
}

var
    V012, I012: TSymCompArray;


begin

    if ActiveModel <> NIL then
    begin

    // Convert abc voltages to 012
        Phase2SymComp(V, @V012);


    // compute I012
        with ActiveModel do
        begin
            case DynaData^.SolutionMode of
                DYNAMICMODE:
                begin
                    CalcDynamic(V012, I012);
                end;
            else  {All other modes are power flow modes}
            begin
                CalcPflow(V012, I012);
            end;
            end;
        end;

        SymComp2Phase(I, @I012);       // convert back to I abc

    end;

end;


procedure Integrate; STDCALL; // Integrates any Var vars
begin

    if ActiveModel <> NIL then
        ActiveModel.Integrate;

end;

procedure Save; STDCALL;
{Save the states to a disk file so that the solution can be restarted from this point in time}
begin

    if ActiveModel <> NIL then
    begin


    end;

end;

procedure Restore; STDCALL;
begin

    if ActiveModel <> NIL then
    begin


    end;

end;

procedure DefineProperties;

begin

     // Define Property names
    PropertyName[1] := 'Rs';
    PropertyName[2] := 'Xs';
    PropertyName[3] := 'Rr';
    PropertyName[4] := 'Xr';
    PropertyName[5] := 'Xm';
    PropertyName[6] := 'slip';
    PropertyName[7] := 'maxslip';
    PropertyName[8] := 'option';
    PropertyName[9] := 'help';

    CommandList := TCommandList.Create(Slice(PropertyName, NumProperties));
    CommandList.Abbrev := TRUE;
end;


procedure DisposeProperties;
var
    i: Integer;
begin
    for i := 1 to NumProperties do
        PropertyName[i] := '';
    CommandList.Free;
end;

procedure Edit(s: pAnsichar; Maxlen: Cardinal); STDCALL; // receive string from OpenDSS to handle

begin

    if ActiveModel <> NIL then
    begin
        ModelParser.CmdString := S;  // Load up Parser
        ActiveModel.Edit;     {Interpret string}
    end;

end;

procedure UpdateModel; STDCALL;

begin
    if ActiveModel <> NIL then
        ActiveModel.RecalcElementData;
end;


function NumVars: Integer; STDCALL;
{Return the number of Vars to be used for monitoring}
begin
    Result := NumVariables;
end;

procedure GetAllVars(Vars: pDoubleArray); STDCALL;
{Fill in the States array for monitoring}

{The Vars array has been allocated by the calling program}
var
    i, j: Integer;

begin

    try
        if Vars <> NIL then
        begin
            for i := 1 to NumVariables do
            begin
                j := i;
                Vars^[i] := GetVariable(j);
            end;
        end;
    except
        On E: Exception do
        begin
         {Oops, there has been an error, probably Vars not allocated right
          or we overflowed the array}
        end;
    end
end;

function GetVariable(var i: Integer): Double; STDCALL;   // get a particular variable

begin
    Result := ActiveModel.Variable[i];
end;

procedure SetVariable(var i: Integer; var value: Double); STDCALL;
begin
    ActiveModel.Variable[i] := Value;
end;


procedure GetVarName(var VarNum: Integer; VarName: pAnsichar; MaxLen: Cardinal);
{Return the name of a Var as a pointer to a null terminated string}
begin

    case VarNum of
        1:
            StrLCopy(VarName, 'Slip', Maxlen);  // Return a pointer to this string constant
        2:
            StrLCopy(VarName, 'puRs', maxlen);
        3:
            StrLCopy(VarName, 'puXs', maxlen);
        4:
            StrLCopy(VarName, 'puRr', maxlen);
        5:
            StrLCopy(VarName, 'puXr', maxlen);
        6:
            StrLCopy(VarName, 'puXm', maxlen);
        7:
            StrLCopy(VarName, 'MaxSlip', maxlen);
        8:
            StrLCopy(VarName, 'Is1', maxlen);
        9:
            StrLCopy(VarName, 'Is2', maxlen);
        10:
            StrLCopy(VarName, 'Ir1', maxlen);
        11:
            StrLCopy(VarName, 'Ir2', maxlen);
        12:
            StrLCopy(VarName, 'StatorLoss', maxlen);
        13:
            StrLCopy(VarName, 'RotorLoss', maxlen);
        14:
            StrLCopy(VarName, 'HPshaft', maxlen);
    else

    end;

end;


initialization

    ModelList := Tlist.Create;
    ModelParser := TParser.Create;
    DefineProperties;

finalization

    DisposeModels;
    ModelList.Free;
    ModelParser.Free;
    DisposeProperties;

end.
