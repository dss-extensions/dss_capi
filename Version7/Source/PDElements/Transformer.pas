unit Transformer;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change log
   1-28-00 Added tap properties so that regulator can control it.
   1-29-00 Added GetWindingVoltages
    2-1-00 Replaced TranParser with global AuxParser
    2-9-00 Fixed Set_PresentTap bug
   1-23-03 Added code to get 30 deg lag correct of y-delta transformers
   2-18-03 changed Rneut default to open (-1)
   2-21-03 changed automatic resetting of connection designator upon changing Rneut
   9-12-11 Fixed pctLoadLoss problem with sequence of definition with kVA property
}

{ You can designate a transformer to be a substation by setting the sub=yes parameter}


interface

uses
    Command,
    DSSClass,
    PDClass,
    Circuit,
    PDElement,
    uComplex,
    UcMatrix,
    ParserDel,
    Arraydef,
    math;

type

    TTransf = class(TPDClass)

    PRIVATE

        procedure SetActiveWinding(w: Integer);
        procedure InterpretConnection(const S: String);
        procedure InterpretAllConns(const S: String);
        procedure InterpretAllBuses(const S: String);
        procedure InterpretAllTaps(const S: String);
        procedure InterpretAllkVRatings(const S: String);
        procedure InterpretAllkVARatings(const S: String);
        procedure InterpretAllRs(const S: String);
        function TrapZero(const Value: Double; DefaultValue: Double): Double;
        function InterpretLeadLag(const S: String): Boolean;

       {PROCEDURE MakeNewBusNameForNeutral(Var NewBusName:String; Nphases:Integer);}
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const TransfName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

    TWinding = class(Tobject)
    PUBLIC
        Connection: Integer;
        kVLL,
        VBase,
        kVA,
        puTap,
        Rpu,      // on transformer MVABase  (1st winding)
        Rdcpu,    // for GIC solutions; default to 85% of Rpu
        RdcOhms,
        Rneut,
        Xneut: Double;
        Y_PPM: Double;  // Anti Float reactance adder
        RdcSpecified: Boolean;

        {Tap Changer Data}
        TapIncrement,
        MinTap,
        MaxTap: Double;
        NumTaps: Integer;

        procedure ComputeAntiFloatAdder(PPM_Factor, VABase1ph: Double);

        constructor Create;
        destructor Destroy; OVERRIDE;
    end;

    WindingArray = array[1..3] of TWinding;
    pWindingArray = ^WindingArray;

    TTransfObj = class(TPDElement)
    PRIVATE

        DeltaDirection: Integer;
        ppm_FloatFactor: Double; //  parts per million winding float factor
        pctImag: Double;
        XRConst: Boolean;

        function Get_PresentTap(i: Integer): Double;
        procedure Set_PresentTap(i: Integer; const Value: Double);
        function Get_MinTap(i: Integer): Double;
        function Get_MaxTap(i: Integer): Double;
        function Get_TapIncrement(i: Integer): Double;
        function Get_BaseVoltage(i: Integer): Double;
        function Get_BasekVLL(i: Integer): Double;
        // CIM accessors
        function Get_NumTaps(i: Integer): Integer;
        function Get_WdgResistance(i: Integer): Double;
        function Get_WdgRdc(i: Integer): Double;
        function Get_WdgConnection(i: Integer): Integer;
        function Get_WdgkVA(i: Integer): Double;
        function Get_Xsc(i: Integer): Double;
        function Get_WdgRneutral(i: Integer): Double;
        function Get_WdgXneutral(i: Integer): Double;
        function Get_WdgYPPM(i: Integer): Double;

        procedure CalcY_Terminal(FreqMult: Double);
        procedure GICBuildYTerminal;

        procedure BuildYPrimComponent(YPrim_Component, Y_Terminal: TCMatrix);
        procedure AddNeutralToY(FreqMultiplier: Double);

        procedure FetchXfmrCode(const Code: String);


    PROTECTED
        NumWindings: Integer;
        MaxWindings: Integer;
        TermRef: pIntegerArray;  // keeps track of terminal connections

        XHL, XHT, XLT: Double;  // per unit
        Zbase: Double;
        XSC: pDoubleArray;     // per unit SC measurements
        VABase: Double;    // FOR impedances

        ZB: TCMatrix;
        Y_1Volt: TCMatrix;
        Y_Term: TCMatrix;
        Y_1Volt_NL: TCMatrix;   // No Load Y's
        Y_Term_NL: TCMatrix;

        Y_Terminal_Freqmult: Double;

        NormMaxHkVA: Double;
        EmergMaxHkVA: Double;
        ThermalTimeConst: Double;  {hr}
        n_thermal: Double;
        m_thermal: Double;  {Exponents}
        FLrise: Double;
        HSrise: Double;
        pctLoadLoss: Double;
        pctNoLoadLoss: Double;

        HVLeadsLV: Boolean;

        XHLChanged: Boolean;

        procedure SetTermRef;
    PUBLIC
        ActiveWinding: Integer;  // public for COM interface

        IsSubstation: Boolean;
        SubstationName: String;
        Winding: pWindingArray;
        XfmrBank: String;
        XfmrCode: String;
        CoreType: Integer; {0=Shell; 1=1ph; 3-3leg; 5=5-leg}
        strCoreType: String;

        constructor Create(ParClass: TDSSClass; const TransfName: String);
        destructor Destroy; OVERRIDE;

        procedure SetNumWindings(N: Integer);

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        {GetLosses override for Transformer}
        procedure GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex); OVERRIDE;

        function RotatePhases(iPhs: Integer): Integer;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        procedure SaveWrite(var F: TextFile); OVERRIDE;
        procedure GetWindingVoltages(iWind: Integer; VBuffer: pComplexArray);
        procedure GetAllWindingCurrents(CurrBuffer: pComplexArray);  // All Winding currents in complex array
        function GetWindingCurrentsResult: String;  // All winding currents in string


        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model

        property PresentTap[i: Integer]: Double READ Get_PresentTap WRITE Set_PresentTap;
        property Mintap[i: Integer]: Double READ Get_MinTap;
        property Maxtap[i: Integer]: Double READ Get_MaxTap;
        property TapIncrement[i: Integer]: Double READ Get_TapIncrement;
        property BaseVoltage[i: Integer]: Double READ Get_BaseVoltage;  // Winding VBase
        property BasekVLL[i: Integer]: Double READ Get_BasekVLL;  // Winding VBase

        // CIM accessors
        property NumTaps[i: Integer]: Integer READ Get_NumTaps;
        property NumberOfWindings: Integer READ NumWindings;
        property WdgResistance[i: Integer]: Double READ Get_WdgResistance;
        property WdgRdc[i: Integer]: Double READ Get_WdgRdc;
        property WdgkVA[i: Integer]: Double READ Get_WdgkVA;
        property WdgConnection[i: Integer]: Integer READ Get_WdgConnection;
        property WdgRneutral[i: Integer]: Double READ Get_WdgRneutral;
        property WdgXneutral[i: Integer]: Double READ Get_WdgXneutral;
        property WdgYPPM[i: Integer]: Double READ Get_WdgYPPM;
        property XscVal[i: Integer]: Double READ Get_Xsc;
        property XhlVal: Double READ Xhl;
        property XhtVal: Double READ Xht;
        property XltVal: Double READ Xlt;
        property NormalHkVA: Double READ NormMaxHkVA;
        property EmergHkVA: Double READ EmergMaxHkVA;
        property thTau: Double READ ThermalTimeConst;
        property thN: Double READ n_thermal;
        property thM: Double READ m_thermal;
        property thFLRise: Double READ FLRise;
        property thHSRise: Double READ HSRise;
        property loadLossPct: Double READ pctLoadLoss;
        property noLoadLossPct: Double READ pctNoLoadLoss;
        property imagPct: Double READ pctImag;
        property ppmFloatFac: Double READ ppm_FloatFactor;
        property baseVA: Double READ VAbase;
    end;

var
    ActiveTransfObj: TTransfObj;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
implementation

{$DEFINE NOTRANSDEBUG}  {TRANSDEBUG}

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Utilities,
    XfmrCode;

var
    XfmrCodeClass: TXfmrCode;

const
    NumPropsThisClass = 47;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TTransf.Create;  // Creates superstructure for all Transformer objects
begin
    inherited Create;
    Class_Name := 'Transformer';
    DSSClassType := DSSClassType + XFMR_ELEMENT; // override PDElement   (kept in both actually)

    ActiveElement := 0;
    XfmrCodeClass := NIL;

    DefineProperties;

     {Make space for transformer property list}
    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;     {Allow property list abbreviations}

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TTransf.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTransf.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

{ Define Property names  }

    PropertyName[1] := 'phases';
    PropertyName[2] := 'windings';

   // Winding Definition
    PropertyName[3] := 'wdg';
    PropertyName[4] := 'bus';
    PropertyName[5] := 'conn';
    PropertyName[6] := 'kV'; // FOR 2-and 3- always kVLL ELSE actual winding KV
    PropertyName[7] := 'kVA';
    PropertyName[8] := 'tap';
    PropertyName[9] := '%R';
    PropertyName[10] := 'Rneut';
    PropertyName[11] := 'Xneut';

   // General Data
    PropertyName[12] := 'buses';
    PropertyName[13] := 'conns';
    PropertyName[14] := 'kVs';
    PropertyName[15] := 'kVAs';
    PropertyName[16] := 'taps';
    PropertyName[17] := 'XHL';
    PropertyName[18] := 'XHT';
    PropertyName[19] := 'XLT';
    PropertyName[20] := 'Xscarray';  // x12 13 14... 23 24.. 34 ..
    PropertyName[21] := 'thermal';
    PropertyName[22] := 'n';
    PropertyName[23] := 'm';
    PropertyName[24] := 'flrise';
    PropertyName[25] := 'hsrise';
    PropertyName[26] := '%loadloss';
    PropertyName[27] := '%noloadloss';
    PropertyName[28] := 'normhkVA';
    PropertyName[29] := 'emerghkVA';
    PropertyName[30] := 'sub';  // =y/n
    PropertyName[31] := 'MaxTap';
    PropertyName[32] := 'MinTap';
    PropertyName[33] := 'NumTaps';
    PropertyName[34] := 'subname';
    PropertyName[35] := '%imag';
    PropertyName[36] := 'ppm_antifloat';
    PropertyName[37] := '%Rs';

    PropertyName[38] := 'bank';
    PropertyName[39] := 'XfmrCode';
    PropertyName[40] := 'XRConst';
    PropertyName[41] := 'X12';
    PropertyName[42] := 'X13';
    PropertyName[43] := 'X23';
    PropertyName[44] := 'LeadLag';
    PropertyName[45] := 'WdgCurrents';
    PropertyName[46] := 'Core';
    PropertyName[47] := 'RdcOhms';


     // define Property help values
    PropertyHelp[1] := 'Number of phases this transformer. Default is 3.';
    PropertyHelp[2] := 'Number of windings, this transformers. (Also is the number of terminals) ' +
        'Default is 2. This property triggers memory allocation for the Transformer and will cause other properties to revert to default values.';
   // Winding Definition
    PropertyHelp[3] := 'Set this = to the number of the winding you wish to define.  Then set ' +
        'the values for this winding.  Repeat for each winding.  Alternatively, use ' +
        'the array collections (buses, kVAs, etc.) to define the windings.  Note: ' +
        'reactances are BETWEEN pairs of windings; they are not the property of a single winding.';
    PropertyHelp[4] := 'Bus connection spec for this winding.';
    PropertyHelp[5] := 'Connection of this winding {wye*, Delta, LN, LL}. Default is "wye" with the neutral solidly grounded. ';
    PropertyHelp[6] := 'For 2-or 3-phase, enter phase-phase kV rating.  Otherwise, kV rating of the actual winding';
    PropertyHelp[7] := 'Base kVA rating of the winding. Side effect: forces change of max normal and emerg kVA ratings.' +
        'If 2-winding transformer, forces other winding to same value. ' +
        'When winding 1 is defined, all other windings are defaulted to the same rating ' +
        'and the first two winding resistances are defaulted to the %loadloss value.';
    PropertyHelp[8] := 'Per unit tap that this winding is on.';
    PropertyHelp[9] := 'Percent resistance this winding.  (half of total for a 2-winding).';
    PropertyHelp[10] := 'Default = -1. Neutral resistance of wye (star)-connected winding in actual ohms. ' +
        'If entered as a negative value, the neutral is assumed to be open, or floating. ' +
        'To solidly ground the neutral, connect the neutral conductor to Node 0 in the Bus property spec for this winding. ' +
        'For example: Bus=MyBusName.1.2.3.0, which is generally the default connection.';
    PropertyHelp[11] := 'Neutral reactance of wye(star)-connected winding in actual ohms.  May be + or -.';

   // General Data
    PropertyHelp[12] := 'Use this to specify all the bus connections at once using an array. Example:' + CRLF + CRLF +
        'New Transformer.T1 buses="Hibus, lowbus"';
    PropertyHelp[13] := 'Use this to specify all the Winding connections at once using an array. Example:' + CRLF + CRLF +
        'New Transformer.T1 buses="Hibus, lowbus" ' +
        '~ conns=(delta, wye)';
    PropertyHelp[14] := 'Use this to specify the kV ratings of all windings at once using an array. Example:' + CRLF + CRLF +
        'New Transformer.T1 buses="Hibus, lowbus" ' + CRLF +
        '~ conns=(delta, wye)' + CRLF +
        '~ kvs=(115, 12.47)' + CRLF + CRLF +
        'See kV= property for voltage rules.';
    PropertyHelp[15] := 'Use this to specify the kVA ratings of all windings at once using an array.';
    PropertyHelp[16] := 'Use this to specify the p.u. tap of all windings at once using an array.';
    PropertyHelp[17] := 'Use this to specify the percent reactance, H-L (winding 1 to winding 2).  Use ' +
        'for 2- or 3-winding transformers. On the kVA base of winding 1. See also X12.';
    PropertyHelp[18] := 'Use this to specify the percent reactance, H-T (winding 1 to winding 3).  Use ' +
        'for 3-winding transformers only. On the kVA base of winding 1. See also X13.';
    PropertyHelp[19] := 'Use this to specify the percent reactance, L-T (winding 2 to winding 3).  Use ' +
        'for 3-winding transformers only. On the kVA base of winding 1.  See also X23.';
    PropertyHelp[20] := 'Use this to specify the percent reactance between all pairs of windings as an array. ' +
        'All values are on the kVA base of winding 1.  The order of the values is as follows:' + CRLF + CRLF +
        '(x12 13 14... 23 24.. 34 ..)  ' + CRLF + CRLF +
        'There will be n(n-1)/2 values, where n=number of windings.';
    PropertyHelp[21] := 'Thermal time constant of the transformer in hours.  Typically about 2.';
    PropertyHelp[22] := 'n Exponent for thermal properties in IEEE C57.  Typically 0.8.';
    PropertyHelp[23] := 'm Exponent for thermal properties in IEEE C57.  Typically 0.9 - 1.0';
    PropertyHelp[24] := 'Temperature rise, deg C, for full load.  Default is 65.';
    PropertyHelp[25] := 'Hot spot temperature rise, deg C.  Default is 15.';
    PropertyHelp[26] := 'Percent load loss at full load. The %R of the High and Low windings (1 and 2) are adjusted to agree at rated kVA loading.';
    PropertyHelp[27] := 'Percent no load losses at rated excitatation voltage. Default is 0. Converts to a resistance in parallel with the magnetizing impedance in each winding.';
    PropertyHelp[28] := 'Normal maximum kVA rating of H winding (winding 1).  Usually 100% - 110% of' +
        'maximum nameplate rating, depending on load shape. Defaults to 110% of kVA rating of Winding 1.';
    PropertyHelp[29] := 'Emergency (contingency)  kVA rating of H winding (winding 1).  Usually 140% - 150% of' +
        'maximum nameplate rating, depending on load shape. Defaults to 150% of kVA rating of Winding 1.';
    PropertyHelp[30] := '={Yes|No}  Designates whether this transformer is to be considered a substation.' +
        'Default is No.';  // =y/n

    PropertyHelp[31] := 'Max per unit tap for the active winding.  Default is 1.10';
    PropertyHelp[32] := 'Min per unit tap for the active winding.  Default is 0.90';
    PropertyHelp[33] := 'Total number of taps between min and max tap.  Default is 32 (16 raise and 16 lower taps about the neutral position). The neutral position is not counted.';
    PropertyHelp[34] := 'Substation Name. Optional. Default is null. If specified, printed on plots';
    PropertyHelp[35] := 'Percent magnetizing current. Default=0.0. Magnetizing branch is in parallel with windings in each phase. Also, see "ppm_antifloat".';
    PropertyHelp[36] := 'Default=1 ppm.  Parts per million of transformer winding VA rating connected to ground to protect against accidentally floating a winding without a reference. ' +
        'If positive then the effect is adding a very large reactance to ground.  If negative, then a capacitor.';
    PropertyHelp[37] := 'Use this property to specify all the winding %resistances using an array. Example:' + CRLF + CRLF +
        'New Transformer.T1 buses="Hibus, lowbus" ' +
        '~ %Rs=(0.2  0.3)';
    PropertyHelp[38] := 'Name of the bank this transformer is part of, for CIM, MultiSpeak, and other interfaces.';
    PropertyHelp[39] := 'Name of a library entry for transformer properties. The named XfmrCode must already be defined.';
    PropertyHelp[40] := '={Yes|No} Default is NO. Signifies whether or not the X/R is assumed contant for harmonic studies.';
    PropertyHelp[41] := 'Alternative to XHL for specifying the percent reactance from winding 1 to winding 2.  Use ' +
        'for 2- or 3-winding transformers. Percent on the kVA base of winding 1. ';
    PropertyHelp[42] := 'Alternative to XHT for specifying the percent reactance from winding 1 to winding 3.  Use ' +
        'for 3-winding transformers only. Percent on the kVA base of winding 1. ';
    PropertyHelp[43] := 'Alternative to XLT for specifying the percent reactance from winding 2 to winding 3.Use ' +
        'for 3-winding transformers only. Percent on the kVA base of winding 1.  ';
    PropertyHelp[44] := '{Lead | Lag (default) | ANSI (default) | Euro } Designation in mixed Delta-wye connections the ' +
        'relationship between HV to LV winding. Default is ANSI 30 deg lag, e.g., Dy1 of Yd1 vector group. ' +
        'To get typical European Dy11 connection, specify either "lead" or "Euro"';
    PropertyHelp[45] := '(Read only) Makes winding currents available via return on query (? Transformer.TX.WdgCurrents). ' +
        'Order: Phase 1, Wdg 1, Wdg 2, ..., Phase 2 ...';
    PropertyHelp[46] := '{Shell*|5-leg|3-Leg|1-phase} Core Type. Used for GIC analysis';
    PropertyHelp[47] := 'Winding dc resistance in OHMS. Useful for GIC analysis. From transformer test report. ' +
        'Defaults to 85% of %R property';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTransf.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit do
    begin

        ActiveCktElement := TTransfObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);  // Return index of transformer in transformer list

    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTransf.Edit: Integer;
{
  A Transf Defaults to 3-phases, 2-windings (both wye)
}
var
    ParamPointer,
    i: Integer;
    ParamName: String;  {For parsing property names}
    Param: String;

begin
  // continue parsing cmdline presently in Parser

  {Make this object the active circuit element}
    ActiveTransfObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveTransfObj;  // use property to set this value

    Result := 0;

    with ActiveTransfObj do
    begin
        XHLChanged := FALSE;
        ParamPointer := 0;
        ParamName := Parser.NextParam;
        Param := Parser.StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "Transformer.' + Name + '"', 110);
                1:
                    Nphases := Parser.IntValue;
                2:
                    SetNumWindings(Parser.IntValue); // Reallocate stuff if bigger
                3:
                    SetActiveWinding(Parser.IntValue);
                4:
                    Setbus(ActiveWinding, param);
                5:
                    InterpretConnection(Param);
                6:
                    Winding^[ActiveWinding].kVLL := parser.Dblvalue;
                7:
                    Winding^[ActiveWinding].kVA := parser.Dblvalue;
                8:
                    Winding^[ActiveWinding].puTap := parser.Dblvalue;
                9:
                    Winding^[ActiveWinding].Rpu := parser.Dblvalue * 0.01;  // %R
                10:
                    Winding^[ActiveWinding].Rneut := parser.Dblvalue;
                11:
                    Winding^[ActiveWinding].Xneut := parser.Dblvalue;
                12:
                    InterpretAllBuses(Param);
                13:
                    InterpretAllConns(Param);
                14:
                    InterpretAllkVRatings(Param);
                15:
                    InterpretAllkVARatings(Param);
                16:
                    InterpretAllTaps(Param);
                17:
                    XHL := TrapZero(parser.Dblvalue, 7.0) * 0.01;
                18:
                    XHT := TrapZero(parser.Dblvalue, 35.0) * 0.01;
                19:
                    XLT := TrapZero(parser.Dblvalue, 30.0) * 0.01;
                20:
                    Parser.ParseAsVector(((NumWindings - 1) * NumWindings div 2), Xsc);
                21:
                    ThermalTimeConst := Parser.DblValue;
                22:
                    n_thermal := Parser.DblValue;
                23:
                    m_thermal := Parser.DblValue;
                24:
                    FLrise := Parser.DblValue;
                25:
                    HSRise := Parser.DblValue;
                26:
                    pctLoadLoss := Parser.DblValue;
                27:
                    pctNoLoadLoss := Parser.DblValue;
                28:
                    NormMaxHkVA := Parser.Dblvalue;
                29:
                    EmergMaxHkVA := Parser.Dblvalue;
                30:
                    IsSubstation := InterpretYesNo(Param);
                31:
                    Winding^[ActiveWinding].MaxTap := Parser.DblValue;
                32:
                    Winding^[ActiveWinding].MinTap := Parser.DblValue;
                33:
                    Winding^[ActiveWinding].NumTaps := Parser.IntValue;
                34:
                    SubstationName := Param;
                35:
                    pctImag := Parser.DblValue;
                36:
                    ppm_FloatFactor := Parser.DblValue * 1.0e-6;
                37:
                    InterpretAllRs(Param);
                38:
                    XfmrBank := Param;
                39:
                    FetchXfmrCode(Param);
                40:
                    XRConst := InterpretYesNo(Param);
                41:
                    XHL := TrapZero(parser.Dblvalue, 7.0) * 0.01;
                42:
                    XHT := TrapZero(parser.Dblvalue, 35.0) * 0.01;
                43:
                    XLT := TrapZero(parser.Dblvalue, 30.0) * 0.01;
                44:
                    HVLeadsLV := InterpretLeadLag(Param);
                45:
                    PropertyValue[45] := '';  // placeholder, do nothing just throw value away if someone tries to set it.
                46:
                    strCoreType := Param;
                47:
                    Winding^[ActiveWinding].RdcOhms := Parser.DblValue;
            else
           // Inherited properties
                ClassEdit(ActiveTransfObj, ParamPointer - NumPropsThisClass)
            end;

         {Take care of properties that require some additional work,}
            case ParamPointer of
                1:
                    NConds := Fnphases + 1;  // Force redefinition of number of conductors and reallocation of matrices
          // default all winding kVAs to first winding so latter Donot have to be specified
                7:
                    if (ActiveWinding = 1) then
                    begin
                        for i := 2 to NumWindings do
                            Winding^[i].kVA := Winding^[1].kVA;
                        NormMaxHkVA := 1.1 * Winding^[1].kVA;    // Defaults for new winding rating.
                        EmergMaxHkVA := 1.5 * Winding^[1].kVA;
                    end
                    else
                    if NumWindings = 2 then
                    begin
                        Winding^[1].kVA := Winding^[2].kVA;  // For 2-winding, force both kVAs to be same
                    end;
           // Update LoadLosskW if winding %r changed. Using only windings 1 and 2
                9:
                    pctLoadLoss := (Winding^[1].Rpu + Winding^[2].Rpu) * 100.0;
                15:
                begin
                    NormMaxHkVA := 1.1 * Winding^[1].kVA;    // Defaults for new winding rating.
                    EmergMaxHkVA := 1.5 * Winding^[1].kVA;
                end;
                17..19:
                    XHLChanged := TRUE;
                20:
                    for i := 1 to ((NumWindings - 1) * NumWindings div 2) do
                        Xsc^[i] := Xsc^[i] * 0.01;  // Convert to per unit

                26:
                begin    // Assume load loss is split evenly  between windings 1 and 2
                    Winding^[1].Rpu := pctLoadLoss / 2.0 / 100.0;
                    Winding^[2].Rpu := Winding^[1].Rpu;
                end;
                37:
                    pctLoadLoss := (Winding^[1].Rpu + Winding^[2].Rpu) * 100.0;  // Update
                41..43:
                    XHLChanged := TRUE;
                46:
                    CoreType := InterpretCoreType(Param); // Assign integer number
                47:
                    Winding^[ActiveWinding].RdcSpecified := TRUE;
            else
            end;

         //YPrim invalidation on anything that changes impedance values
            case ParamPointer of
                5..19:
                    YprimInvalid := TRUE;
                26..27:
                    YprimInvalid := TRUE;
                35..37:
                    YprimInvalid := TRUE;
                41..43:
                    YPrimInvalid := TRUE;
            else
            end;

         {Advance to next property on input line}
            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        RecalcElementData;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTransf.SetActiveWinding(w: Integer);

begin
    with ActiveTransfObj do
        if (w > 0) and (w <= NumWindings) then
            ActiveWinding := w
        else
            DoSimpleMsg('Wdg parameter invalid for "' + ActiveTransfObj.Name + '"', 112);
end;

function TTransf.TrapZero(const Value: Double; DefaultValue: Double): Double;
begin
    if Value = 0.0 then
    begin
        Dosimplemsg('Zero Reactance specified for Transformer.' + ActiveTransfObj.Name, 11201);
        Result := DefaultValue;
    end
    else
        Result := Value;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTransf.InterpretConnection(const S: String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN

begin
    with ActiveTransfObj do
    begin
        with Winding^[ActiveWinding] do
        begin
            case lowercase(S)[1] of
                'y', 'w':
                    Connection := 0;  {Wye}
                'd':
                    Connection := 1;  {Delta or line-Line}
                'l':
                    case lowercase(s)[2] of
                        'n':
                            Connection := 0;
                        'l':
                            Connection := 1;
                    end;
            end;
        end;
        Yorder := fNConds * fNTerms;
        YPrimInvalid := TRUE;
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTransf.InterpretAllConns(const S: String);
//  routine expecting all winding connections expressed in one array of strings
var
    S1,
    S2: String;
    i: Integer;
begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings}
    with ActiveTransfObj do
        for i := 1 to Numwindings do
        begin
            ActiveWinding := i;
            S1 := AuxParser.NextParam; // ignore any parameter name  not expecting any
            S2 := AuxParser.StrValue;
            if Length(S2) > 0 then
                InterpretConnection(S2);
        end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTransf.InterpretAllBuses(const S: String);
//  routine expecting all winding bus connections expressed in one array of strings
var
    BusNam: String;
    i: Integer;
begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings;  Ignore omitted values}
    with ActiveTransfObj do
        for i := 1 to Numwindings do
        begin
            ActiveWinding := i;
            AuxParser.NextParam; // ignore any parameter name  not expecting any
            BusNam := AuxParser.StrValue;
            if Length(BusNam) > 0 then
                SetBus(ActiveWinding, BusNam);
        end;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTransf.InterpretLeadLag(const S: String): Boolean;
//  routine expecting all winding bus connections expressed in one array of strings
begin

    Result := FALSE;   // default to ANSI 30 Deg Lag if can't understand S

    if CompareTextShortest(S, 'lead') = 0 then
        Result := TRUE
    else
    if CompareTextShortest(S, 'euro') = 0 then
        Result := TRUE;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTransf.InterpretAllkVRatings(const S: String);
//  routine expecting all winding kV ratings expressed in one array of strings
var
    DataStr: String;
    i: Integer;
begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings;  Ignore omitted values}
    with ActiveTransfObj do
        for i := 1 to Numwindings do
        begin
            ActiveWinding := i;
            AuxParser.NextParam; // ignore any parameter name  not expecting any
            DataStr := AuxParser.StrValue;
            if Length(DataStr) > 0 then
                Winding^[ActiveWinding].kVLL := AuxParser.Dblvalue;
        end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTransf.InterpretAllkVARatings(const S: String);
//  routine expecting all winding ratings expressed in one array of strings
var
    DataStr: String;
    i: Integer;
begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings;  Ignore omitted values}
    with ActiveTransfObj do
        for i := 1 to Numwindings do
        begin
            ActiveWinding := i;
            AuxParser.NextParam; // ignore any parameter name  not expecting any
            DataStr := AuxParser.StrValue;
            if Length(DataStr) > 0 then
                Winding^[ActiveWinding].kVA := AuxParser.Dblvalue;
        end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTransf.InterpretAllRs(const S: String);
//  routine expecting all winding ratings expressed in one array of strings
var
    DataStr: String;
    i: Integer;
begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings;  Ignore omitted values}
    with ActiveTransfObj do
        for i := 1 to Numwindings do
        begin
            ActiveWinding := i;
            AuxParser.NextParam; // ignore any parameter name  not expecting any
            DataStr := AuxParser.StrValue;
            if Length(DataStr) > 0 then
                Winding^[ActiveWinding].Rpu := AuxParser.Dblvalue * 0.01;
        end;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTransf.InterpretAllTaps(const S: String);
//  routine expecting all winding taps expressed in one array of strings
var
    DataStr: String;
    i: Integer;
begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings;  Ignore omitted values}
    with ActiveTransfObj do
        for i := 1 to Numwindings do
        begin
            ActiveWinding := i;
            AuxParser.NextParam; // ignore any parameter name,  not expecting any
            DataStr := AuxParser.StrValue;
            if Length(DataStr) > 0 then
                Winding^[ActiveWinding].puTap := AuxParser.Dblvalue;
        end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTransf.MakeLike(const TransfName: String): Integer;
var
    OtherTransf: TTransfObj;
    i: Integer;

begin
    Result := 0;
   {See if we can find this Transf name in the present collection}
    OtherTransf := Find(TransfName);
    if OtherTransf <> NIL then
        with ActiveTransfObj do
        begin
            Nphases := OtherTransf.Fnphases;
            SetNumWindings(OtherTransf.NumWindings);
            NConds := Fnphases + 1; // forces reallocation of terminals and conductors

            Yorder := fNConds * fNTerms;
            YPrimInvalid := TRUE;

            for i := 1 to NumWindings do
                with Winding^[i] do
                begin
                    Connection := OtherTransf.Winding^[i].Connection;
                    kVLL := OtherTransf.Winding^[i].kVLL;
                    VBase := OtherTransf.Winding^[i].VBase;
                    kVA := OtherTransf.Winding^[i].kVA;
                    puTAP := OtherTransf.Winding^[i].puTAP;
                    Rpu := OtherTransf.Winding^[i].Rpu;
                    RdcOhms := OtherTransf.Winding^[i].RdcOhms;
                    RdcSpecified := OtherTransf.Winding^[i].RdcSpecified;
                    RNeut := OtherTransf.Winding^[i].RNeut;
                    Xneut := OtherTransf.Winding^[i].Xneut;
           // copy the taps
                    TapIncrement := OtherTransf.Winding^[i].TapIncrement;
                    MinTap := OtherTransf.Winding^[i].MinTap;
                    MaxTap := OtherTransf.Winding^[i].MaxTap;
                    NumTaps := OtherTransf.Winding^[i].NumTaps;
                end;

            SetTermRef;

            XHL := OtherTransf.XHL;
            XHT := OtherTransf.XHT;
            XLT := OtherTransf.XLT;

            for i := 1 to (NumWindings * (NumWindings - 1) div 2) do
                XSc^[i] := OtherTransf.XSC^[i];

            ZB.CopyFrom(OtherTransf.ZB);
            Y_1Volt.CopyFrom(OtherTransf.Y_1Volt);
            Y_Term.CopyFrom(OtherTransf.Y_Term);
            Y_1Volt_NL.CopyFrom(OtherTransf.Y_1Volt_NL);
            Y_Term_NL.CopyFrom(OtherTransf.Y_Term_NL);

            ThermalTimeConst := OtherTransf.ThermalTimeConst;
            n_thermal := OtherTransf.n_thermal;
            m_thermal := OtherTransf.m_thermal;
            FLrise := OtherTransf.FLrise;
            HSrise := OtherTransf.HSrise;
            pctLoadLoss := OtherTransf.pctLoadLoss;
            pctNoLoadLoss := OtherTransf.pctNoLoadLoss;
            NormMaxHkVA := OtherTransf.NormMaxHkVA;
            EmergMaxHkVA := OtherTransf.EmergMaxHkVA;
            XRConst := OtherTransf.XRConst;

            XfmrBank := OtherTransf.XfmrBank;
            XfmrCode := OtherTransf.XfmrCode;

            ClassMakeLike(OtherTransf);

            for i := 1 to ParentClass.NumProperties do
         // Skip readonly properties
                if i <> 45 then
                    PropertyValue[i] := OtherTransf.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in Transf MakeLike: "' + TransfName + '" Not Found.', 113);


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTransf.Init(Handle: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TTransf.Init', -1);
    Result := 0;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TTransf Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TTransfObj.Create(ParClass: TDSSClass; const TransfName: String);
var
    i: Integer;
begin
    inherited Create(ParClass);
    Name := LowerCase(TransfName);
    DSSObjType := ParClass.DSSClassType; //DSSObjType + XFMR; // override PDElement   (kept in both actually)

    Nphases := 3;  // Directly set conds and phases
    fNConds := Fnphases + 1;
    SetNumWindings(2);  // must do this after setting number of phases
    ActiveWinding := 1;

    Nterms := NumWindings;  // Force allocation of terminals and conductors

    XHL := 0.07;
    XHT := 0.35;
    XLT := 0.30;
    XHLChanged := TRUE;  // Set flag to for calc of XSC array from XHL, etc.

    DeltaDirection := 1;
    SubstationName := '';
    XfmrBank := '';
    XfmrCode := '';

    CoreType := 0;
    strCoreType := 'shell';

    VABase := Winding^[1].kVA * 1000.0;
    ThermalTimeconst := 2.0;
    n_thermal := 0.8;
    m_thermal := 0.8;
    FLrise := 65.0;
    HSrise := 15.0;  // Hot spot rise
    NormMaxHkVA := 1.1 * Winding^[1].kVA;
    EmergMaxHkVA := 1.5 * Winding^[1].kVA;
    pctLoadLoss := 2.0 * Winding^[1].Rpu * 100.0; //  assume two windings for init'ing
    ppm_FloatFactor := 0.000001;
  {Compute antifloat added for each winding    }
    for i := 1 to NumWindings do
        Winding^[i].ComputeAntiFloatAdder(ppm_FloatFactor, VABase / FNPhases);

  {Default the no load properties to zero}
    pctNoLoadLoss := 0.0;
    pctImag := 0.0;

  {Basefrequency := 60.0;   set in base class to circuit fundamental freq; Do not reset here}
    FaultRate := 0.007;
    IsSubstation := FALSE;
    XRConst := FALSE;

    HVLeadsLV := FALSE; // Defaults to ANSI connection

    Y_Terminal_FreqMult := 0.0;

    Yorder := fNTerms * fNconds;
    InitPropertyValues(0);
    RecalcElementData;
end;


procedure TTransfObj.SetNumWindings(N: Integer);
var
    i: Integer;
    OldWdgSize: Integer;
    NewWdgSize: Integer;
begin
    if N > 1 then
    begin
        for i := 1 to NumWindings do
            Winding^[i].Free;  // Free old winding objects
        OldWdgSize := (NumWindings - 1) * NumWindings div 2;
        NumWindings := N;
        MaxWindings := N;
        NewWdgSize := (NumWindings - 1) * NumWindings div 2;
        FNconds := Fnphases + 1;
        Nterms := NumWindings;
        Reallocmem(Winding, Sizeof(Winding^[1]) * MaxWindings);  // Reallocate collector array
        for i := 1 to MaxWindings do
            Winding^[i] := TWinding.Create;

    // array of short circuit measurements between pairs of windings
        ReAllocmem(XSC, SizeOF(XSC^[1]) * NewWdgSize);
        for i := OldWdgSize + 1 to NewWdgSize do
            XSC^[i] := 0.30;
        Reallocmem(TermRef, SizeOf(TermRef^[1]) * 2 * NumWindings * Fnphases);

    {Reallocate impedance matrices}
        ZB.Free;
        Y_1Volt.Free;
        Y_1Volt_NL.Free;
        Y_Term.Free;
        Y_Term_NL.Free;

        ZB := TCMatrix.CreateMatrix(NumWindings - 1);
        Y_1Volt := TCMatrix.CreateMatrix(NumWindings);
        Y_1Volt_NL := TCMatrix.CreateMatrix(NumWindings);
        Y_Term := TCMatrix.CreateMatrix(2 * NumWindings);
        Y_Term_NL := TCMatrix.CreateMatrix(2 * NumWindings);
    end
    else
        Dosimplemsg('Invalid number of windings: (' + IntToStr(N) + ') for Transformer ' + Name, 111);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TTransfObj.Destroy;

var
    i: Integer;
begin
    {Throw away stuff allocated for this object}
    for i := 1 to NumWindings do
        Winding^[i].Free;
    Reallocmem(Winding, 0);
    Reallocmem(XSC, 0);
    Reallocmem(TermRef, 0);
    ZB.Free;
    Y_1Volt.Free;
    Y_1Volt_NL.Free;
    Y_Term.Free;
    Y_Term_NL.Free;
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTransfObj.RecalcElementData;
var
    i,
    ihvolt: Integer;
    VFactor: Double;

begin

  // Determine Delta Direction
   // If high voltage is delta, delta leads y
   // If high voltage is wye, delta lags wye
    if Winding^[1].connection = Winding^[2].connection then
        DeltaDirection := 1
    else
    begin
        if Winding^[1].kVLL >= Winding^[2].kVLL then
            iHvolt := 1
        else
            iHVolt := 2;
        case Winding^[iHvolt].Connection of
            0:
                if HVLeadsLV then
                    DeltaDirection := -1
                else
                    DeltaDirection := 1;
            1:
                if HVLeadsLV then
                    DeltaDirection := 1
                else
                    DeltaDirection := -1;
        else
         // ---old code --- If Winding^[2].Connection = 0 Then DeltaDirection := -1 Else DeltaDirection := 1;
        end;
    end;

    SetTermRef;   // Re-establish TermRef IF num windings or connection changed

    for i := 1 to NumWindings do
    begin
        with Winding^[i] do
            if (NumTaps > 0) then
                TapIncrement := (MaxTap - MinTap) / NumTaps
            else
                TapIncrement := 0.0;
    end;

    if XHLChanged then
    begin
     {should only happen for 2- and 3-winding transformers}
        if NumWindings <= 3 then
            for i := 1 to (NumWindings * (NumWindings - 1) div 2) do
                case i of
                    1:
                        XSC^[1] := XHL;
                    2:
                        XSC^[2] := XHT;
                    3:
                        XSC^[3] := XLT;
                else
                end;
        XHLChanged := FALSE;
    end;

   // Set winding voltage bases (in volts)
    for i := 1 to NumWindings do
        with Winding^[i] do  // Get the actual turns voltage base for each winding
            case Connection of
                0:
                    case Fnphases of   // Wye
                        2, 3:
                            VBase := kVLL * InvSQRT3x1000;   // assume 3-phase for 2-phase designation
                    else
                        VBase := kVLL * 1000.0;
                    end;
                1:
                    VBase := kVLL * 1000.0;     // delta

            end;

  {Base rating of Winding 1 }
    VABase := Winding^[1].kVA * 1000.0;

   // Set Rdc parameters for each winding.
    for i := 1 to NumWindings do
        with Winding^[i] do
        begin
            if RdcSpecified then
                Rdcpu := RdcOhms / (SQR(VBase) / VABase)
            else
            begin
                Rdcpu := abs(0.85 * Rpu); // use abs in case this resistance comes out negative.
                RdcOhms := Rdcpu * SQR(VBase) / VABase;
            end;
        end;


    for i := 1 to NumWindings do
        Winding^[i].ComputeAntiFloatAdder(ppm_FloatFactor, VABase / FNPhases);

   { Normal and Emergency terminal current Rating for UE check}
    Vfactor := 1.0;  // ensure initialization
    case Winding^[1].connection of
        0:
            VFactor := Winding^[1].VBase * 0.001;   // wye
        1:
            case Fnphases of
                1:
                    VFactor := Winding^[1].VBase * 0.001;
                2, 3:
                    VFactor := Winding^[1].VBase * 0.001 / SQRT3;
            else
                VFactor := Winding^[1].VBase * 0.001 * 0.5 / sin(pi / Fnphases);
            end;
    end;

     {Divide per phase kVA by voltage to neutral}
    NormAmps := NormMaxHkVA / Fnphases / Vfactor;
    EmergAmps := EmergMaxHkVA / Fnphases / Vfactor;

    CalcY_Terminal(1.0);   // Calc Y_Terminal at base frequency
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTransfObj.SaveWrite(var F: TextFile);
{Override standard SaveWrite}
{Transformer structure not conducive to standard means of saving}
var
    iprop: Integer;
    i: Integer;
begin
   {Write only properties that were explicitly set in the
   final order they were actually set}
    iProp := GetNextPropertySet(0); // Works on ActiveDSSObject
    while iProp > 0 do
    begin
        with ParentClass do
       {Trap wdg= and write out array properties instead}
            case RevPropertyIdxMap[iProp] of
                3:
                begin   // if WDG= was ever used write out arrays ...
                    for i := 12 to 16 do
                        Write(F, Format(' %s=%s', [PropertyName^[i], GetPropertyValue(i)]));
                    for i := 1 to Numwindings do
                        with Winding^[i] do
                            Write(F, Format(' wdg=%d %sR=%.7g RdcOhms=%.7g', [i, '%', Rpu * 100.0, RdcOhms]));
                end;
                4..9:
{do Nothing}; // Ignore these properties; use arrays instead

            else
                if Length(PropertyValue[iProp]) > 0 then
                    Write(F, Format(' %s=%s', [PropertyName^[RevPropertyIdxMap[iProp]], CheckForBlanks(PropertyValue[iProp])]));
            end;
        iProp := GetNextPropertySet(iProp);
    end;


end;

procedure TTransfObj.SetTermRef;

// sets an array which maps the two conductors of each winding to the
// phase and neutral conductors of the transformer according to the winding connection

var
    i, j, k: Integer;

begin
    k := 0;

    case Fnphases of
        1:
            for j := 1 to NumWindings do
            begin
                Inc(k);
                TermRef^[k] := (j - 1) * fNconds + 1;
                Inc(k);
                TermRef^[k] := j * fNconds;
            end;
    else
        for i := 1 to Fnphases do
        begin
            for  j := 1 to NumWindings do
            begin
                Inc(k);
                case Winding^[j].Connection of
                    0:
                    begin      // Wye
                        TermRef^[k] := (j - 1) * fNconds + i;
                        Inc(k);
                        TermRef^[k] := j * fNconds;
                    end;
{**** WILL THIS WORK for 2-PHASE OPEN DELTA ???? Need to check this sometime}
                    1:
                    begin   // Delta
                        TermRef^[k] := (j - 1) * fNconds + i;
                        Inc(k);
                        TermRef^[k] := (j - 1) * fNconds + RotatePhases(i);  // connect to next phase in sequence
                    end;
                end; {CASE connection}
            end;
        end;
    end; {CASE Fnphases}
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTransfObj.CalcYPrim;

var
    FreqMultiplier: Double;

begin

    if YPrimInvalid then
    begin
         // Reallocate YPrim if something has invalidated old allocation
        if YPrim_Series <> NIL then
            YPrim_Series.Free;
        if YPrim_Shunt <> NIL then
            YPrim_Shunt.Free;
        if YPrim <> NIL then
            YPrim.Free;

        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin  {Same size as last time; just zero out to start over}
        YPrim_Series.Clear; // zero out YPrim
        YPrim_Shunt.Clear; // zero out YPrim
        Yprim.Clear;
    end;

    // Set frequency multipliers for this calculation
    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;
    // Check for rebuilding Y_Terminal; Only rebuild if freq is different than last time
    if FreqMultiplier <> Y_Terminal_Freqmult then
        CalcY_Terminal(FreqMultiplier);

    BuildYPrimComponent(YPrim_Series, Y_Term);
    BuildYPrimComponent(YPrim_Shunt, Y_Term_NL);

    AddNeutralToY(FreqMultiplier);

    {Combine the two Yprim components into Yprim}
    YPrim.CopyFrom(YPrim_Series);
    Yprim.AddFrom(Yprim_Shunt);

    {Now Account for Open Conductors}
    {For any conductor that is open, zero out row and column}
    inherited CalcYPrim;

    YprimInvalid := FALSE;
end;

procedure TTransfObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, j: Integer;
    ZBtemp: Tcmatrix;

begin
    inherited DumpProperties(F, Complete);

    {Basic Property Dump}

    Writeln(F, '~ ', 'NumWindings=', NumWindings: 0);
    Writeln(F, '~ ', 'phases=', Fnphases: 0);

    for i := 1 to NumWindings do
    begin
        with Winding^[i] do
        begin
            if i = 1 then
                Writeln(F, '~ ', 'Wdg=', i: 0, ' bus=', firstbus)
            else
                Writeln(F, '~ ', 'Wdg=', i: 0, ' bus=', nextbus);
            case Connection of
                0:
                    Writeln(f, '~ conn=wye');
                1:
                    Writeln(f, '~ conn=delta');
            end;
            Writeln(f, '~ kv=', kVLL: 0: 2);
            Writeln(f, '~ kVA=', kVA: 0: 1);
            Writeln(f, '~ tap=', putap: 0: 3);
            Writeln(f, '~ %R=', (Rpu * 100.0): 0: 2);
            Writeln(f, Format('~ RdcOhms=%.7g', [Rdcohms]));
            Writeln(f, '~ rneut=', rneut: 0: 3);
            Writeln(f, '~ xneut=', xneut: 0: 3);
        end;
    end;

    Writeln(F, '~ ', 'XHL=', xhl * 100.0: 0: 3);
    Writeln(F, '~ ', 'XHT=', xht * 100.0: 0: 3);
    Writeln(F, '~ ', 'XLT=', xlt * 100.0: 0: 3);
    Writeln(F, '~ ', 'X12=', xhl * 100.0: 0: 3);
    Writeln(F, '~ ', 'X13=', xht * 100.0: 0: 3);
    Writeln(F, '~ ', 'X23=', xlt * 100.0: 0: 3);
    Write(F, '~ Xscmatrix= "');
    for i := 1 to (NumWindings - 1) * NumWindings div 2 do
        Write(F, Xsc^[i] * 100.0: 0: 2, ' ');
    Writeln(F, '"');
    Writeln(F, '~ ', 'NormMAxHkVA=', NormMAxHkVA: 0: 0);
    Writeln(F, '~ ', 'EmergMAxHkVA=', EmergMAxHkVA: 0: 0);
    Writeln(F, '~ ', 'thermal=', thermalTimeConst: 0: 1);
    Writeln(F, '~ ', 'n=', n_thermal: 0: 1);
    Writeln(F, '~ ', 'm=', m_thermal: 0: 1);
    Writeln(F, '~ ', 'flrise=', flrise: 0: 0);
    Writeln(F, '~ ', 'hsrise=', hsrise: 0: 0);
    Writeln(F, '~ ', '%loadloss=', pctLoadLoss: 0: 0);
    Writeln(F, '~ ', '%noloadloss=', pctNoLoadLoss: 0: 0);

    for i := 28 to NumPropsThisClass do
        Writeln(F, '~ ', ParentClass.PropertyName^[i], '=', PropertyValue[i]);

    with ParentClass do
    begin
        for i := NumPropsthisClass + 1 to NumProperties do
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
    end;

    if Complete then
    begin
        Writeln(F);
        ZBTemp := TCmatrix.CreateMatrix(NumWindings - 1);
        ZBTemp.CopyFrom(ZB);
        ZBTemp.Invert;

        Writeln(F, 'ZB:');
        with ZBTemp do
        begin
            for i := 1 to NumWindings - 1 do
            begin
                for j := 1 to i do
                    Write(F, format('%g ', [GetElement(i, j).re]));
                Writeln(F);
            end;
            for i := 1 to NumWindings - 1 do
            begin
                for j := 1 to i do
                    Write(F, format('%g ', [GetElement(i, j).im]));
                Writeln(F);
            end;
        end;  {WITH}

        ZBTemp.Free;

        Writeln(F);
        Writeln(F, 'ZB: (inverted)');
        with ZB do
        begin
            for i := 1 to NumWindings - 1 do
            begin
                for j := 1 to i do
                    Write(F, GetElement(i, j).re: 0: 4, ' ');
                Writeln(F);
            end;
            for i := 1 to NumWindings - 1 do
            begin
                for j := 1 to i do
                    Write(F, GetElement(i, j).im: 0: 4, ' ');
                Writeln(F);
            end;
        end;  {WITH}

        Writeln(F);
        Writeln(F, 'Y_OneVolt');
        with Y_1Volt do
        begin
            for i := 1 to NumWindings do
            begin
                for j := 1 to i do
                    Write(F, GetElement(i, j).re: 0: 4, ' ');
                Writeln(F);
            end;
            for i := 1 to NumWindings do
            begin
                for j := 1 to i do
                    Write(F, GetElement(i, j).im: 0: 4, ' ');
                Writeln(F);
            end;
        end;

        Writeln(F);
        Writeln(F, 'Y_Terminal');
        with Y_Term do
        begin
            for i := 1 to 2 * NumWindings do
            begin
                for j := 1 to i do
                    Write(F, GetElement(i, j).re: 0: 4, ' ');
                Writeln(F);
            end;
            for i := 1 to 2 * NumWindings do
            begin
                for j := 1 to i do
                    Write(F, GetElement(i, j).im: 0: 4, ' ');
                Writeln(F);
            end;
        end;
        Writeln(F);
        Write(F, 'TermRef= ');
        for i := 1 to 2 * NumWindings * Fnphases do
            Write(F, TermRef^[i]: 0, ' ');
        Writeln(F);

    end;
end;

procedure TWinding.ComputeAntiFloatAdder(PPM_Factor, VABase1ph: Double);
begin
    Y_PPM := -PPM_Factor / (SQR(VBase) / VABase1ph) / 2.0;  //12-11-12 divided by two
       // put half on each terminal of the winding.
end;

constructor TWinding.Create;
{
   Make a new winding
}
begin
    inherited Create;
    Connection := 0;
    kVLL := 12.47;
    VBase := kVLL / SQRT3 * 1000.0;
    kVA := 1000.0;
    puTap := 1.0;
    Rpu := 0.002;
    Rdcpu := Rpu * 0.85;  // default value
    RdcOhms := Sqr(kVLL) / (kVA / 1000.0) * Rdcpu;
    RdcSpecified := FALSE;
    Rneut := -1.0;    // default to open - make user specify connection
    Xneut := 0.0;
    ComputeAntiFloatAdder(1.0e-6, kVA / 3.0 / 1000.0);     //  1 PPM

    TapIncrement := 0.00625;
    NumTaps := 32;
    MaxTap := 1.10;
    MinTap := 0.90;

end;

destructor TWinding.Destroy;
begin
    inherited Destroy;
end;

function TTransfObj.Get_PresentTap(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].puTap
    else
        Result := 0.0;
end;

procedure TTransfObj.Set_PresentTap(i: Integer; const Value: Double);

var
    TempVal: Double;

begin
    if (i > 0) and (i <= NumWindings) then
        with Winding^[i] do
        begin
           {Range Checking}
            TempVal := Value;
            if (TempVal < MinTap) then
                TempVal := MinTap
            else
            if (TempVal > MaxTap) then
                TempVal := MaxTap;

            if TempVal <> puTap then
            begin    {Only if there's been a change}
                puTap := TempVal;
                YPrimInvalid := TRUE;  // this property triggers setting SystemYChanged=true
                RecalcElementData;
            end;
        end;
end;

function TTransfObj.Get_WdgResistance(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].Rpu
    else
        Result := 0.0;
end;

function TTransfObj.Get_WdgRdc(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].Rdcohms
    else
        Result := 0.0;
end;

function TTransfObj.Get_WdgkVA(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].kVA
    else
        Result := 0.0;
end;

function TTransfObj.Get_WdgRneutral(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].Rneut
    else
        Result := 0.0;
end;

function TTransfObj.Get_WdgXneutral(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].Xneut
    else
        Result := 0.0;
end;

function TTransfObj.Get_WdgYPPM(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].Y_PPM
    else
        Result := 0.0;
end;

function TTransfObj.Get_Xsc(i: Integer): Double;
var
    imax: Integer;
begin
    imax := (NumWindings - 1) * NumWindings div 2;
    if (i > 0) and (i <= imax) then
        Result := XSC^[i]
    else
        Result := 0.0;
end;


function TTransfObj.Get_WdgConnection(i: Integer): Integer;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].Connection
    else
        Result := 0;
end;

function TTransfObj.Get_MinTap(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].MinTap
    else
        Result := 0.0;
end;

function TTransfObj.Get_MaxTap(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].MaxTap
    else
        Result := 0.0;
end;

function TTransfObj.Get_NumTaps(i: Integer): Integer;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].NumTaps
    else
        Result := 0;
end;

function TTransfObj.Get_TapIncrement(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].TapIncrement
    else
        Result := 0.0;
end;

procedure TTransfObj.GetAllWindingCurrents(CurrBuffer: pComplexArray);

{
  Return a vector of complex currents for each Winding of all phases

  Iterm = Yterm * Vterm

  Yterm order is 2*NumWindings.  Each phase has same Yterm.
  Vterm order is 2*NumWindings .

  Calculate Iterm phase-by-phase and concatenate into CurrBuffer.
}

var
    i, jphase, k, iPhase, iWind, NeutTerm: Integer;
    VTerm: pComplexArray;
    ITerm: pComplexArray;
    ITerm_NL: pComplexArray;

begin

    try
        Vterm := Allocmem(SizeOf(Complex) * 2 * NumWindings);
        Iterm := Allocmem(SizeOf(Complex) * 2 * NumWindings);
        ITerm_NL := Allocmem(SizeOf(Complex) * 2 * NumWindings);

     {Load up Vterminal - already allocated for all cktelements}
        with ActiveCircuit.Solution do
            if Assigned(NodeV) then
                for i := 1 to Yorder do
                    Vterminal^[i] := NodeV^[NodeRef^[i]]
            else
                for i := 1 to Yorder do
                    Vterminal^[i] := CZERO;


        k := 0;
        for iPhase := 1 to Fnphases do
        begin
            for iWind := 1 to NumWindings do
            begin
                NeutTerm := iWind * FNConds;
                i := 2 * iWind - 1;

                case Winding^[iWind].Connection of
                    0:
                    begin   // Wye
                        VTerm^[i] := Vterminal^[iphase + (iWind - 1) * FNconds];
                        VTerm^[i + 1] := Vterminal^[NeutTerm];
                    end;
                    1:
                    begin   // Delta
                        jphase := RotatePhases(iphase);      // Get next phase in sequence
                        VTerm^[i] := Vterminal^[iphase + (iWind - 1) * FNconds];
                        VTerm^[i + 1] := Vterminal^[jphase + (iWind - 1) * FNconds];
                    end
                end; {CASE}

            end;
            Y_Term.MVmult(ITerm, VTerm);  // ITerm = Y_Term Vterm
            Y_Term_NL.MVmult(ITerm_NL, Vterm);// no load part
        // Add into Currbuffer
            for i := 1 to 2 * NumWindings do
            begin
                k := k + 1;
                CurrBuffer^[k] := Cadd(ITerm^[i], ITerm_NL^[i]);
            end;
        end;

        ReallocMem(Vterm, 0);
        ReallocMem(Iterm, 0);
        ReallocMem(Iterm_NL, 0);


    except
        On E: Exception do
            DoSimpleMsg('Error filling voltage buffer in GetAllWindingCurrents for Circuit Element:Transformer.' + Name + CRLF +
                'Probable Cause: Invalid definition of element.' + CRLF +
                'System Error Message: ' + E.Message, 100114);
    end;

end;


function TTransfObj.GetWindingCurrentsResult: String;

// Returns string mag, angle
var
    WindingCurrents: pComplexArray;
    i, j, k: Integer;

begin

    WindingCurrents := AllocMem(Sizeof(Complex) * 2 * FNPhases * NumWindings);

    GetAllWindingCurrents(WindingCurrents);

    Result := '';
    k := 0;
    for  i := 1 to Fnphases do
    begin
        for j := 1 to NumWindings do
        begin
            k := k + 1;
            Result := Result + Format('%.7g, (%.5g), ', [Cabs(WindingCurrents^[k]), Cdang(WindingCurrents^[k])]);
            k := k + 1;
           // Skip currents from other end of the winding
        end;
    end;

    Reallocmem(WindingCurrents, 0);  // throw away temp array

end;

procedure TTransfObj.GetWindingVoltages(iWind: Integer; VBuffer: pComplexArray);

//  Voltages across indicated winding
// Fill Vbuffer array which must be adequately allocated by calling routine
// Order is Number of Phases

var
    i, ii, k, NeutTerm: Integer;

begin

    try

     {return Zero if winding number improperly specified}
        if (iWind < 1) or (iWind > NumWindings) then
        begin
            for i := 1 to FNconds do
                VBuffer^[i] := CZERO;
            Exit;
        end;

     {Load up VTerminal - already allocated for all cktelements}
        with ActiveCircuit.Solution do
            for i := 1 to Yorder do
                Vterminal^[i] := NodeV^[NodeRef^[i]];


        k := (iWind - 1) * FNconds;    // Offset for winding
        NeutTerm := Fnphases + k + 1;
        for i := 1 to Fnphases do
            case Winding^[iWind].Connection of
                0:
                begin      // Wye
                    VBuffer^[i] := Csub(Vterminal^[i + k], Vterminal^[NeutTerm]);
                end;
                1:
                begin   // Delta
                    ii := RotatePhases(i);      // Get next phase in sequence
                    VBuffer^[i] := CSub(Vterminal^[i + k], Vterminal^[ii + k]);
                end
            end; {CASE}

    except
        On E: Exception do
            DoSimpleMsg('Error filling voltage buffer in GetWindingVoltages for Circuit Element:Transformer.' + Name + CRLF +
                'Probable Cause: Invalid definition of element.' + CRLF +
                'System Error Message: ' + E.Message, 114);
    end;
end;


function TTransfObj.Get_BaseVoltage(i: Integer): Double;
begin
    if (i < 1) or (i > NumWindings) then
        Result := Winding^[1].VBase
    else
        Result := Winding^[i].VBase;
end;

{============================== GetLosses Override ===============================}

procedure TTransfObj.GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex);
var
    cTempIterminal: pComplexArray;
    i: Integer;
begin
  {inherited;}

  {Calculates losses in watts, vars}
    TotalLosses := Losses;   // Side effect: computes Iterminal

  {Compute No load losses in Yprim_Shunt}
    cTempIterminal := AllocMem(Sizeof(Complex) * Yorder);
    ComputeVterminal;
    Yprim_Shunt.MVmult(cTempIterminal, Vterminal);
  {No Load Losses are sum of all powers coming into YPrim_Shunt from each terminal}
    NoLoadLosses := CZERO;
    for i := 1 to Yorder do
        Caccum(NoLoadLosses, Cmul(VTerminal^[i], conjg(cTempIterminal^[i])));

    LoadLosses := CSub(TotalLosses, NoLoadLosses);

    Reallocmem(cTempIterminal, 0);

end;

function TTransfObj.GetPropertyValue(Index: Integer): String;

{ gets the property for the active winding ; Set the active winding before calling}

var
    i: Integer;

begin
    case Index of
        12..16, 20, 37:
            Result := '[';
    else
        Result := '';
    end;

    case Index of
        1:
            Result := IntToStr(nPhases);
        2:
            Result := IntToStr(NumWindings);
        3:
            Result := IntToStr(ActiveWinding);  // return active winding
        4:
            Result := Getbus(ActiveWinding);    // return bus spec for active winding
        5:
            case Winding^[ActiveWinding].Connection of
                0:
                    Result := 'wye ';
                1:
                    Result := 'Delta ';
            else
            end;
        6:
            Result := Format('%.7g', [Winding^[ActiveWinding].kVLL]);
        7:
            Result := Format('%.7g', [Winding^[ActiveWinding].kVA]);
        8:
            Result := Format('%.7g', [Winding^[ActiveWinding].puTap]);
        9:
            Result := Format('%.7g', [Winding^[ActiveWinding].Rpu * 100.0]);   // %R
        10:
            Result := Format('%.7g', [Winding^[ActiveWinding].Rneut]);
        11:
            Result := Format('%.7g', [Winding^[ActiveWinding].Xneut]);

        12:
            for i := 1 to NumWindings do
                Result := Result + GetBus(i) + ', ';
        13:
            for i := 1 to NumWindings do
                case Winding^[i].Connection of
                    0:
                        Result := Result + 'wye, ';
                    1:
                        Result := Result + 'delta, ';
                else
                end;
        14:
            for i := 1 to NumWindings do
                Result := Result + Format('%.7g, ', [Winding^[i].kVLL]);
        15:
            for i := 1 to NumWindings do
                Result := Result + Format('%.7g, ', [Winding^[i].kVA]);
        16:
            for i := 1 to NumWindings do
                Result := Result + Format('%.7g, ', [Winding^[i].puTap]);// InterpretAllTaps(Param);
        17:
            Result := Format('%.7g', [XHL * 100.0]);
        18:
            Result := Format('%.7g', [XHT * 100.0]);
        19:
            Result := Format('%.7g', [XLT * 100.0]);
        20:
            for i := 1 to (NumWindings - 1) * NumWindings div 2 do
                Result := Result + Format('%-g, ', [Xsc^[i] * 100.0]);// Parser.ParseAsVector(((NumWindings - 1)*NumWindings div 2), Xsc);
        26:
            Result := Format('%.7g', [pctLoadLoss]);
        27:
            Result := Format('%.7g', [pctNoLoadLoss]);
        28:
            Result := Format('%.7g', [NormMaxHkVA]);
        29:
            Result := Format('%.7g', [EmergMaxHkVA]);
        31:
            Result := Format('%.7g', [Winding^[ActiveWinding].MaxTap]);
        32:
            Result := Format('%.7g', [Winding^[ActiveWinding].MinTap]);
        33:
            Result := Format('%-d', [Winding^[ActiveWinding].NumTaps]);
        35:
            Result := Format('%.7g', [pctImag]);
        36:
            Result := Format('%.7g', [ppm_FloatFactor / 1.0e-6]);
        37:
            for i := 1 to NumWindings do
                Result := Result + Format('%.7g, ', [Winding^[i].rpu * 100.0]);
        40:
            if XRconst then
                Result := 'YES'
            else
                Result := 'NO';
        41:
            Result := Format('%.7g', [XHL * 100.0]);
        42:
            Result := Format('%.7g', [XHT * 100.0]);
        43:
            Result := Format('%.7g', [XLT * 100.0]);

        45:
            Result := GetWindingCurrentsResult;
        46:
            case CoreType of
                0:
                    Result := 'shell';
                1:
                    Result := '1-phase';
                3:
                    Result := '3-leg';
                5:
                    Result := '5-Leg';
            end;
        47:
            Result := Format('%.7g', [Winding^[ActiveWinding].RdcOhms]);

    else
        Result := inherited GetPropertyValue(index);
    end;

        // Overrides
    case (Index - NumPropsThisClass) of
        1:
            Result := Format('%-.5g', [normamps]);  //Normamps
        2:
            Result := Format('%-.5g', [emergamps]);  //emergamps
    end;

    case Index of
        12..16, 20, 37:
            Result := Result + ']';
    else
    end;

end;

procedure TTransfObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '3'; //'phases';
    PropertyValue[2] := '2'; //'windings';
   // Winding Definition
    PropertyValue[3] := '1'; //'wdg';
    PropertyValue[4] := Getbus(1); //'bus';
    PropertyValue[5] := 'wye'; // 'conn';
    PropertyValue[6] := '12.47'; // IF 2or 3-phase:  phase-phase    ELSE actual winding
    PropertyValue[7] := '1000';
    PropertyValue[8] := '1.0';
    PropertyValue[9] := '0.2';
    PropertyValue[10] := '-1';
    PropertyValue[11] := '0';

   // General Data
    PropertyValue[12] := '';
    PropertyValue[13] := '';
    PropertyValue[14] := ''; // IF 1-phase: actual winding rating; ELSE phase-phase
    PropertyValue[15] := ''; // IF 1-phase: actual winding rating; ELSE phase-phase
    PropertyValue[16] := '';
    PropertyValue[17] := '7';
    PropertyValue[18] := '35';
    PropertyValue[19] := '30';
    PropertyValue[20] := '';  // x12 13 14... 23 24.. 34 ..
    PropertyValue[21] := '2';
    PropertyValue[22] := '.8';
    PropertyValue[23] := '.8';
    PropertyValue[24] := '65';
    PropertyValue[25] := '15';
    PropertyValue[26] := Format('%.7g', [pctLoadLoss]);
    PropertyValue[27] := Format('%.7g', [pctNoLoadLoss]);    // Defaults to zero
    PropertyValue[28] := '';
    PropertyValue[29] := '';
    PropertyValue[30] := 'n';  // =y/n
    PropertyValue[31] := '1.10';
    PropertyValue[32] := '0.90';
    PropertyValue[33] := '32';
    PropertyValue[34] := '';
    PropertyValue[35] := '0';
    PropertyValue[36] := '1';
    PropertyValue[37] := '';
    PropertyValue[38] := '';
    PropertyValue[39] := '';
    PropertyValue[40] := 'NO';
    PropertyValue[41] := '7';   // Same as XHT ...
    PropertyValue[42] := '35';
    PropertyValue[43] := '30';
    PropertyValue[44] := 'Lag';
    PropertyValue[45] := '0';
    PropertyValue[46] := 'shell';
    PropertyValue[47] := '26.4';  // ohms


    inherited  InitPropertyValues(NumPropsThisClass);

      // Override some Inherited properties
    PropertyValue[NumPropsThisClass + 1] := '400';  //Normamps
    PropertyValue[NumPropsThisClass + 2] := '600';  //emergamps
    PropertyValue[NumPropsThisClass + 3] := '0.007';  //Fault rate
    PropertyValue[NumPropsThisClass + 4] := '100';   // Pct Perm
    PropertyValue[NumPropsThisClass + 5] := '36';    // Hrs to repair

    ClearPropSeqArray;    // so the overrides don't show up on save

end;


function TTransfObj.RotatePhases(iPhs: Integer): Integer;
// For Delta connections or Line-Line voltages
begin
    Result := iPhs + DeltaDirection;

     // make sure result is within limits
    if FnPhases > 2 then
    begin
         // Assumes 2 phase delta is open delta
        if Result > Fnphases then
            Result := 1;
        if Result < 1 then
            Result := Fnphases;
    end
    else
    if Result < 1 then
        Result := 3;    // For 2-phase delta, next phase will be 3rd phase

end;

procedure TTransfObj.MakePosSequence;
{
  Converts default 3-phase transformer model into equivalent positive-sequence
  using scripting
}
var
    iW,
    i,
    N: Integer;
    S: String;
    Nodes: array[1..50] of Integer; // Big integer buffer
    OnPhase1: Boolean;
begin

  {First, determine if we can convert this one.}
    if (FnPhases = 1) or (FNphases = 2) then
    begin //disable if any terminal not connected to phase one
        for iW := 1 to NumWindings do
        begin
            OnPhase1 := FALSE;
       {Load up auxiliary parser}
            AuxParser.CmdString := GetBus(iW);
            AuxParser.NextParam;
            S := AuxParser.ParseAsBusName(N, @Nodes);
            if N = 0 then
                OnPhase1 := TRUE;
            for i := 1 to N do
                if Nodes[i] = 1 then
                    OnPhase1 := TRUE;
            if not OnPhase1 then
            begin
                Enabled := FALSE;   // We won't use this one
                Exit;
            end;
        end;
    end;

   {Construct transformer definition string }
    S := 'Phases=1  Conns=(';
    for i := 1 to NumWindings do
        S := S + 'Wye ';
    S := S + ')  buses=(';

    for i := 1 to NumWindings do
        S := S + Getbus(i) + ' ';
    S := S + ')  kVS=(';

    for i := 1 to NumWindings do
        with Winding^[i] do
            if (NPhases > 1) or (Connection <> 0) then
                S := S + Format(' %-.5g', [kVLL / SQRT3])
            else
                S := S + Format(' %-.5g', [kVLL]);
    S := S + ')  kVAs=(';

    for i := 1 to NumWindings do
        with Winding^[i] do
            S := S + Format(' %-.5g', [kVA / FNPhases]);
    S := S + ')';

    S := S + ' NormHkVA=' + Format(' %-.5g %-.5g', [NormMaxHkVA / FNPhases, EmergMaxHkVA / FNPhases]);

    Parser.CmdString := S;
    Edit;

    inherited;

end;

procedure TTransfObj.AddNeutralToY(FreqMultiplier: Double);
var
    i: Integer;
    Value: complex;
    j: Integer;
begin
  {Account for neutral impedances}
    with YPrim_Series do
    begin
        for i := 1 to NumWindings do
        begin
            with Winding^[i] do
            begin
                if Connection = 0 then
                begin
          // handle wye, but ignore delta  (and open wye)
                    if Rneut >= 0 then
                    begin
              // <0 is flag for open neutral  (Ignore)
                        if (Rneut = 0) and (Xneut = 0) then
                  // Solidly Grounded
                            Value := Cmplx(1000000, 0)
                        else
                  // 1 microohm resistor
                            Value := Cinv(Cmplx(Rneut, XNeut * FreqMultiplier));
                        j := i * fNconds;
                        AddElement(j, j, Value);
                    end

                    else
                    begin
            // Bump up neutral admittance a bit in case neutral is floating
                        j := i * fNconds;
                        if ppm_FloatFactor <> 0.0 then
                            SetElement(j, j, Cadd(GetElement(j, j), Cmplx(0.0, Y_PPM)));
             { SetElement(j, j, CmulReal_im(GetElement(j, j), ppm_FloatFactorPlusOne));}
                    end;

                end;
            end;
        end;
    end;
end;

procedure TTransfObj.BuildYPrimComponent(YPrim_Component, Y_Terminal: TCMatrix);
var
    NW2: Integer;
    i: Integer;
    k: Integer;
    Value: complex;
    j: Integer;

begin
    with YPrim_Component do
    begin
    { Now, Put in Yprim matrix }
    {have to add every element of Y_terminal into Yprim somewhere}
        NW2 := 2 * NumWindings;
        for i := 1 to NW2 do
        begin
            for j := 1 to i do
            begin
                Value := Y_Terminal.GetElement(i, j);
        // This value goes in Yprim nphases times
                for k := 0 to Fnphases - 1 do
                    AddElemSym(TermRef^[i + k * NW2], TermRef^[j + k * NW2], Value);
            end;
        end;
    end;
end;

function TTransfObj.Get_BasekVLL(i: Integer): Double;
begin
    Result := Winding^[i].kVLL;
end;


procedure TTransfObj.GICBuildYTerminal;
// Build YTerminal considering on resistance and no coupling to other winding.

var
    i, j, idx: Integer;
    yR: Complex;
    F: TextFile;
    Yadder: Complex;

begin

    Y_Term.Clear;
    Y_Term_NL.Clear;

    for i := 1 to NumWindings do
    begin
         {Use Rdc to build GIC model}
        yR := Cmplx(1.0 / (Winding^[i].Rdcohms), 0.0); // convert to Siemens
        with Y_Term do
        begin
            idx := 2 * i - 1;
            SetElement(idx, idx, yR);
            SetElement(idx + 1, idx + 1, yR);
            SetElemSym(idx, idx + 1, Cnegate(yR));   // set off-diagonals
        end;
    end;

    {For GIC add a small *Conductance* to both conductors of each winding so that
    the matrix will always invert even if the user neglects to define a voltage
    reference on all sides}
    if ppm_FloatFactor <> 0.0 then
        with Y_Term do
            for i := 1 to NumWindings do
            begin
                Yadder := cmplx(-Winding^[i].Y_PPM, 0.0);    // G + j0
                for j := (2 * i - 1) to (2 * i) do
                    SetElement(j, j, Cadd(GetElement(j, j), Yadder));
{           SetElement(j, j, CmulReal_im(GetElement(j, j) , ppm_FloatFactorPlusOne));}
            end;

 {$IFDEF TRANSDEBUG}
    AssignFile(F, CircuitName_ + 'Transformer_' + Name + '.TXT');
    Rewrite(F);
    Writeln(F, 'Y_Term after building...');
    DumpComplexMatrix(F, Y_Term);
    CloseFile(F);
 {$ENDIF}

end;

procedure TTransfObj.CalcY_Terminal(FreqMult: Double);

var
    i,
    j,
    k: Integer;
    A: pComplexArray;
    ctempArray1,
    ctempArray2: pComplexArray;
    cMinusOne: Complex;
    AT: TcMatrix;
    Yadder: Complex;
    Rmult: Double;
{$IFDEF TRANSDEBUG}
    F: Textfile;

{$ENDIF}
    {Function to fix a specification of a pu tap of 0.0}
    {Regcontrol can attempt to force zero tap position in some models}
    function ZeroTapFix(const tapvalue: Double): Double;
    begin
        if TapValue = 0.0 then
            Result := 0.0001
        else
            Result := Tapvalue;
    end;

begin

    if ActiveCircuit.Solution.Frequency < 0.51 then
         {Build Yterminal for GIC ~dc simulation}

        GICBuildYTerminal

    else
    begin  {Normal Y matrix build}

        if XRConst then
            RMult := FreqMult
        else
            RMult := 1.0;


  // Construct ZBMatrix;
        ZB.Clear;
        ZBase := 1.0 / (VABase / Fnphases); // base ohms on 1.0 volt basis
        for i := 1 to Numwindings - 1 do
          { convert pu to ohms on one volt base as we go... }
            ZB.SetElement(i, i, CmulReal(Cmplx(Rmult * (Winding^[1].Rpu + Winding^[i + 1].Rpu), Freqmult * XSC^[i]), ZBase));

       // Off diagonals
        k := NumWindings;
        with ZB do
            for  i := 1 to Numwindings - 1 do
            begin
                for j := i + 1 to Numwindings - 1 do
                begin
                    SetElemSym(i, j,
                        CmulReal(
                        Csub(CAdd(GetElement(i, i), GetElement(j, j)),
                        CmulReal(Cmplx(Rmult * (Winding^[i + 1].Rpu + Winding^[j + 1].Rpu), Freqmult * XSC^[k]),
                        ZBase)
                        ), 0.5));
                    Inc(k);
                end;
            end;

  {******************************DEBUG******************************************************}
  {$IFDEF TRANSDEBUG}
        AssignFile(F, CircuitName_ + 'Transformer_' + Name + '.TXT');
        Rewrite(F);
        Writeln(F, Format('Zbase=%g, VABase=%g, Nphases=%d, Rpu=%g ', [Zbase, VABase, Fnphases, Winding^[1].Rpu]));
        Writeln(F, 'ZB before inverting...');
        DumpComplexMatrix(F, ZB);
  {$ENDIF}
  {*****************************************************************************************}

        ZB.Invert;   // mhos on one volt base

        if ZB.InvertError > 0 then
        begin
            DoErrorMsg('TTransformerObj.CalcYPrim', 'Matrix Inversion Error for Transformer "' + Name + '"',
                'Invalid impedance specified. Replaced with tiny conductance to ground.', 117);
            ZB.Clear;
            for i := 1 to ZB.Order do
                ZB.SetElement(i, i, Cmplx(EPSILON, 0.0));
        end;

  {******************************DEBUG******************************************************}
  {$IFDEF TRANSDEBUG}
        Writeln(F, 'ZB after inverting...');
        DumpComplexMatrix(F, ZB);
  {$ENDIF}
  {*****************************************************************************************}

   // Now construct Y_Oneturn = AT * ZB.Invert * A
   {     -1 1 0 ...
     A = -1 0 1 ..   order:  N-1 x N   N = NumWindings
         ...
                           -1 -1 ...
     AT = Transpose of A =  1  0 ...    N X N-1
                            0  1 ..
   }

        Y_1Volt.Clear;
        Y_1Volt_NL.Clear;

     {Allocate temp complex arrays}
        ctempArray1 := AllocMem(SizeOf(Complex) * NumWindings * 2);
        ctempArray2 := AllocMem(SizeOf(Complex) * NumWindings * 2);

        A := AllocMem(SizeOf(Complex) * NumWindings * 2);
        cMinusOne := cmplx(-1.0, 0.0);
        AT := TcMatrix.Creatematrix(NumWindings);
        for i := 1 to NumWindings - 1 do
            AT.SetElement(i + 1, i, cONE);
        for i := 1 to NumWindings - 1 do
            AT.SetElement(1, i, cMinusOne);
        ctemparray1^[NumWindings] := CZERO;
        for i := 1 to Numwindings do
        begin
            if i = 1 then
                for k := 1 to NumWindings - 1 do
                    A^[k] := cMinusOne
            else
                for k := 1 to NumWindings - 1 do
                    if k = (i - 1) then
                        A^[k] := cONE
                    else
                        A^[k] := cZERO;
            ZB.MVmult(ctemparray1, A); {Zb.invert * A}
            AT.MVmult(ctempArray2, ctemparray1); {AT * Result}
            for j := 1 to NumWindings do
                Y_1Volt.SetElement(j, i, ctempArray2^[j]);
        end;

   {Add magnetizing Reactance to 2nd winding, assuming it is closest to the core
    Add both resistive element representing core losses and a reactive element representing
    magnetizing current
   }
        Y_1Volt_NL.AddElement(2, 2, Cmplx((pctNoLoadLoss / 100.0 / Zbase), -pctImag / 100.0 / Zbase / Freqmult));

  {******************************DEBUG******************************************************}
  {$IFDEF TRANSDEBUG}
        Writeln(F, 'Y_OneVolt ...');
        DumpComplexMatrix(F, Y_1Volt);
        Writeln(F, 'Y_OneVolt_NL ...');
        DumpComplexMatrix(F, Y_1Volt_NL);
  {$ENDIF}
  {*****************************************************************************************}
     // should have admittance of one phase of the transformer on a one-volt, wye-connected base

     // Now make into terminal admittance matrix and correct for actual voltage ratings
     // Y_Terminal = AT * Y_onevolt * A  where V_onevolt = A * V_terminal

        AT.Free;

        Y_Term.Clear;
        Y_Term_NL.Clear;
        AT := TcMatrix.Creatematrix(NumWindings * 2);

     // 8/22/2013 Added ZeroTapFix so that regcontrol can set a tap to zero

        for i := 1 to NumWindings do
            with Winding^[i] do
                AT.SetElement(2 * i - 1, i, Cmplx(1.0 / (VBase * ZeroTapFix(puTap)), 0.0));
        for i := 1 to NumWindings do
            with Winding^[i] do
                AT.SetElement(2 * i, i, Cmplx(-1.0 / (VBase * ZeroTapFix(puTap)), 0.0));
        for i := 1 to 2 * Numwindings do
            ctemparray1^[i] := CZERO;

        for i := 1 to 2 * Numwindings do
        begin
            for k := 1 to NumWindings do
                with Winding^[k] do
                begin
                    if i = (2 * k - 1) then
                        A^[k] := Cmplx((1.0 / (VBase * ZeroTapFix(puTap))), 0.0)
                    else
                    if i = 2 * k then
                        A^[k] := Cmplx((-1.0 / (VBase * ZeroTapFix(puTap))), 0.0)
                    else
                        A^[k] := cZERO;
                end;
       {Main Transformer part}
            Y_1Volt.MVmult(ctemparray1, A);
            AT.MVmult(ctemparray2, ctemparray1);    {AT * Result}
            for j := 1 to 2 * NumWindings do
                Y_Term.SetElement(j, i, ctemparray2^[j]);
       {No Load part}
            Y_1Volt_NL.MVmult(ctemparray1, A);
            AT.MVmult(ctemparray2, ctemparray1);    {AT * Result}
            for j := 1 to 2 * NumWindings do
                Y_Term_NL.SetElement(j, i, ctemparray2^[j]);
        end;

  {******************************DEBUG******************************************************}
  {$IFDEF TRANSDEBUG}
        Writeln(F, 'Y_Terminal before adding small element to diagonals ...');
        DumpComplexMatrix(F, Y_Term);
        Writeln(F, 'Y_Terminal_NL before adding small element to diagonals ...');
        DumpComplexMatrix(F, Y_Term_NL);
  {$ENDIF}
  {*****************************************************************************************}

     {Add a small Admittance to both conductors of each winding so that
    the matrix will always invert even if the user neglects to define a voltage
    reference on all sides}
        if ppm_FloatFactor <> 0.0 then
            with Y_Term do
                for i := 1 to NumWindings do
                begin
                    Yadder := cmplx(0.0, Winding^[i].Y_PPM);
                    for j := (2 * i - 1) to (2 * i) do
                        SetElement(j, j, Cadd(GetElement(j, j), Yadder));
{           SetElement(j, j, CmulReal_im(GetElement(j, j) , ppm_FloatFactorPlusOne));}
                end;

{******************************DEBUG******************************************************}
  {$IFDEF TRANSDEBUG}
        Writeln(F, 'Y_Terminal after adding small element to diagonals ...');
        DumpComplexMatrix(F, Y_Term);
        Writeln(F, 'Y_Terminal_NL after adding small element to diagonals ...');
        DumpComplexMatrix(F, Y_Term_NL);
        CloseFile(F);
  {$ENDIF}
  {*****************************************************************************************}
        AT.Free;
        Reallocmem(A, 0);
        Reallocmem(ctemparray1, 0);
        Reallocmem(ctemparray2, 0);

    end;

    Y_Terminal_FreqMult := Freqmult;

end;

procedure TTransfObj.FetchXfmrCode(const Code: String);
var
    Obj: TXfmrCodeObj;
    i: Integer;
begin
    if XfmrCodeClass = NIL then
        XfmrCodeClass := DSSClassList.Get(ClassNames.Find('xfmrcode'));

    if XfmrCodeClass.SetActive(Code) then
    begin
        Obj := XfmrCodeClass.GetActiveObj;
        XfmrCode := LowerCase(Code);
    // set sizes and copy parameters
        Nphases := Obj.Fnphases;
        SetNumWindings(Obj.NumWindings);
        NConds := Fnphases + 1; // forces reallocation of terminals and conductors
        for i := 1 to NumWindings do
            with Winding^[i] do
            begin
                Connection := Obj.Winding^[i].Connection;
                kVLL := Obj.Winding^[i].kVLL;
                VBase := Obj.Winding^[i].VBase;
                kVA := Obj.Winding^[i].kVA;
                puTAP := Obj.Winding^[i].puTAP;
                Rpu := Obj.Winding^[i].Rpu;
                Rdcohms := Obj.Winding^[i].Rdcohms;
                RdcSpecified := Obj.Winding^[i].RdcSpecified;
                RNeut := Obj.Winding^[i].RNeut;
                Xneut := Obj.Winding^[i].Xneut;
                TapIncrement := Obj.Winding^[i].TapIncrement;
                MinTap := Obj.Winding^[i].MinTap;
                MaxTap := Obj.Winding^[i].MaxTap;
                NumTaps := Obj.Winding^[i].NumTaps;
            end;
        SetTermRef;

    // Parameters for all windings
        XHL := Obj.XHL;
        XHT := Obj.XHT;
        XLT := Obj.XLT;
        for i := 1 to (NumWindings * (NumWindings - 1) div 2) do
            XSc^[i] := Obj.XSC^[i];

        ThermalTimeConst := Obj.ThermalTimeConst;
        n_thermal := Obj.n_thermal;
        m_thermal := Obj.m_thermal;
        FLrise := Obj.FLrise;
        HSrise := Obj.HSrise;
        pctLoadLoss := Obj.pctLoadLoss;
        pctNoLoadLoss := Obj.pctNoLoadLoss;
        pctImag := Obj.pctImag;  // Omission corrected 12-14-18
        NormMaxHkVA := Obj.NormMaxHkVA;
        EmergMaxHkVA := Obj.EmergMaxHkVA;
        ppm_FloatFactor := Obj.ppm_FloatFactor;
        Yorder := fNConds * fNTerms;
        YPrimInvalid := TRUE;
        Y_Terminal_FreqMult := 0.0;

        RecalcElementData
    end
    else
        DoSimpleMsg('Xfmr Code:' + Code + ' not found.', 180);
end;

end.
