unit TOPExport;

{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Supports Creation of a STO file for interfacing to TOP and the
 invoking of TOP.}

interface

uses
    Classes,
    ArrayDef;

type
    time_t = Longint;

    ToutfileHdr = packed record
        Size: Word;
        Signature: array[0..15] of Char;
        VersionMajor,
        VersionMinor: Word;
        FBase,
        VBase: Double;
        tStart,
        tFinish: time_t;
        StartTime,
        StopT,
        DeltaT: Double;
        Nsteps: Longword;
        NVoltages,
        NCurrents,
        VoltNameSize,
        CurrNameSize: Word;
        IdxVoltNames,
        IdxCurrentNames,
        IdxBaseData,
        IdxData: Longint;
        Title1,
        Title2,
        Title3,
        Title4,
        Title5: array[0..79] of Char;  // Fixed length 80-byte string  space
    end;

    TOutFile32 = class(Tobject)
        Header: ToutfileHdr;
        Fname: String;  {Default is RLCWOUT.STO'}
        Fout: file;

    PRIVATE

    PUBLIC
          {constructor Create(Owner: TObject);}
        procedure Open;
        procedure Close;
        procedure WriteHeader(const t_start, t_stop, h: Double; const NV, NI, NameSize: Integer; const Title: String);
        procedure WriteNames(var Vnames, Cnames: TStringList);
        procedure WriteData(const t: Double; const V, Curr: pDoubleArray);
        procedure OpenR;  {Open for Read Only}
        procedure ReadHeader; {Opposite of WriteHeader}
        procedure GetVoltage(T, V: pDoubleArray; Idx, MaxPts: Integer); {Read a single node voltage from disk}
        procedure SendToTop;

        property FileName: String READ Fname WRITE Fname;

    end;

var
    TOPTransferFile: TOutFile32;
    TOP_Object: Variant;  // For Top Automation

implementation

uses
    SysUtils,
    DSSGlobals;

procedure StartTop;

begin

end;

procedure TOutFile32.SendToTop;
begin

end;


procedure TOutFile32.Open;
begin
end;


procedure TOutFile32.Close;
begin
end;

procedure TOutFile32.WriteHeader(const t_start, t_stop, h: Double; const NV, NI, NameSize: Integer; const Title: String);

begin

end;

procedure TOutFile32.WriteNames(var Vnames, Cnames: TStringList);

begin

end;

procedure TOutFile32.WriteData(const t: Double; const V, Curr: pDoubleArray);

begin

end;

procedure TOutFile32.OpenR;  {Open for Read Only}

begin
end;

procedure TOutFile32.ReadHeader; {Opposite of WriteHeader}

begin

end;

procedure TOutFile32.GetVoltage(T, V: pDoubleArray; Idx, MaxPts: Integer); {Read a voltage from disk}

{Gets a specified voltage from an STO file for plotting.  Idx specifies the index into the voltage array}
begin

end;

initialization

end.
