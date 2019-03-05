unit HelpForm;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Windows,
    Messages,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    ComCtrls;

type
    THelpForm1 = class(TForm)
        TreeView1: TTreeView;
        Memo1: TMemo;
        Button1: TButton;
        Button2: TButton;
        SaveDialog1: TSaveDialog;
        rdoAlphabetical: TRadioButton;
        rdoNumerical: TRadioButton;
        Label1: TLabel;
        procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure BuildTreeViewList;
        procedure rdoAlphabeticalClick(Sender: TObject);
        procedure rdoNumericalClick(Sender: TObject);
    PRIVATE
    { Private declarations }
        procedure AddHelpForClasses(BaseClass: Word);
    PUBLIC
    { Public declarations }
    end;

var
    HelpFormObj: THelpForm1;

implementation

{$R *.DFM}

uses
    DSSClassDefs,
    DSSGlobals,
    ExecCommands,
    ExecOptions,
    ShowOptions,
    PlotOptions,
    ExportOptions,
    DSSClass;

const
    TreeSep: String = '== classes ==';

function CompareClassNames(Item1, Item2: Pointer): Integer;
begin
    Result := CompareText(TDSSClass(Item1).name, TDSSClass(Item2).name);
end;

procedure THelpForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
    Memo1.Clear;
    if Treeview1.selected.data <> NIL then
        Memo1.Text := String(Treeview1.selected.data^);
end;

function TranslateCRLF(const s: String): String;

{ Translate Every CRLF to chr(11) soft return for Word}
var
    i: Integer;
begin
    Result := '';
    for i := 1 to Length(S) do
    begin
        case S[i] of
            Chr(13):
                Result := Result + Chr(11);
            Chr(10):  {Do Nothing}
        else
            Result := Result + S[i]
        end;
    end;
end;

procedure WriteItem(var F: Textfile; Str: String; p: Pointer);

begin
    Write(F, Str, chr(9));
    if p <> NIL then
    begin
        Write(F, TranslateCRLF(String(p^)));
    end;
    Writeln(F);
end;

procedure THelpForm1.Button1Click(Sender: TObject);
var
    Fname: String;
    ObjectItem,
    CmdItem,
    OptItem: TTreeNode;
    F: TextFile;

begin

// Save present contents of HelpForm to a Disk File

// first prompt for a file
    with SaveDialog1 do
    begin
        Filter := 'Text files (*.txt)|*.TXT';
        FileName := 'DSSHelp.Txt';

        if Execute then
        begin
            Fname := FileName;

            try
                AssignFile(F, Fname);
                Rewrite(F);

                try
                    ObjectItem := TreeView1.Items.GetFirstNode;
                    while ObjectItem <> NIL do
                    begin
                        if ObjectItem.Text <> TreeSep then
                        begin
                            Writeln(F);
                            Writeln(F, 'Object = ', UpperCase(ObjectItem.Text));
                            Writeln(F, 'Property', Chr(9), 'Description');
                            CmdItem := ObjectItem.GetFirstChild;
                            while CmdItem <> NIL do
                            begin
                                WriteItem(F, CmdItem.Text, CmdItem.Data);
                                OptItem := CmdItem.GetFirstChild;
                                while OptItem <> NIL do
                                begin
                                    Write(F, '   ');
                                    WriteItem(F, OptItem.Text, OptItem.Data);
                                    OptItem := OptItem.GetNextSibling;
                                end;
                                CmdItem := CmdItem.GetNextSibling;
                            end;
                        end;
                        ObjectItem := ObjectItem.GetNextSibling;
                    end;

                    Closefile(F);

                except
                    On E: Exception do
                    begin
                        DoErrorMsg('Problem writing file: ' + Fname + '.', E.Message, 'Disk protected or other file error', 140);
                    end;
                end;


            except
                On E: Exception do
                begin
                    DoErrorMsg('Problem opening ' + Fname + ' for writing.', E.Message, 'Disk protected or other file error', 141);
                end;
            end;

        end;

    end;


end;

procedure THelpForm1.Button2Click(Sender: TObject);
begin
    Close;
end;

procedure THelpForm1.rdoAlphabeticalClick(Sender: TObject);
begin
    BuildTreeViewList;
end;

procedure THelpForm1.rdoNumericalClick(Sender: TObject);
begin
    BuildTreeViewList;
end;

procedure THelpForm1.AddHelpForClasses(BaseClass: Word);
var
    HelpList: TList;
    Node1: TTreeNode;
    pDSSClass: TDSSClass;
    i, j: Integer;
begin

    with Treeview1.Items do
    begin

 // put the other DSS Classes in alphabetical order within Base Class
        HelpList := TList.Create();
        pDSSClass := DSSClassList.First;
        while pDSSClass <> NIL do
        begin
            if (pDSSClass.DSSClassType and BASECLASSMASK) = BaseClass then
                HelpList.Add(pDSSClass);
            pDSSClass := DSSClassList.Next;
        end;
        HelpList.Sort(@CompareClassNames);

        // now display the other DSS classes
        for i := 1 to HelpList.Count do
        begin
            pDSSClass := HelpList.Items[i - 1];
            Node1 := AddObject(NIL, pDSSClass.name, NIL);
            if rdoAlphabetical.Checked then
            begin
                for j := 1 to pDSSClass.NumProperties do
                    AddChildObject(Node1, pDSSClass.PropertyName[j], @pDSSClass.PropertyHelp^[j]);
                Node1.AlphaSort();
            end
            else
            begin
                for j := 1 to pDSSClass.NumProperties do
                    AddChildObject(Node1, '(' + IntToStr(j) + ') ' + pDSSClass.PropertyName[j], @pDSSClass.PropertyHelp^[j]);
            end;
        end;
        HelpList.Free;

    end;

end;

procedure THelpForm1.BuildTreeViewList;

var
    Node1: TTreeNode;
    i: Integer;


begin
    Treeview1.Items.Clear;
    with Treeview1.Items do
    begin
     // Do Executive Commands
        Node1 := AddObject(NIL, 'Executive', NIL);
        for i := 1 to NumExeccommands do
        begin
          // AddChildObject returns a Node2, if we wanted to link another level
            AddChildObject(Node1, ExecCommand[i], @CommandHelp[i]);
        end;
        Node1.AlphaSort();

     // Do Exec Options
        Node1 := AddObject(NIL, 'Options', NIL);
        for i := 1 to NumExecOptions do
        begin
            AddChildObject(Node1, ExecOption[i], @OptionHelp[i]);
        end;
        Node1.AlphaSort();

      // Do Show Options
        Node1 := AddObject(NIL, 'Show', NIL);
        for i := 1 to NumShowOptions do
        begin
            AddChildObject(Node1, ShowOption[i], @ShowHelp[i]);
        end;
        Node1.AlphaSort();    // always sort


      // Do Export Options
        Node1 := AddObject(NIL, 'Export', NIL);
        for i := 1 to NumExportOptions do
        begin
            AddChildObject(Node1, ExportOption[i], @ExportHelp[i]);
        end;
        Node1.AlphaSort();    // always sort

      // Do Plot Options
        Node1 := AddObject(NIL, 'Plot', NIL);
        for i := 1 to NumPlotOptions do
        begin
            AddChildObject(Node1, PlotOption[i], @PlotHelp[i]);
        end;
        if rdoAlphabetical.Checked then
            Node1.AlphaSort();

        // separator
        // AddObject (nil, TreeSep, nil);

        AddObject(NIL, '=== PD Elements ===', NIL);
        AddHelpForClasses(PD_ELEMENT);
        AddObject(NIL, '=== PC Elements ===', NIL);
        AddHelpForClasses(PC_ELEMENT);
        AddObject(NIL, '=== Controls ===', NIL);
        AddHelpForClasses(CTRL_ELEMENT);
        AddObject(NIL, '=== General ===', NIL);
        AddHelpForClasses(0);
        AddObject(NIL, '=== Meters ===', NIL);
        AddHelpForClasses(METER_ELEMENT);
        AddObject(NIL, '=== Other ===', NIL);
        AddHelpForClasses(NON_PCPD_ELEM);

    end;
    Caption := 'DSS Commands & Properties';

end;

end.
