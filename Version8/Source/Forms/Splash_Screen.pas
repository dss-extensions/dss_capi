unit Splash_Screen;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Imaging.pngimage,
  Vcl.StdCtrls, Vcl.ComCtrls;

type
  TSplashScreen = class(TForm)
    banner: TImage;
    logo: TImage;
    LicenseText: TRichEdit;
    Label1: TLabel;
    Version: TLabel;

    procedure FormCreate(Sender: TObject);
  protected
      procedure CreateParams(var Params: TCreateParams); override; // ADD THIS LINE!
  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  SplashScreen: TSplashScreen;

implementation

uses

  DSSGlobals;

{$R *.dfm}

procedure TSplashScreen.FormCreate(Sender: TObject);
begin

    Left:=(Screen.Width-Width)  div 2;
    Top:=(Screen.Height-Height) div 2;

     Version.Caption := VersionString;

     LicenseText.Lines.Clear();
     LicenseText.Lines.Add('Copyright (c) 2008-2024, Electric Power Research Institute, Inc.');
     LicenseText.Lines.Add('All rights reserved.');
     LicenseText.Lines.Add('');
     LicenseText.Lines.Add('Redistribution and use in source and binary forms, with or without');
     LicenseText.Lines.Add('modification, are permitted provided that the following conditions are met:');
     LicenseText.Lines.Add('    * Redistributions of source code must retain the above copyright');
     LicenseText.Lines.Add('      notice, this list of conditions and the following disclaimer.');
     LicenseText.Lines.Add('    * Redistributions in binary form must reproduce the above copyright');
     LicenseText.Lines.Add('      notice, this list of conditions and the following disclaimer in the');
     LicenseText.Lines.Add('      documentation and/or other materials provided with the distribution.');
     LicenseText.Lines.Add('    * Neither the name of the Electric Power Research Institute, Inc., nor');
     LicenseText.Lines.Add('      the names of its contributors may be used to endorse or promote');
     LicenseText.Lines.Add('      products derived from this software without specific prior written');
     LicenseText.Lines.Add('      permission.');
     LicenseText.Lines.Add('');
     LicenseText.Lines.Add('THIS SOFTWARE IS PROVIDED BY Electric Power Research Institute, Inc., "AS IS"');
     LicenseText.Lines.Add('AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE');
     LicenseText.Lines.Add('IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR');
     LicenseText.Lines.Add('PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Electric Power Research Institute, Inc.,');
     LicenseText.Lines.Add('BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR');
     LicenseText.Lines.Add('CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF');
     LicenseText.Lines.Add('SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS');
     LicenseText.Lines.Add('INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN');
     LicenseText.Lines.Add('CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)');
     LicenseText.Lines.Add('ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE');
     LicenseText.Lines.Add('POSSIBILITY OF SUCH DAMAGE.');

     LicenseText.SelStart := 0;
end;

procedure TSplashScreen.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_THICKFRAME;
end;

end.
