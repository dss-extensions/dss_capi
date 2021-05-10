unit CAPI_Globals;

interface

uses
    DSSClass;

type
    dss_callback_plot_t = function (jsonParams: PChar): Integer; CDECL;
    dss_callback_message_t = function (messageStr: PChar; messageType: Integer): Integer; CDECL;

var
    FPropIndex: Integer;
    FPropClass: TDSSClass;
    DSSPlotCallback: dss_callback_plot_t;
    DSSMessageCallback: dss_callback_message_t;

implementation

initialization
    FPropIndex := 0;
    FPropClass := NIL;
    DSSPlotCallback := NIL;
end.
