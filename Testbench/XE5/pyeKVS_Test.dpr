program pyeKVS_Test;

uses
  Vcl.Forms,
  uMain in '..\uMain.pas' {Main},
  pyeKVS in '..\..\Source\pyeKVS.pas',
  uFooData in '..\uFooData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
