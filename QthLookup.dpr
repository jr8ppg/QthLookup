program QthLookup;

{$R 'resource.res' 'resource.rc'}

uses
  Forms,
  Main in 'Main.pas' {Form1},
  Jccg in 'Jccg.pas',
  LicenseInfo in 'LicenseInfo.pas',
  Progress in 'Progress.pas' {formProgress},
  SelectZlog in 'SelectZlog.pas' {formSelectZLog},
  ImpotantNotice in 'ImpotantNotice.pas' {formImpotantNotice},
  DialogHook in 'DialogHook.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
