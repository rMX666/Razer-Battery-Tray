program razbat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uSettings, uRazerUtil, uUtil
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Name := 'Razbat';
  Application.Title := 'Razbat';
  Application.Scaled := True;
  Application.Initialize;
  Application.ShowMainForm := False;
  Application.CreateForm(TfSettings, fSettings);
  Application.Run;
end.

