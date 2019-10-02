program SPCAM_USB2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, ConnectForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='Spresense Camera Viewer2';
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.

