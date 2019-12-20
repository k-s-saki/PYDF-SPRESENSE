program SPCAM2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  //
  FileUtil,Classes,

  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, ConnectForm, spcam_functions;

{$R *.res}

begin

  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Title:='Spresense Camera Viewer - USB';
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;

end.

