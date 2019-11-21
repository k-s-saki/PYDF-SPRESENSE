program SPCAM2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  //
  FileUtil,Classes,

  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, ConnectForm;

{$R *.res}

(* TEST
var
  dir,s:string;
  sl:TStringList;
begin
  // Tests.
  dir:= 'C:\Users\sae\Pictures\spresense\Image\Piyo';
  sl:=TStringList.Create;
  FindAllFiles(sl,dir,'*.PGM',false);

  for s in sl do
    ConvertGrayPgmToBmp(s);

  sl.Free;
end.
*)

begin

  RequireDerivedFormResource := True;
  Application.Title:='Spresense Camera Viewer - USB';
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;

end.

