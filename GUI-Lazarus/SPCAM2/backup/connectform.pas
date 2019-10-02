unit ConnectForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFrmConnect }

  TFrmConnect = class(TForm)
    Button1: TButton;
    Button2: TButton;
    EdtPort: TEdit;
    EdtBaudRate: TEdit;
    Label1: TLabel;
    Label2: TLabel;
  private

  public

  end;

//var
//  FrmConnect: TFrmConnect;

implementation

{$R *.lfm}

end.

