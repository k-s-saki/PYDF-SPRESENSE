unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Windows, Menus, ExtCtrls, ActnList, LCLType, types, Clipbrd,
  StdCtrls, {PairSplitter,Math,} Synaser;

const
  SEC:TDateTime  =0;
  MSEC:TDateTime =0;
  RecvEnd:string = #10;


type

  { TFrmMain }
  TFrmMain = class(TForm)
    BtnE: TButton;
    BtnS: TButton;
    BtnConnect: TButton;
    BtnShot: TButton;
    DebugMemo: TMemo;
    LblFPS: TLabel;
    MainMenu1: TMainMenu;
    PnlBottom: TPanel;
    PnlMemo: TPanel;
    PnlOperation: TPanel;
    rbColor: TRadioButton;
    rbGray: TRadioButton;
    MemoSplit: TSplitter;
    ImageSplit: TSplitter;
    MenuItem1: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemFile: TMenuItem;
    OpenDialog1: TOpenDialog;
    ImagePaint: TPaintBox;
    SaveDialog1: TSaveDialog;
    SpreMemo: TMemo;
    Timer1: TTimer;
    procedure BtnConnect_Click(Sender: TObject);
    procedure BtnShot_Click(Sender: TObject);
    procedure BtnS_Click(Sender: TObject);
    procedure BtnE_Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure ImagePaint_Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    FBmp:Graphics.TBitmap;
    FSer: TBlockSerial;
    FPrevDrawDt:TDateTime;
    FPrevEndDetect:boolean;
    FTransferCmd:string;

    procedure CalcFPS();
    procedure TryPreviewEnd(maxTryCount:integer);

    function  GetDrawRect(DstRect, SrcRect: TRect): TRect;

    function  Serial_RecvStr(aSerial:TBlockSerial;  aTimeout:integer;out oRecvStr:string; aDebugStr:string=''):boolean;
    (*
      文字列をSerialから読み取る( CRまでの文字列)   エラーの場合にはSerialのデバッグ情報の文字出力を行う
      aSerial:    シリアル通信
      aTimeout:   タイムアウト(msec)
      oRecvStr:   受信した文字列
      aDebugStr:  エラーになった時に表示する文字列
      result: true 文字列として受信できた
    *)

    function  Serial_RecvImage(aSerial:TBlockSerial):boolean;

    function  Serial_IsError(aSerial:TBlockSerial; aDebugStr:string):boolean;


    function  CheckDebugMsg(s:string):boolean;
    procedure Debug(s:string);
    procedure Error(s:string);
  end;

var
  FrmMain: TFrmMain;

implementation

uses ConnectForm;

{$R *.lfm}

{ TFrmMain }

const
  BW=320;
  BH=240;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  ImagePaint.Align:=alClient;
  SpreMemo.Align:=alClient;
  PnlBottom.Align:=alBottom;


  FBmp:=Graphics.TBitmap.Create;
  FBmp.SetSize(BW,BH);
  FBmp.PixelFormat:=pf24bit;

  FPrevEndDetect:=false;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  FBmp.Free;
  if Assigned(FSer) then
    FreeAndNil(FSer);
end;

procedure TFrmMain.MenuItem4Click(Sender: TObject);
begin
  Application.Terminate;
end;


function TFrmMain.GetDrawRect(DstRect, SrcRect: TRect): TRect;


var
  DstSize, SrcSize: TSize;
  I: Integer;

  function CheckRectSize():boolean;
  begin
    result:=
            (DstSize.cx > 0) and
            (DstSize.cy > 0) and
            (SrcSize.cx > 0) and
            (SrcSize.cy > 0);
  end;

  function MakeSize(aRect:TRect):TSize;
  begin
    result.cx:= aRect.Right - aRect.Left;
    result.cy:= aRect.Bottom - aRect.Top;
  end;

begin

  DstSize:= MakeSize( DstRect );
  SrcSize:= MakeSize( SrcRect );
  if CheckRectSize()=true then
  begin
    Result:= DstRect;
    if(DstSize.cx * SrcSize.cy) > (DstSize.cy * SrcSize.cx) then
    begin
      // 上下、左右余白
      I := DstSize.cy * SrcSize.cx div SrcSize.cy;
      Result.Left   := (DstRect.Left + DstRect.Right - I) div 2;
      Result.Right  := Result.Left + I;
    end
    else
    begin
      // 左右、上下余白
      I := DstSize.cx * SrcSize.cy div SrcSize.cx;
      Result.Top    := (DstRect.Top + DstRect.Bottom - I) div 2;
      Result.Bottom := Result.Top + I;
    end;
  end
  else
  begin
    Result := Classes.Rect(0, 0, 0, 0);
  end;
end;


procedure TFrmMain.Debug(s: string);
begin
  DebugMemo.Lines.Add(s);
end;

procedure TFrmMain.Error(s: string);
begin
  DebugMemo.Lines.Add(s);
end;

procedure TFrmMain.CalcFPS();
var
  dt,span:TDateTime;
begin
  dt:=Now;
  if FPrevDrawDt<>0 then
  begin
    span:= (dt-FPrevDrawDt);
    if (span > MSEC*10) then
      LblFPS.Caption:= Format('%.2f',[SEC / span] )+'FPS'
    else
      LblFPS.Caption:='0.0 FPS';
  end;
  FPrevDrawDt:=dt;
end;

procedure TFrmMain.ImagePaint_Paint(Sender: TObject);
var
  PaintRect: TRect;
begin
  with ImagePaint do
  begin
    Canvas.Brush.Color := clBlack;
    Canvas.FillRect(ClientRect);
  end;

  if FBmp.Width>0 then
  begin
    PaintRect := GetDrawRect(ImagePaint.ClientRect, Rect(0,0,FBmp.Width,FBmp.Height));
    ImagePaint.Canvas.StretchDraw( PaintRect,FBmp)
  end;

end;

procedure TFrmMain.BtnConnect_Click(Sender: TObject);
var
  frm:TFrmConnect;
  br:integer;
begin
  if FSer<>nil then begin
    FSer.Free;
    FSer:=nil;
  end;

  frm:=TFrmConnect.Create(nil);
  try
    if frm.ShowModal()=mrOk then
    begin

      FSer := TBlockSerial.Create;
      FSer.Connect(frm.EdtPort.Text);                       // COM ポート指定接続
      Sleep(100);
      br:= StrToIntDef( frm.EdtBaudRate.Text, 0);
      FSer.Config( br, 8, 'N', SB1, False, False); // ボーレート他の設定
      FSer.ConvertLineEnd:=false;

      if Serial_IsError(FSer, 'Connect ' + frm.EdtPort.Text ) then
        Debug('Connect Fail')
      else
        Debug('Connect Success');
    end
    else begin
      Debug('Cancel Connect.');
    end;

  finally
    frm.Free;
  end;
end;

procedure TFrmMain.BtnShot_Click(Sender: TObject);
begin
  if FSer=nil then exit;

  Debug('SHOT');
  FSer.SendString('S');

end;

procedure TFrmMain.BtnS_Click(Sender: TObject);
begin
  if FSer=nil then exit;

  if not FPrevEndDetect then
    TryPreviewEnd(5);

  if rbGray.Checked then begin
    FTransferCmd:='G';
    FBmp.PixelFormat:=pf8bit;
  end
  else begin
    FTransferCmd:='R';
    FBmp.PixelFormat:=pf24bit;
  end;

  FSer.SendString(FTransferCmd);
  Debug('START REQUEST = '+FTransferCmd);

  FPrevDrawDt:=0;
end;

procedure TFrmMain.BtnE_Click(Sender: TObject);
begin
  TryPreviewEnd(5);
end;

procedure TFrmMain.TryPreviewEnd(maxTryCount:integer);
var
  s:string;
  cnt:integer;
begin
  if FSer=nil then exit;

  Debug('END REQUEST START');
  cnt:=1;
  while (true) do begin
    s:='';
    FSer.SendString('E');
    s := FSer.RecvTerminated( 50, RecvEnd);

    if Length(s)>64 then begin
      //バッファ異常のためバッファを一旦破棄(カウントしない)
      Debug('Purge Buffer.');
      FSer.Purge();
      continue;
    end;

    //デバッグ用のメッセージが残っている場合は出力しておく
    while (CheckDebugMsg(s)=true) do
      s:= FSer.RecvTerminated(10, RecvEnd);

    Debug(IntToStr(cnt)+' -> '+s);
    if s='OK' then
    begin
      Debug('SUCCESS END REQUEST');
      FPrevEndDetect:= True;
      break;
    end;

    if cnt>maxTryCount then begin
      Debug('FAIL END REQUEST');
      FPrevEndDetect:= false;
      break;
    end;

    inc(cnt);
    Sleep(50);
  end;
end;


function convertRGB565(ms:TMemoryStream; bmp: Graphics.TBitmap):boolean;
  type
    TFormat565 = packed record
      case Integer of
      0: (AsWord:WORD);
      1: (AsByte:Array[0..1] of Byte);
    end;

var
  pMem: PByte;
  LineIndex, PixelIndex: Integer;
  Data565:TFormat565;
  dR,dG,dB:byte;
  w:WORD;
begin
  result:=false;
  bmp.BeginUpdate();
  for LineIndex := 0 to bmp.Height - 1 do
  begin
    pMem := bmp.ScanLine[LineIndex];
    for PixelIndex := 0 to bmp.Width - 1 do
    begin
      Data565.AsByte[0]:=ms.ReadByte();
      Data565.AsByte[1]:=ms.ReadByte();
      w:=Data565.AsWord;
      dR:= (((w and $f800) shr 11) shl 3);
      dG:= (((w and $07E0) shr  5) shl 2);
      dB:=  ((w and $001F) shl 3);
      //Windowsでは BitmapはBGRの順
      (pMem+0)^:= dB;
      (pMem+1)^:= dG;
      (pMem+2)^:= dR;
      pMem:=pMem+3;
    end;
  end;
  bmp.EndUpdate();
  result:=true;
end;

function convertGray(ms:TMemoryStream; bmp: Graphics.TBitmap):boolean;

type
  TMemoryFormatGray = packed array [0 .. MaxInt] of Byte;
  PMemoryFormatGray = ^TMemoryFormatGray;

var
  pMem: PMemoryFormatGray;
  LineIndex, PixelIndex: Integer;
  cnt:integer;
begin
  result:=false;
  cnt:=0;
  bmp.BeginUpdate();
  for LineIndex := 0 to bmp.Height - 1 do
  begin
    pMem := bmp.ScanLine[LineIndex];
    for PixelIndex := 0 to bmp.Width - 1 do
    begin
      pMem^[PixelIndex]:= ms.ReadByte();
      inc(cnt);
    end;
  end;
  bmp.EndUpdate();
  result:=true;
end;


function TFrmMain.CheckDebugMsg(s:string):boolean;
begin
  result:=false;
  if Length(s)=0 then exit;

  if s[1]='@' then begin
    result:=true;
    SpreMemo.Lines.Add(s);
  end;
end;


function TFrmMain.Serial_RecvStr(aSerial: TBlockSerial;  aTimeout:integer; out oRecvStr: string;
  aDebugStr: string): boolean;
var s:string;
begin
  s:= FSer.RecvTerminated( aTimeout, RecvEnd{ #13});
  if not Serial_IsError(aSerial,aDebugStr) then begin
    oRecvStr:=s;
    result:=true;
  end
  else
    result:=false;
end;

function TFrmMain.Serial_IsError(aSerial:TBlockSerial; aDebugStr:string):boolean;
begin
  if aSerial.LastError<>0 then begin
    Error(aDebugStr+':'+aSerial.LastErrorDesc);
    result:=true;
  end
  else
    result:=false;
end;

procedure TFrmMain.Timer1Timer(Sender: TObject);
var
  s: string;
begin
  if not Assigned(FSer) then
    exit;

  while FSer.CanRead(10) do
  begin

    //1行取得
    if Serial_RecvStr(FSer,10, s,'FirstRead') = false then
      exit;

    if Length(s)=0 then
      continue;

    if Length(s)>64 then begin
      //バッファ異常のためバッファを一旦破棄
      FSer.Purge();
      continue;
    end;

    //デバッグ用の文字列
    if (CheckDebugMsg(s)=true) then
      continue;

    //コマンド処理
    Debug('Command Detect:'+s);
    if s='Image' then begin
      Serial_RecvImage(FSer);
      CalcFPS();
    end;

  end;
end;


function TFrmMain.Serial_RecvImage(aSerial:TBlockSerial):boolean;
var
  MemStream:TMemoryStream;
  bytes:integer;
  s,image_type:string;
begin
  result:=false;

  Debug('Serial_RecvImage()');

  //FrameNo:
  if Serial_RecvStr(aSerial,100, s,'FrameNo') = false then begin
    aSerial.Purge;
    exit;
  end;
  Debug('1:'+s);

  //ImageType
  if Serial_RecvStr(aSerial,100, s,'ImageType') = false then begin
    aSerial.Purge();
    exit;
  end;
  Debug(s);
  image_type:= Copy(s, AnsiPos('=',s)+1, length(s));

  //ImageSize
  if Serial_RecvStr(aSerial,100, s,'ImageSize') = false then begin
    aSerial.Purge();
    exit;
  end;
  Debug(s);

  bytes:= StrToIntDef(Copy(s, AnsiPos('=',s)+1, length(s)) , -1);
  Debug( 'Recv StreamSize :'+IntToStr(bytes) );
  if bytes>0 then begin
    MemStream:=TMemoryStream.Create;

    aSerial.RecvStreamSize(MemStream,1000, bytes);
    if Serial_IsError(aSerial,'ImageRecv') then begin
      aSerial.Purge();
      exit;
    end;

    MemStream.Position:=0;

    if image_type = 'G' then
      convertGray(MemStream,FBmp)
    else if image_type = 'R' then
      convertRGB565(MemStream,FBmp);

    ImagePaint.Repaint;

    MemStream.Free;
    Debug('Draw Success');
  end;
end;

initialization
begin
  SEC  := EncodeTime(0,0,1,0);
  MSEC := EncodeTime(0,0,0,1);
end;


end.

