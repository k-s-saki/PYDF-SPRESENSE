unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Windows, Menus, ExtCtrls, ActnList, LCLType, types, Clipbrd,
  StdCtrls, Synaser,IniFiles,windirs,
  intfgraphics,graphtype,fpImage,
  SpCam_Functions;

const
  SEC:TDateTime  =0;
  MSEC:TDateTime =0;
  RecvEnd:string = #10;
  MyDocDir:string ='';

type
  TImageTags= record
    FrameNo:integer;
    ImageType:string;
    ImageSize:integer;
  end;

{ TFrmMain }
  TFrmMain = class(TForm)
    BtnConnect: TButton;
    BtnDNN_Start: TButton;
    BtnStop: TButton;
    BtnImagePgmFile: TButton;
    BtnStart: TButton;
    BtnImageJpegFile: TButton;
    BtnDNN_Stop: TButton;
    Button1: TButton;
    BtnSetFileFolder: TButton;
    BtnOpenFileFolder: TButton;
    CbDebugPanel: TCheckBox;
    DebugMemo: TMemo;
    EdtFileName: TEdit;
    EdtSeqNo: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MainMenu1: TMainMenu;
    PnlBottom: TPanel;
    PnlMemo: TPanel;
    MemoSplit: TSplitter;
    ImageSplit: TSplitter;
    MenuItem1: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemFile: TMenuItem;
    ImagePaint: TPaintBox;
    PnlOperation: TPanel;
    ProgressBar1: TProgressBar;
    SelDirDlg: TSelectDirectoryDialog;
    SpreMemo: TMemo;
    StatusBar: TStatusBar;
    Timer1: TTimer;
    procedure BtnConnect_Click(Sender: TObject);
    procedure BtnDNN_StartClick(Sender: TObject);
    procedure BtnDNN_StopClick(Sender: TObject);
    procedure BtnImagePgmFileClick(Sender: TObject);
    procedure BtnShot_Click(Sender: TObject);
    procedure BtnS_Click(Sender: TObject);
    procedure BtnE_Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BtnSetFileFolderClick(Sender: TObject);
    procedure BtnOpenFileFolderClick(Sender: TObject);
    procedure CbDebugPanelChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    FIni:TIniFile;
    FConnect:boolean;

    procedure CalcFPS();

    procedure DisplayConnectInfo();


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
    function  Serial_StreamImage(aSerial:TBlockSerial):boolean;

    function  Serial_FileImage(aSerial:TBlockSerial):boolean;

    function  Serial_ReadImageTags(aSerial:TBlockSerial; out oImageTags:TImageTags):boolean;

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
var s:string;
begin
  ImagePaint.Align:=alClient;
  SpreMemo.Align:=alClient;
  PnlBottom.Align:=alBottom;


  FBmp:=Graphics.TBitmap.Create;
  FBmp.SetSize(BW,BH);
  FBmp.PixelFormat:=pf24bit;

  FConnect := false;
  FPrevEndDetect:=false;
  s:= MyDocDir+ ExtractFileName(ChangeFileExt(ParamStr(0),'.ini'));
  FIni := TINIFile.Create(s);
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  FIni.Free;
  FBmp.Free;
  if Assigned(FSer) then
    FreeAndNil(FSer);
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  DisplayConnectInfo();
  EdtFileName.Text:= FIni.ReadString('File','Name','ABC');
  EdtSeqNo.Text:= FIni.ReadString('File','SeqNo','1');
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
  s:string;
begin
  dt:=Now;
  if FPrevDrawDt<>0 then
  begin
    span:= (dt-FPrevDrawDt);
    if (span > MSEC*10) then
      s:= Format('%.2f',[SEC / span] )+'FPS'
    else
      s:='0.0 FPS';
    StatusBar.Panels[2].Text:='Camera Preview:'+s;
  end;
  FPrevDrawDt:=dt;
end;

procedure TFrmMain.DisplayConnectInfo();
var s:string;
begin
  if FConnect then
    s:='<Connect>';
  StatusBar.Panels[0].text:=FIni.ReadString('SERIAL','PORT','COM?') +s;
  StatusBar.Panels[1].text:=FIni.ReadString('SERIAL','BAUDRATE','2000000')+'bps';
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
  FConnect:=false;
  if FSer<>nil then begin
    FSer.Free;
    FSer:=nil;
  end;

  frm:=TFrmConnect.Create(nil);
  try

    //成功した設定を読みだす
    if FIni.SectionExists('SERIAL') then begin
      frm.EdtPort.Text:= FIni.ReadString('SERIAL','PORT','COM5');
      frm.EdtBaudRate.Text:= FIni.ReadString('SERIAL','BAUDRATE','2000000');
    end;

    if frm.ShowModal()=mrOk then begin

      FSer := TBlockSerial.Create;
      FSer.Connect(frm.EdtPort.Text);                       // COM ポート指定接続
      Sleep(100);
      br:= StrToIntDef( frm.EdtBaudRate.Text, 0);
      FSer.Config( br, 8, 'N', SB1, False, False); // ボーレート他の設定
      FSer.ConvertLineEnd:=false;

      if Serial_IsError(FSer, 'Connect ' + frm.EdtPort.Text ) then
        Debug('Connect Fail')
      else begin
        Debug('Connect Success');
        FConnect:=true;
        //成功した設定を保存
        FIni.WriteString('SERIAL','PORT',frm.EdtPort.Text);
        FIni.WriteString('SERIAL','BAUDRATE',frm.EdtBaudRate.Text);
      end;
    end
    else begin
      Debug('Cancel Connect.');
    end;

    DisplayConnectInfo();

  finally
    frm.Free;
  end;
end;

procedure TFrmMain.BtnDNN_StartClick(Sender: TObject);
begin
  if FSer=nil then exit;
  //FTransferCmd:='D';
  Debug('Start DNN Image Recognition');
  FSer.SendString('D');
end;

procedure TFrmMain.BtnDNN_StopClick(Sender: TObject);
begin
  if FSer=nil then exit;
  //FTransferCmd:='d';
  Debug('Stop DNN Image Recognition ');
  FSer.SendString('d');
end;

procedure TFrmMain.BtnImagePgmFileClick(Sender: TObject);
begin
  if FSer=nil then exit;

  Debug('Pgm to File');
  FSer.SendString('P');
end;

procedure TFrmMain.BtnShot_Click(Sender: TObject);
begin
  if FSer=nil then exit;

  Debug('Shot to SD');
  FSer.SendString('S');

end;

procedure TFrmMain.BtnS_Click(Sender: TObject);
begin
  if FSer=nil then exit;
  FTransferCmd:='C';
  FBmp.PixelFormat:=pf24bit;
  Debug('StartStream() :'+FTransferCmd);
  FSer.SendString(FTransferCmd);

  FPrevDrawDt:=0;
end;

procedure TFrmMain.BtnE_Click(Sender: TObject);
begin
  if FSer=nil then exit;

  Debug('StopStream()');
  FSer.SendString('E');
end;

procedure TFrmMain.Button1Click(Sender: TObject);
begin
  if FSer=nil then exit;

  Debug('Test"."');
  FSer.SendString('.');

end;

procedure TFrmMain.BtnSetFileFolderClick(Sender: TObject);
begin
  if FIni.SectionExists('Folder') then
    SelDirDlg.InitialDir:=
      FIni.ReadString('Folder','Image',MyDocDir);

  if SelDirDlg.Execute then begin
    FIni.WriteString('Folder','Image', SelDirDlg.Filename);
  end;
end;

procedure TFrmMain.BtnOpenFileFolderClick(Sender: TObject);
var s:string;
begin
  s:=FIni.ReadString('Folder','Image',MyDocDir);
  WinExec(PChar('explorer.exe /e, '+s),SW_SHOWNORMAL);
end;

procedure TFrmMain.CbDebugPanelChange(Sender: TObject);
begin
  PnlBottom.Visible:= CbDebugPanel.Checked;
end;

procedure TFrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if CloseAction = caFree then
  begin
    FIni.WriteString('File','Name',EdtFileName.Text);
    FIni.WriteString('File','SeqNo',EdtSeqNo.Text);
  end;
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

    //Spresense通信処理
    Debug('From Spresense :'+s);

    if s='ImageStream' then begin
      Serial_StreamImage(FSer);
      CalcFPS();
    end;

    if s='ImageFile' then begin
      Serial_FileImage(FSer);
    end;

  end;
end;


function TFrmMain.Serial_ReadImageTags(aSerial: TBlockSerial;
  out oImageTags: TImageTags): boolean;
var s:string;
begin
  result:=false;

  //FrameNo:
  if Serial_RecvStr(aSerial,100, s,'FrameNo') = false then begin
    aSerial.Purge;
    exit;
  end;
  Debug(s);
  oImageTags.FrameNo := GetIntegerProperty(s);

  //ImageType
  if Serial_RecvStr(aSerial,100, s,'ImageType') = false then begin
    aSerial.Purge();
    exit;
  end;
  Debug(s);
  oImageTags.ImageType:= GetStringProperty(s);

  //ImageSize
  if Serial_RecvStr(aSerial,100, s,'ImageSize') = false then begin
    aSerial.Purge();
    exit;
  end;
  Debug(s);
  oImageTags.ImageSize:= GetIntegerProperty(s);
  result:=true;

end;


function TFrmMain.Serial_StreamImage(aSerial:TBlockSerial):boolean;
var
  MemStream:TMemoryStream;
  imgTags:TImageTags;
begin
  result:=false;
  Debug('Serial_StreamImage()');

  if Serial_ReadImageTags(aSerial, imgTags)=false then
    exit;

  if imgTags.ImageSize>0 then
  begin
    MemStream:=TMemoryStream.Create;
    try
      try
        aSerial.RecvStreamSize(MemStream,1000, imgTags.ImageSize);
        if Serial_IsError(aSerial,'Serial_StreamImage') then begin
          aSerial.Purge();
          exit;
        end;

        MemStream.Position:=0;
        ConvertJpeg(MemStream,FBmp);
        ImagePaint.Repaint;
        Debug('Draw Success');
      except
        on e:Exception do
          Debug('Exception:'+e.Message);
      end;
    finally
      MemStream.Free;
    end;
  end;
end;

function TFrmMain.Serial_FileImage(aSerial: TBlockSerial): boolean;
var
  MemStream:TMemoryStream;
  imgTags:TImageTags;
  fs: TFileStream;
  fn: string;
begin
  result:=false;
  Debug('Serial_FileImage()');

  if Serial_ReadImageTags(aSerial, imgTags)=false then
    exit;

  if imgTags.ImageSize>0 then
  begin

    fn:= FIni.ReadString('Folder','Image',MyDocDir);
    fn:= fn+'\'+EdtFileName.Text+EdtSeqNo.Text+'.'+imgTags.ImageType;
    Debug('Save filename ='+fn);

    MemStream:=TMemoryStream.Create;
    fs:=TFileStream.Create( fn,fmOpenWrite or fmCreate );
    try

      aSerial.RecvStreamSize(MemStream,1000, imgTags.ImageSize);
      if Serial_IsError(aSerial,'Serial_FileImage') then begin
        aSerial.Purge();
        exit;
      end;

      MemStream.Position:=0;
      fs.CopyFrom(MemStream, MemStream.Size);
      edtSeqNo.Text:= IntToStr( StrToIntDef(edtSeqNo.Text,0) +1 );
      Debug('Save Success , NextSeqNo='+edtSeqNo.Text);
      fs.Free;

      if imgTags.ImageType='PGM' then
      begin
        // PGMファイルをPNGファイルに変換
        ConvertGrayPgmToBmp(fn);
      end;

    finally
      MemStream.Free;
      //fs.Free;
    end;
  end;
end;

initialization
begin
  SEC  := EncodeTime(0,0,1,0);
  MSEC := EncodeTime(0,0,0,1);
  MyDocDir := GetWindowsSpecialDir(CSIDL_PERSONAL);

end;


end.

