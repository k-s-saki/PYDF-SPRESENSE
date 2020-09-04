unit MainForm;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Windows, Menus, ExtCtrls, LCLType, types, Clipbrd,
  StdCtrls, Synaser,IniFiles,windirs,
  graphtype,fpImage, fgl, LazUTF8, LConvEncoding,
  SpCam_Functions;

const
  SEC:TDateTime  =0;
  MSEC:TDateTime =0;
  RecvEnd:ansistring = #10;
  MyDocDir:string ='';

  Timeout_Image= 1000; //  serial function timeout in milliseconds
  Timeout_Str = 100;   //  serial function timeout in milliseconds

type
  TImageTags= record
    FrameNo:integer;
    ImageType:string;
    ImageSize:integer;
  end;

  TDnnInfo = class
    Caption:string;
    Value:double;
  end;

  TDnnInfoList = specialize TFPGObjectList<TDnnInfo>;

{ TFrmMain }
  TFrmMain = class(TForm)
    BtnConnect: TButton;
    BtnDisConnect: TButton;
    BtnDNN_Start: TButton;
    BtnStop: TButton;
    BtnImagePgmFile: TButton;
    BtnStart: TButton;
    BtnImageJpegFile: TButton;
    BtnDNN_Stop: TButton;
    BtnSetFileFolder: TButton;
    BtnOpenFileFolder: TButton;
    BtnFileTransfer: TButton;
    BtnTest: TButton;
    DebugMemo: TMemo;
    EdtTest: TEdit;
    EdtFileName: TEdit;
    EdtSeqNo: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LblDnn: TLabel;
    MainMenu1: TMainMenu;
    MenuItem_ShowOperationPanel: TMenuItem;
    MenuItem_ShowDebugPanel: TMenuItem;
    MenuItem_View: TMenuItem;
    MenuItem_OpenIniFile: TMenuItem;
    OpenDialog: TOpenDialog;
    PnlBottom: TPanel;
    PnlMemo: TPanel;
    MemoSplit: TSplitter;
    ImageSplit: TSplitter;
    MenuItem1: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem_Exit: TMenuItem;
    MenuItemFile: TMenuItem;
    ImagePaint: TPaintBox;
    PnlOperation: TPanel;
    SelDirDlg: TSelectDirectoryDialog;
    SpreMemo: TMemo;
    StatusBar: TStatusBar;
    Timer1: TTimer;
    procedure BtnConnect_Click(Sender: TObject);
    procedure BtnDNN_StartClick(Sender: TObject);
    procedure BtnDNN_StopClick(Sender: TObject);
    procedure BtnFileTransferClick(Sender: TObject);
    procedure BtnImagePgmFileClick(Sender: TObject);
    procedure BtnShot_Click(Sender: TObject);
    procedure BtnS_Click(Sender: TObject);
    procedure BtnE_Click(Sender: TObject);
    procedure BtnTestClick(Sender: TObject);
    procedure BtnSetFileFolderClick(Sender: TObject);
    procedure BtnOpenFileFolderClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem_ShowDebugPanelClick(Sender: TObject);
    procedure MenuItem_ExitClick(Sender: TObject);
    procedure ImagePaint_Paint(Sender: TObject);
    procedure MenuItem_OpenIniFileClick(Sender: TObject);
    procedure MenuItem_ShowOperationPanelClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    FBmp:Graphics.TBitmap;
    FSer: TBlockSerial;
    FPrevDrawDt:TDateTime;
    FPrevEndDetect:boolean;
    FIni:TIniFile;
    FConnect:boolean;
    FDnnInfoList:TDnnInfoList;

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
    procedure GetDnnResult(s:string);

    procedure Debug(s:string);
    procedure Error(s:string);

    procedure DNN_Init();

    function  GetIniFileName():string;
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

  FConnect := false;
  FPrevEndDetect:=false;

  FIni := TINIFile.Create( GetIniFileName() );
  FDnnInfoList:= TDnnInfoList.Create;

end;

function TFrmMain.GetIniFileName(): string;
var s:string;
begin
  // iniファイルを MyDocumentに配置するとき
  // result:= MyDocDir+ ExtractFileName(ChangeFileExt(ParamStr(0),'.ini'));

  // iniファイルを実行ファイルと同じ場所に配置するとき(書き込みできない場合がある)
  //result:= ChangeFileExt(ParamStr(0),'.ini');

  // iniファイルを C:\Users\<user-name>\AppData\Local\<application-title>\
  s:=GetAppConfigDir(False);
  result:= s+ ExtractFileName(ChangeFileExt(ParamStr(0),'.ini'));
  Debug(result);
end;


procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  FIni.Free;
  FBmp.Free;
  FDnnInfoList.Free;
  if Assigned(FSer) then
    FreeAndNil(FSer);
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  DNN_Init();
  LblDnn.Caption:='Init';
  LblDnn.Visible:=false;
  DisplayConnectInfo();
  EdtFileName.Text:= FIni.ReadString('File','Name','ABC');
  EdtSeqNo.Text:= FIni.ReadString('File','SeqNo','1');
end;

procedure TFrmMain.DNN_Init();
var
  s:string;
  s0:RawByteString;
  i:integer;
  v:double;
  di:TDnnInfo;
begin
  // read Dnn descriptions from inifile;

  Debug('DNN_Init()');
  i:=0;
  while true do begin
    s0:=FIni.ReadString('DNN','CAPTION_'+IntToStr(i),'');
    // Iniファイルの文字列記述　ANSIをUTF8に変換
    s:= WinCPToUTF8(s0);
    v:=FIni.ReadFloat('DNN','VALUE_'+IntToStr(i),-1);
    if v<0 then break;
    Debug( Format('DNN[%d] Caption=%s, Value=%.3f',[i,s,v]));
    di:=TDnnInfo.Create;
    di.Caption:=s;
    di.Value:=v;
    FDnnInfoList.Add(di);
    inc(i);
  end;
end;

procedure TFrmMain.MenuItem_OpenIniFileClick(Sender: TObject);
begin
  WinExec(PChar('notepad.exe "'+GetIniFileName()+'"'),SW_SHOWNORMAL);
  ShowMessage('Iniファイルは文字コードANSIで保存してください。変更後の反映にはアプリケーションの再起動が必要です。');
end;

procedure TFrmMain.MenuItem_ShowDebugPanelClick(Sender: TObject);
begin
  MenuItem_ShowDebugPanel.Checked:=not MenuItem_ShowDebugPanel.Checked;
  PnlBottom.Visible:= MenuItem_ShowDebugPanel.Checked;
end;


procedure TFrmMain.MenuItem_ShowOperationPanelClick(Sender: TObject);
begin
  MenuItem_ShowOperationPanel.Checked:=not MenuItem_ShowOperationPanel.Checked;
  PnlOperation.Visible:= MenuItem_ShowOperationPanel.Checked;
end;

procedure TFrmMain.MenuItem_ExitClick(Sender: TObject);
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
  // Disconnect
  FConnect:=false;
  if FSer<>nil then begin
    FSer.Free;
    FSer:=nil;
    Debug('Disconnected.');
  end;
  if Sender= BtnDisConnect then exit;

  // Connect
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
  LblDNN.Visible:=true;
  if FSer=nil then exit;
  Debug('Start DNN Image Recognition');
  FSer.SendString('D');
end;

procedure TFrmMain.BtnDNN_StopClick(Sender: TObject);
begin
  if FSer=nil then exit;
  Debug('Stop DNN Image Recognition ');
  FSer.SendString('d');
end;

procedure TFrmMain.BtnFileTransferClick(Sender: TObject);
var
  fn_sd:AnsiString;
  fn, s_size:string;
  st:TFileStream;
begin

  // 注:　このコードでは1KB未満程度のファイルしか送れません。（シリアル経由でspresenseのSDに書き込み）

  if OpenDialog.Execute() then begin
    fn:=OpenDialog.FileName;
    fn_sd:=LowerCase(Utf8ToSys( ExtractFileName(fn) ));

    Debug('SendFile = '+fn);
    Debug('SD name='+fn_sd);

    FSer.SendString('F'); // FileReceive Command
    FSer.SendString( fn_sd+#13#10);
    st:= TFileStream.Create( fn, fmOpenRead );
    try
      Debug('stream.size='+IntToStr(st.Size));
      s_size:=IntToStr( st.Size );
      FSer.SendString( s_size+#13#10 );
      FSer.SendStreamRaw( st );
      FSer.SendString('END FILE TRANSFER'+#13#10);
    finally
      st.Free;
    end;
  end;
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
  LblDNN.Visible:=false;
  if FSer=nil then exit;
  FBmp.PixelFormat:=pf24bit;
  Debug('StartStream() :C');
  FSer.SendString('C');

  FPrevDrawDt:=0;
end;

procedure TFrmMain.BtnE_Click(Sender: TObject);
begin
  if FSer=nil then exit;

  Debug('StopStream()');
  FSer.SendString('E');
end;

procedure TFrmMain.BtnTestClick(Sender: TObject);
begin
  if FSer=nil then exit;

  Debug('Test:'+EdtTest.Text);
  FSer.SendString(EdtTest.Text);
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

  if Length(s)=0 then begin
    SpreMemo.Lines.Add('Zero string');
    exit;
  end;

  if s[1]='@' then begin
    result:=true;
    SpreMemo.Lines.Add(s);

    //DNNの文字列かどうか
    if Pos('@DNN Output',s)>0 then begin
      GetDnnResult(s);
    end;
  end;
end;


procedure TFrmMain.GetDnnResult(s:string);
var
  sa:TStringArray;
  cnt,i,j:integer;

  v:array of double;
  detect:boolean;
begin
  sa:=s.Split([':','=',',']);
  //s="@DNN Output:0=value0,1=value1,2=value2 ..."
  cnt:= (Length(sa)-1) div 2;

  SetLength(v, cnt );

  for i:=0 to Length(v)-1 do
  begin
    v[i]:= StrToFloatDef(sa[2+i*2],0.0);
  end;

  detect:=false;

  for j:=0 to FDnnInfoList.Count-1 do
  begin
    // check index.
    if j>=cnt then break;

    if v[j]> FDnnInfoList[j].Value then begin
      lblDnn.Caption:= FDnnInfoList[j].Caption;
      detect:=true;
      break;
    end;
  end;

  if not detect then
    LblDnn.Caption:='??';
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
    //Error(aDebugStr+':'+aSerial.LastErrorDesc);
    result:=true;
  end
  else
    result:=false;
end;

function StrDump(s:string):string;
var i:integer;
begin
  result:='';
  for i:=1 to Length(s) do begin
    result:=result + '$'+IntToHex( ord(s[i]),2 );
  end;
end;

procedure TFrmMain.Timer1Timer(Sender: TObject);
var
  s: string;
begin
  if not Assigned(FSer) then
    exit;

(*
  // CanReadは不要と思われる
  while FSer.CanRead(10) do
  begin
    //1行取得(10msec)
    if Serial_RecvStr(FSer,10, s,'FirstRead') = false then
      exit;
*)
  while Serial_RecvStr(FSer,10, s,'FirstRead') do begin
    Debug('receive = '+s);
    Debug('1= '+StrDump(s));
    s:= Trim(s);
    Debug('2= '+StrDump(s));

    if Length(s)=0 then begin
      Debug('zero string');
      continue;
    end;

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
  if Serial_RecvStr(aSerial,Timeout_Str, s,'FrameNo') = false then begin
    aSerial.Purge;
    exit;
  end;
  Debug(s);
  oImageTags.FrameNo := GetIntegerProperty(s);

  //ImageType
  if Serial_RecvStr(aSerial,Timeout_Str, s,'ImageType') = false then begin
    aSerial.Purge();
    exit;
  end;
  Debug(s);
  oImageTags.ImageType:= GetStringProperty(s);

  //ImageSize
  if Serial_RecvStr(aSerial,Timeout_Str, s,'ImageSize') = false then begin
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
        aSerial.RecvStreamSize(MemStream, Timeout_Image, imgTags.ImageSize);

        if Serial_IsError(aSerial,'Serial_StreamImage') then begin
          aSerial.Purge();
          exit;
        end;

        MemStream.Position:=0;
        if imgTags.ImageType='JPEG' then
          ConvertStreamJpegToBitmap(MemStream,FBmp)
        else if imgTags.ImageType='RGB565' then
          ConvertStreamRGB565ToBitmap(MemStream,FBmp)
        else if imgTags.ImageType='GRAY' then
          ConvertStreamGrayToBitmap(MemStream,FBmp);

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

      aSerial.RecvStreamSize(MemStream,Timeout_Image, imgTags.ImageSize);

      if Serial_IsError(aSerial,'Serial_FileImage') then begin
        aSerial.Purge();
        exit;
      end;

      MemStream.Position:=0;
      fs.CopyFrom(MemStream, MemStream.Size);
      edtSeqNo.Text:= IntToStr( StrToIntDef(edtSeqNo.Text,0) +1 );
      Debug('Save Success , NextSeqNo='+edtSeqNo.Text);

      if imgTags.ImageType='PGM' then
      begin
        // PGMファイルをBMPファイルに変換
        CreateBitmapFileFromGrayPgm(fn);
      end;

    finally
      MemStream.Free;
      fs.Free;
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

