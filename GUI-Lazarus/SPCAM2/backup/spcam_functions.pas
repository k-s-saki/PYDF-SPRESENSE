unit Spcam_Functions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Windows, Menus, ExtCtrls, ActnList, LCLType, types, Clipbrd,
  StdCtrls, Synaser,IniFiles,windirs,
  intfgraphics,graphtype,fpImage;


procedure ConvertGrayPgmToBmp(aPgmFileName:string);

function ConvertRGB565(ms:TMemoryStream; bmp: Graphics.TBitmap):boolean;
function ConvertGray(ms:TMemoryStream; bmp: Graphics.TBitmap):boolean;
function ConvertJpeg(ms:TMemoryStream; bmp: Graphics.TBitmap):boolean;

function GetStringProperty(s:string):string;
function GetIntegerProperty(s:string):integer;


implementation

procedure ConvertGrayPgmToBmp(aPgmFileName:string);
var
  //png_fn,
  bmp_fn, ln:string;
  //png: TPortableNetworkGraphic;
  img: TLazIntfImage;
  col: TFPColor;
  ix,iy,i:integer;
  fs:TFileStream;
  ch:char;
  pix:word;
  desc:TRawImageDescription;

const
  SX=28; SY=28;
begin
  //png_fn:= ChangeFileExt( aPgmFileName, '.png');
  bmp_fn:= ChangeFileExt( aPgmFileName, '.bmp');

  //png := TPortableNetworkGraphic.Create;
  img := TLazIntfImage.Create(SX,SY,[riqfGrey]);
  fs  := TFileStream.Create(aPgmFileName, fmOpenRead);
  ch:=#0;
  try
    img.SetSize(SX, SY);
    //\nを3回読み飛ばす
    for i:=0 to 2 do begin
      while( fs.Read(ch,1) > 0) do
      begin
        if (ch=#13) or (ch=#10) then
        begin
          ln:='';
          break;
        end;
        ln:=ln+ch;
      end;
    end;

    for iy := 0 to SY - 1 do
    begin
      for ix := 0 to SX - 1 do
      begin
        pix:= fs.ReadByte shl 8;
        //pix:= fs.ReadByte;
        col.red:= pix;
        col.green:=pix;
        col.blue:=pix;
        img.Colors[ix, iy] := col;
      end;
    end;
    {
    png.LoadFromIntfImage(img);
    png.ColorType:=ctGrayscale;
    png.PixelFormat:= pf8bit;
    png.Monochrome:=true;
    png.SaveToFile(png_fn);
    }

    desc:=img.DataDescription;
    desc.Depth:=8;
    desc.Format:=ricfGray;
    img.DataDescription:=desc;
    img.SaveToFile(bmp_fn);
  finally
    //png.Free;
    img.Free;
    fs.Free;
  end;
end;


function GetStringProperty(s:string):string;
begin
  result:=Trim( Copy(s, AnsiPos('=',s)+1, length(s)) );
end;

function GetIntegerProperty(s:string):integer;
var w:string;
begin
  w:= GetStringProperty(s);
  result:= StrToIntDef(w,0);
end;


function ConvertRGB565(ms:TMemoryStream; bmp: Graphics.TBitmap):boolean;
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

function ConvertGray(ms:TMemoryStream; bmp: Graphics.TBitmap):boolean;

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

function ConvertJpeg(ms:TMemoryStream; bmp: Graphics.TBitmap):boolean;
var jpg:TJPEGImage;
begin
  jpg:=TJPEGImage.Create;
  try
    jpg.LoadFromStream(ms);
    bmp.Assign(jpg);
    result:=true;
  finally
    jpg.Free;
  end;
end;


end.

