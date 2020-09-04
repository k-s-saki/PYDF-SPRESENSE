unit Spcam_Functions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, ExtCtrls, ActnList, LCLType, types, Clipbrd,
  StdCtrls, windirs,
  intfgraphics, graphtype, fpImage;

procedure CreateBitmapFileFromGrayPgm(aPgmFileName: string);
(* モノクロPGMをBMPに変換(ファイル) *)

function ConvertStreamRGB565ToBitmap(ms: TMemoryStream; bmp: Graphics.TBitmap): boolean;
(* RGB565のストリームを TBitmapオブジェクトに変換 *)

function ConvertStreamGrayToBitmap(ms: TMemoryStream; bmp: Graphics.TBitmap): boolean;
(* 輝度ストリームをTBitmapオブジェクトに変換 *)

function ConvertStreamJpegToBitmap(ms: TMemoryStream; bmp: Graphics.TBitmap): boolean;
(* JpegストリームをTBitmapオブジェクトに変換 *)


function GetStringProperty(s: string): string;
(* 文字列'a=b' の 　bの部分を文字列で返す *)

function GetIntegerProperty(s: string): integer;
(* 文字列'a=b' の 　bの部分を数値で返す *)


implementation

procedure CreateBitmapFileFromGrayPgm(aPgmFileName: string);
var
  bmp_fn, ln: string;
  img: TLazIntfImage;
  col: TFPColor;
  ix, iy, i: integer;
  fs: TFileStream;
  ch: char;
  pix: word;
  desc: TRawImageDescription;

const
  SX = 28;
  SY = 28;
begin
  bmp_fn := ChangeFileExt(aPgmFileName, '.bmp');

  img := TLazIntfImage.Create(SX, SY, [riqfGrey]);
  fs := TFileStream.Create(aPgmFileName, fmOpenRead);
  ch := #0;
  try
    img.SetSize(SX, SY);
    //\nを3回読み飛ばす
    for i := 0 to 2 do
    begin
      while (fs.Read(ch, 1) > 0) do
      begin
        if (ch = #13) or (ch = #10) then
        begin
          ln := '';
          break;
        end;
        ln := ln + ch;
      end;
    end;

    for iy := 0 to SY - 1 do
    begin
      for ix := 0 to SX - 1 do
      begin
        pix := fs.ReadByte shl 8;
        col.red := pix;
        col.green := pix;
        col.blue := pix;
        img.Colors[ix, iy] := col;
      end;
    end;

    desc := img.DataDescription;
    desc.Depth := 8;
    desc.Format := ricfGray;
    img.DataDescription := desc;
    img.SaveToFile(bmp_fn);
  finally
    img.Free;
    fs.Free;
  end;
end;


function GetStringProperty(s: string): string;
begin
  Result := Trim(Copy(s, AnsiPos('=', s) + 1, length(s)));
end;

function GetIntegerProperty(s: string): integer;
var
  w: string;
begin
  w := GetStringProperty(s);
  Result := StrToIntDef(w, 0);
end;


function ConvertStreamRGB565ToBitmap(ms: TMemoryStream; bmp: Graphics.TBitmap): boolean;
type
  TFormat565 = packed record
    case integer of
      0: (AsWord: word);
      1: (AsByte: array[0..1] of byte);
  end;

var
  pMem: PByte;
  LineIndex, PixelIndex: integer;
  Data565: TFormat565;
  dR, dG, dB: byte;
  w: word;
begin
  Result := False;
  bmp.Width := 320;
  bmp.Height := 240;
  bmp.PixelFormat := pf24bit;

  bmp.BeginUpdate();
  for LineIndex := 0 to bmp.Height - 1 do
  begin
    pMem := bmp.ScanLine[LineIndex];
    for PixelIndex := 0 to bmp.Width - 1 do
    begin
      Data565.AsByte[0] := ms.ReadByte();
      Data565.AsByte[1] := ms.ReadByte();
      w := Data565.AsWord;
      dR := (((w and $f800) shr 11) shl 3);
      dG := (((w and $07E0) shr 5) shl 2);
      dB := ((w and $001F) shl 3);
      //Windowsでは BitmapはBGRの順
      (pMem +0)^ := dB;
      (pMem +1)^ := dG;
      (pMem +2)^ := dR;
      pMem := pMem + 3;
    end;
  end;
  bmp.EndUpdate();
  Result := True;
end;

function ConvertStreamGrayToBitmap(ms: TMemoryStream; bmp: Graphics.TBitmap): boolean;

type
  TMemoryFormatGray = packed array [0 .. MaxInt] of byte;
  PMemoryFormatGray = ^TMemoryFormatGray;

var
  pMem: PMemoryFormatGray;
  LineIndex, PixelIndex: integer;
  cnt: integer;
begin
  Result := False;
  cnt := 0;
  bmp.Width := 320;
  bmp.Height := 240;
  bmp.PixelFormat := pf8bit;

  bmp.BeginUpdate();
  for LineIndex := 0 to bmp.Height - 1 do
  begin
    pMem := bmp.ScanLine[LineIndex];
    for PixelIndex := 0 to bmp.Width - 1 do
    begin
      pMem^[PixelIndex] := ms.ReadByte();
      Inc(cnt);
    end;
  end;
  bmp.EndUpdate();
  Result := True;
end;

function ConvertStreamJpegToBitmap(ms: TMemoryStream; bmp: Graphics.TBitmap): boolean;
var
  jpg: TJPEGImage;
begin
  jpg := TJPEGImage.Create;
  try
    jpg.LoadFromStream(ms);
    bmp.Assign(jpg);
    Result := True;
  finally
    jpg.Free;
  end;
end;


end.
