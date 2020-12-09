//------------------------------------------------------------------------------
//
//  I3D_Viewer - Model Viewer for Speed Haste models
//  Copyright (C) 2020 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  JCL File reader (Speed Haste container file)
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/speed-game/
//------------------------------------------------------------------------------

unit jcl_file;

interface

uses
  Classes, SysUtils;

type
  speedlump_t = packed record
    filename: array[0..23] of char;
    start, size: integer;
  end;
  speedlump_p = ^speedlump_t;
  speedlump_tArray = packed array[0..$FFF] of speedlump_t;
  Pspeedlump_tArray = ^speedlump_tArray;

const
  JCL_MAGIC = $df73b489;

type
  speedheader_t = packed record
    magic: LongWord;
    nlumps1: integer;
    nlumps: integer;
    lastoffset: integer;
  end;

function getlumpname(const l: speedlump_p): string;

const
  SH_FLAT_PREFIX = 'FLAT';
  SH_WALL_PREFIX = 'WALL';

type
  TJCLReader = class(TObject)
  private
    header: speedheader_t;
    f: TFileStream;
    flumps: Pspeedlump_tArray;
    fnumlumps: integer;
  protected
    function DoReadLump(const l: Pspeedlump_tArray; const numl: integer;
      const lmp: string; var buf: pointer; var size: integer): boolean;
    function FindLump(const l: Pspeedlump_tArray; const numl: integer;
       const lmp: string): integer;
    procedure Clear;
    function ReadHeader: boolean;
    function ReadDirectory: boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFile(const fname: string);
    function ReadLump(const lmp: string; var buf: pointer; var size: integer): boolean;
    property lumps: Pspeedlump_tArray read flumps;
    property numlumps: integer read fnumlumps;
  end;


implementation

uses
  i3d_utils;

function getlumpname(const l: speedlump_p): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to 23 do
  begin
    if l.filename[i] = #0 then
      break;
    result := result + l.filename[i];
  end;
  result := Trim(result);
end;

constructor TJCLReader.Create;
begin
  f := nil;
  flumps := nil;
  fnumlumps := 0;
  Inherited;
end;

destructor TJCLReader.Destroy;
begin
  Clear;
  Inherited;
end;

procedure TJCLReader.Clear;
begin
  if f <> nil then
    f.Free;

  if fnumlumps <> 0 then
  begin
    FreeMem(flumps, fnumlumps * SizeOf(speedlump_t));
    fnumlumps := 0;
  end;
end;

function TJCLReader.DoReadLump(const l: Pspeedlump_tArray; const numl: integer;
  const lmp: string; var buf: pointer; var size: integer): boolean;
var
  i: integer;
begin
  for i := 0 to numl - 1 do
    if getlumpname(@l[i]) = lmp then
    begin
      f.Seek(l[i].start, soFrombeginning);
      size := l[i].size;
      GetMem(buf, size);
      f.Read(buf^, size);
      result := true;
      exit;
    end;
  buf := nil;
  result := false;
  size := 0;
end;

function TJCLReader.FindLump(const l: Pspeedlump_tArray; const numl: integer;
  const lmp: string): integer;
var
  i: integer;
begin
  for i := 0 to numl - 1 do
    if getlumpname(@l[i]) = lmp then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

function TJCLReader.ReadHeader: boolean;
begin
  f.Seek(SizeOf(speedheader_t), soFromEnd);
  f.Read(header, SizeOf(speedheader_t));
  result := header.magic = JCL_MAGIC;
end;

function TJCLReader.ReadDirectory: boolean;
var
  i, j: integer;
  sz: integer;
  item: speedlump_t;

  function _compare_lumps(const ii, jj: integer): integer;
  var
    vii, vjj: integer;
    fii, fjj: string;
    nii, njj: string;
    eii, ejj: string;
  begin
    fii := getlumpname(@flumps[ii]);
    fjj := getlumpname(@flumps[jj]);
    splitstring(fii, nii, eii, '.');
    splitstring(fjj, njj, ejj, '.');
    vii := StrToIntDef(nii, $FFFF);
    vjj := StrToIntDef(njj, $FFFF);
    if vii > vjj then
      result := 1
    else if vii < vjj then
      result := -1
    else if flumps[ii].filename > flumps[jj].filename then
      result := 1
    else if flumps[ii].filename < flumps[jj].filename then
      result := -1
    else
      result := 0;
  end;

begin
  fnumlumps := header.nlumps;
  GetMem(flumps, fnumlumps * SizeOf(speedlump_t));
  FillChar(flumps^, fnumlumps * SizeOf(speedlump_t), #0);
  f.Seek(header.lastoffset, soFromEnd);
  result := f.Read(flumps^, fnumlumps * SizeOf(speedlump_t)) = fnumlumps * SizeOf(speedlump_t);
  sz := f.Size;
  for i := 0 to fnumlumps - 1 do
    flumps[i].start := sz - header.lastoffset - flumps[i].start;

  for i := 0 to fnumlumps - 1 do
    for j := 0 to fnumlumps - 2 do
      if _compare_lumps(j, j + 1) > 0 then
      begin
        item := flumps[j];
        flumps[j] := flumps[j + 1];
        flumps[j + 1] := item;
      end;

end;

procedure TJCLReader.LoadFile(const fname: string);
begin
  Clear;

  f := TFileStream.Create(fname, fmOpenRead);

  ReadHeader;
  ReadDirectory;
end;

function TJCLReader.ReadLump(const lmp: string; var buf: pointer; var size: integer): boolean;
begin
  result := DoReadLump(flumps, fnumlumps, lmp, buf, size);
end;

end.

