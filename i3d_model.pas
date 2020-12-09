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
//  I3D Models
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/speed-game/
//------------------------------------------------------------------------------

unit i3d_model;

interface

uses
  SysUtils, Classes, i3d_structs;

type
  TI3DModel = class(TObject)
  private
    obj: O3DM_TObject_p;
    objfaces: PO3DM_TFaceArray;
    objsize: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function LoadFromStream(const strm: TStream): boolean;
    function LoadFromFile(const fname: string): boolean;
    procedure Clear;
  end;

implementation

constructor TI3DModel.Create;
begin
  Inherited Create;
  obj := nil;
  objsize := 0;
end;

destructor TI3DModel.Destroy;
begin
  Clear;
  Inherited Destroy;
end;

function TI3DModel.LoadFromStream(const strm: TStream): boolean;
var
  magic: LongWord;
  base: LongWord;
  i, j, l: integer;
  face, pf2: O3DM_TFace_p;
  facecachepos: integer;

  function _OF(const p: pointer): pointer;
  begin
    Result := pointer(LongWord(p) + base);
  end;

  function _CacheRead(size: integer): pointer;
  begin
    result := @obj.facecache[facecachepos];
    facecachepos := facecachepos + size;
  end;

begin
  Clear;
  strm.Read(magic, SizeOf(LongWord));
  if magic <> ID3_MAGIC then
  begin
    Result := False;
    Exit;
  end;

  strm.Read(objsize, SizeOf(integer));
  GetMem(obj, objsize);
  strm.Read(obj^, objsize);
  base := LongWord(obj);

  obj.verts := _OF(obj.verts);
  obj.normals := _OF(obj.normals);
  obj.facecache := _OF(obj.facecache);
  obj.materials := _OF(obj.materials);

  GetMem(objfaces, obj.nFaces * SizeOf(O3DM_TFace));

  facecachepos := 0;
  for i := 0 to obj.nFaces - 1 do
  begin
    objfaces[i].h := _CacheRead(SizeOf(O3DM_TFaceHeader));
    objfaces[i].verts := _CacheRead(objfaces[i].h.nVerts * SizeOf(O3DM_TFaceVertex));
    for j := 0 to objfaces[i].h.nVerts - 1 do
    begin
      objfaces[i].verts[j].vert := _OF(objfaces[i].verts[j].vert);
      if objfaces[i].verts[j].normal <> nil then
        objfaces[i].verts[j].normal := _OF(objfaces[i].verts[j].normal);
    end;
    if objfaces[i].h.material <> nil then
      objfaces[i].h.material := _OF(objfaces[i].h.material);
    if objfaces[i].h.front <> nil then
      objfaces[i].h.front := _OF(objfaces[i].h.front);
    if objfaces[i].h.back <> nil then
      objfaces[i].h.back := _OF(objfaces[i].h.back);
    if objfaces[i].h.next <> nil then
      objfaces[i].h.next := _OF(objfaces[i].h.next);
  end;

(*  pf2 := nil;
  face := @obj.faces[0];

  while face <> nil do
  begin
    if face.h.front <> nil then
      face.h.front := _OF(face.h.front);
    pf2 := face.h.front;
    for i := 0 to face.h.nVerts - 1 do
    begin
      face.verts[i].vert := _OF(face.verts[i].vert);
      if face.verts[i].normal <> nil then
        face.verts[i].normal := _OF(face.verts[i].normal);
    end;
    if face.h.material <> nil then
      face.h.material := _OF(face.h.material);
    face := pf2;
  end;*)

  for i := 0 to obj.nMaterials - 1 do
    if obj.materials[i].texture <> nil then
    begin
      if obj.materials[i].flags and O3DMF_256 <> 0 then
        l := 256 * 64
      else
        l := 64 * 64;
      GetMem(obj.materials[i].texture, l);
      if obj.materials[i].texture <> nil then
        strm.read(obj.materials[i].texture^, 1)
      else
        strm.Position := strm.Position + 1;
    end;
end;

function TI3DModel.LoadFromFile(const fname: string): boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
  result := LoadFromStream(fs);
  fs.Free;
end;

procedure TI3DModel.Clear;
var
  i, l: integer;
begin
  if obj <> nil then
  begin
    FreeMem(objfaces, obj.nFaces * SizeOf(O3DM_TFace));
    for i := 0 to obj.nMaterials - 1 do
      if obj.materials[i].texture <> nil then
      begin
        if obj.materials[i].flags and O3DMF_256 <> 0 then
          l := 256 * 64
        else
          l := 64 * 64;
        FreeMem(obj.materials[i].texture, l);
      end;
    FreeMem(obj, objsize);
    obj := nil;
    objsize := 0;
  end;
end;

end.
