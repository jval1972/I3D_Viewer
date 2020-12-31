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
//  Site  : https://sourceforge.net/projects/i3dviewer/
//------------------------------------------------------------------------------

unit i3d_model;

interface

uses
  SysUtils,
  Classes,
  i3d_structs;

type
  TI3DModel = class(TObject)
  private
    obj: O3DM_TObject_p;
    objfaces: PO3DM_TFaceArray;
    objsize: integer;
    textures: array[0..$7F] of LongWord;  // texid is shortint (-128..127)
    numtextures: integer;
    fbitmaps: TStringList;
    fselected: integer;
  protected
    function GetNumFaces: integer; virtual;
    function GetFace(Index: Integer): O3DM_TFace_p; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function LoadFromStream(const strm: TStream): boolean;
    function LoadFromFile(const fname: string): boolean;
    procedure Clear;
    function CreateTexture(const m: O3DM_TMaterial_p): integer;
    function RenderGL(const scale: single): integer;
    property faces[Index: integer]: O3DM_TFace_p read GetFace;
    property numfaces: integer read GetNumFaces;
    property Bitmaps: TStringList read fbitmaps;
    property selected: integer read fselected write fselected;
  end;

implementation

uses
  dglOpenGL,
  graphics,
  i3d_palette;

constructor TI3DModel.Create;
begin
  Inherited Create;
  obj := nil;
  objsize := 0;
  numtextures := 0;
  fselected := -1;
  fbitmaps := TStringList.Create;
end;

destructor TI3DModel.Destroy;
begin
  Clear;
  fbitmaps.Free;
  Inherited Destroy;
end;

function TI3DModel.GetNumFaces: integer;
begin
  if obj = nil then
    Result := 0
  else
    Result := obj.nFaces;
end;

function TI3DModel.GetFace(Index: Integer): O3DM_TFace_p;
begin
  if obj = nil then
    Result := nil
  else if (Index >= 0) and (Index < obj.nFaces) then
    Result := @objfaces[Index]
  else
    Result := nil;
end;

function TI3DModel.LoadFromStream(const strm: TStream): boolean;
var
  magic: LongWord;
  base: LongWord;
  i, j, l: integer;
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
  end;

  for i := 0 to obj.nMaterials - 1 do
    if obj.materials[i].texture <> nil then
    begin
      if obj.materials[i].flags and O3DMF_256 <> 0 then
        l := 256 * 64
      else
        l := 64 * 64;
      GetMem(obj.materials[i].texture, l);
      if obj.materials[i].texture <> nil then
        strm.read(obj.materials[i].texture^, l)
      else
        strm.Position := strm.Position + 1;
      CreateTexture(@obj.materials[i]);
    end
    else
      obj.materials[i].texid := -1;

  Result := True;
end;

function TI3DModel.CreateTexture(const m: O3DM_TMaterial_p): integer;
type
  TLongWordArrayBuffer = array[0..$3FFF] of LongWord;
  PLongWordArrayBuffer = ^TLongWordArrayBuffer;
var
  buffer: PLongWordArrayBuffer;
  i: integer;
  dest: PLongWord;
  TEXDIMX, TEXDIMY: integer;
  gltex: LongWord;
  bm: TBitmap;
begin
  if m.flags and O3DMF_256 <> 0 then
  begin
    TEXDIMX := 256;
    TEXDIMY := 64;
  end
  else
  begin
    TEXDIMX := 64;
    TEXDIMY := 64;
  end;

  bm := TBitmap.create;
  bm.Width := TEXDIMX;
  bm.height := TEXDIMY;

  GetMem(buffer, TEXDIMX * TEXDIMY * SizeOf(LongWord));
  dest := @buffer[0];
  for i := 0 to TEXDIMX * TEXDIMY - 1 do
  begin
    dest^ := I3DPalColorL(m.texture[i]);
    bm.Canvas.Pixels[i mod TEXDIMX, i div TEXDIMX] := dest^;
    inc(dest);
  end;

  fbitmaps.AddObject(m.texname, bm);

  glGenTextures(1, @gltex);
  glBindTexture(GL_TEXTURE_2D, gltex);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8,
               TEXDIMX, TEXDIMY,
               0, GL_RGBA, GL_UNSIGNED_BYTE, buffer);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  FreeMem(buffer, TEXDIMX * TEXDIMY * SizeOf(LongWord));

  Result := numtextures;
  m.texid := Result;
  textures[numtextures] := gltex;
  inc(numtextures);
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
    for i := 0 to numtextures - 1 do
      glDeleteTextures(1, @textures[i]);
    numtextures := 0;
  end;
  for i := 0 to fbitmaps.Count - 1 do
    fbitmaps.Objects[i].Free;
  fbitmaps.Clear;
  fselected := -1;
end;

function TI3DModel.RenderGL(const scale: single): integer;
var
  i, j: integer;
  lasttex, newtex: LongWord;

  procedure _glcolor(const m: O3DM_TMaterial_p);
  var
    cl: i3dcolor3f_t;
  begin
    cl := I3DPalColor3f(m.color);
    if m.flags and O3DMF_TRANS <> 0 then
      glColor4f(cl.r, cl.g, cl.b, 0.5)
    else
      glColor4f(cl.r, cl.g, cl.b, 1.0);
  end;

  procedure _gltexcoord(const tx, ty: integer);
  begin
    glTexCoord2f(- tx / 262144 / 64, ty / 262144 / 64);
  end;

  procedure _glvertex(const x, y, z: integer);
  begin
    glVertex3f(x * scale, y * scale, z * scale);
  end;

begin
  Result := 0;

  if obj = nil then
    exit;

  lasttex := $FFFFFFFF;

  for i := 0 to obj.nFaces - 1 do
  begin
    if fselected = i then
      Continue;
    if objfaces[i].h.material <> nil then
    begin
      newtex := 0;
      if objfaces[i].h.material.texid >= 0 then
        newtex := textures[objfaces[i].h.material.texid];
      if newtex > 0 then
      begin
        glEnable(GL_TEXTURE_2D);
        if newtex <> lasttex then
        begin
          glColor4f(1.0, 1.0, 1.0, 1.0);
          glBindTexture(GL_TEXTURE_2D, newtex);
          lasttex := newtex;
        end;
      end
      else
      begin
        glDisable(GL_TEXTURE_2D);
        lasttex := 0;
        _glcolor(objfaces[i].h.material);
      end;
    end
    else
    begin
      glDisable(GL_TEXTURE_2D);
      lasttex := 0;
      glColor4f(1.0, 1.0, 1.0, 1.0);
    end;

    glBegin(GL_TRIANGLE_FAN);

    for j := 0 to objfaces[i].h.nVerts - 1 do
    begin
      _gltexcoord(objfaces[i].verts[j].tx, objfaces[i].verts[j].ty);
      _glvertex(objfaces[i].verts[j].vert.x, objfaces[i].verts[j].vert.y, objfaces[i].verts[j].vert.z);
    end;

    glEnd;

    Result := Result + objfaces[i].h.nVerts - 2;
  end;
end;

end.
