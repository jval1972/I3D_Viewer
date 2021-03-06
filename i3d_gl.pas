//------------------------------------------------------------------------------
//
//  I3D_Viewer - Model Viewer for Speed Haste models
//  Copyright (C) 2020-2021 by Jim Valavanis
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
//  OpenGL Rendering
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/i3dviewer/
//------------------------------------------------------------------------------

unit i3d_gl;

interface

uses
  Windows,
  Graphics,
  dglOpenGL,
  i3d_model;

var
  gld_max_texturesize: integer = 0;
  gl_tex_format: integer = GL_RGBA8;
  gl_tex_filter: integer = GL_LINEAR;

procedure glInit;

procedure ResetCamera;

procedure glBeginScene(const Width, Height: integer);
procedure glEndScene(dc: HDC);
procedure glRenderEnviroment;
procedure glRenderI3D(const t: TI3DModel);

type
  TCDCamera = record
    x, y, z: glfloat;
    ax, ay, az: glfloat;
  end;

var
  camera: TCDCamera;

var
  i3d_rendredtriangles: integer = 0;

implementation

uses
  SysUtils,
  Classes,
  Math,
  i3d_defs;

procedure ResetCamera;
begin
  camera.x := 0.0;
  camera.y := -3.0;
  camera.z := -10.0;
  camera.ax := 0.0;
  camera.ay := 0.0;
  camera.az := 0.0;
end;


{------------------------------------------------------------------}
{  Initialise OpenGL                                               }
{------------------------------------------------------------------}
procedure glInit;
begin
  glClearColor(0.0, 0.0, 0.0, 0.0);   // Black Background
  glShadeModel(GL_SMOOTH);            // Enables Smooth Color Shading
  glClearDepth(1.0);                  // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);            // Enable Depth Buffer
  glDepthFunc(GL_LESS);		            // The Type Of Depth Test To Do
  glEnable(GL_POINT_SIZE);

  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @gld_max_texturesize);

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);   //Realy Nice perspective calculations
  glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
end;

procedure infinitePerspective(fovy: GLdouble; aspect: GLdouble; znear: GLdouble);
var
  left, right, bottom, top: GLdouble;
  m: array[0..15] of GLdouble;
begin
  top := znear * tan(fovy * pi / 360.0);
  bottom := -top;
  left := bottom * aspect;
  right := top * aspect;

  m[ 0] := (2 * znear) / (right - left);
  m[ 4] := 0;
  m[ 8] := (right + left) / (right - left);
  m[12] := 0;

  m[ 1] := 0;
  m[ 5] := (2 * znear) / (top - bottom);
  m[ 9] := (top + bottom) / (top - bottom);
  m[13] := 0;

  m[ 2] := 0;
  m[ 6] := 0;
  m[10] := -1;
  m[14] := -2 * znear;

  m[ 3] := 0;
  m[ 7] := 0;
  m[11] := -1;
  m[15] := 0;

  glMultMatrixd(@m);
end;

procedure glBeginScene(const Width, Height: integer);
begin
  glDisable(GL_CULL_FACE);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  infinitePerspective(64.0, width / height, 0.01);

  glMatrixMode(GL_MODELVIEW);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);    // Clear The Screen And The Depth Buffer
  glLoadIdentity;                                       // Reset The View

  glTranslatef(camera.x, camera.y, camera.z);
  glRotatef(camera.az, 0, 0, 1);
  glRotatef(camera.ay, 0, 1, 0);
  glRotatef(camera.ax, 1, 0, 0);
end;

procedure glEndScene(dc: HDC);
begin
  SwapBuffers(dc);                                // Display the scene
end;

procedure glRenderEnviroment;
const
  DRUNIT = 5;
  DREPEATS = 10;
  DWORLD = DREPEATS + 1;
var
  i: integer;
begin
  if opt_renderevniroment then
  begin
    glColor3f(1.0, 1.0, 1.0);

    glBegin(GL_LINES);
      for i := -DREPEATS to DREPEATS do
      begin
        if i = 0 then
          glColor3f(1.0, 1.0, 0.0);
        glVertex3f((DREPEATS + 1) * DRUNIT, 0.0, i * DRUNIT);
        glVertex3f(-(DREPEATS + 1) * DRUNIT, 0.0, i * DRUNIT);
        if i = 0 then
          glColor3f(1.0, 1.0, 1.0);
      end;

      for i := -DREPEATS to DREPEATS do
      begin
        if i = 0 then
          glColor3f(1.0, 1.0, 0.0);
        glVertex3f(i * DRUNIT, 0.0, (DREPEATS + 1) * DRUNIT);
        glVertex3f(i * DRUNIT, 0.0, -(DREPEATS + 1) * DRUNIT);
        if i = 0 then
          glColor3f(1.0, 1.0, 1.0);
      end;

      glColor3f(1.0, 1.0, 0.0);
      glVertex3f(0.0, (DREPEATS + 1) * DRUNIT, 0.0);
      glVertex3f(0.0, -(DREPEATS + 1) * DRUNIT, 0.0);
    glEnd;

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    glColor3f(0.0, 0.5, 0.0);
    glBegin(GL_QUADS);
      glVertex3f(DWORLD * DRUNIT, -DWORLD * DRUNIT, DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, -DWORLD * DRUNIT, -DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, -DWORLD * DRUNIT, -DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, -DWORLD * DRUNIT, DWORLD * DRUNIT);

      glVertex3f(DWORLD * DRUNIT, 0.0, DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, 0.0, -DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, -DWORLD * DRUNIT, -DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, -DWORLD * DRUNIT, DWORLD * DRUNIT);

      glVertex3f(DWORLD * DRUNIT, 0.0, -DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, 0.0, -DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, -DWORLD * DRUNIT, -DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, -DWORLD * DRUNIT, -DWORLD * DRUNIT);

      glVertex3f(-DWORLD * DRUNIT, 0.0, -DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, 0.0, DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, -DWORLD * DRUNIT, DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, -DWORLD * DRUNIT, -DWORLD * DRUNIT);

      glVertex3f(-DWORLD * DRUNIT, 0.0, DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, 0.0, DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, -DWORLD * DRUNIT, DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, -DWORLD * DRUNIT, DWORLD * DRUNIT);
    glEnd;

    glDisable(GL_CULL_FACE);
    glColor3f(0.4, 0.4, 1.0);
    glBegin(GL_QUADS);
      glVertex3f(DWORLD * DRUNIT, DWORLD * DRUNIT, DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, DWORLD * DRUNIT, -DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, DWORLD * DRUNIT, -DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, DWORLD * DRUNIT, DWORLD * DRUNIT);

      glVertex3f(DWORLD * DRUNIT, -DRUNIT / 50, DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, -DRUNIT / 50, -DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, DWORLD * DRUNIT, -DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, DWORLD * DRUNIT, DWORLD * DRUNIT);

      glVertex3f(DWORLD * DRUNIT, -DRUNIT / 50, -DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, -DRUNIT / 50, -DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, DWORLD * DRUNIT, -DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, DWORLD * DRUNIT, -DWORLD * DRUNIT);

      glVertex3f(-DWORLD * DRUNIT, -DRUNIT / 50, -DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, -DRUNIT / 50, DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, DWORLD * DRUNIT, DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, DWORLD * DRUNIT, -DWORLD * DRUNIT);

      glVertex3f(-DWORLD * DRUNIT, -DRUNIT / 50, DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, -DRUNIT / 50, DWORLD * DRUNIT);
      glVertex3f(DWORLD * DRUNIT, DWORLD * DRUNIT, DWORLD * DRUNIT);
      glVertex3f(-DWORLD * DRUNIT, DWORLD * DRUNIT, DWORLD * DRUNIT);
    glEnd;
  end;
end;

procedure glRenderI3D(const t: TI3DModel);
const
  DEF_SCALE = 0.001;
begin
  if opt_renderwireframe then
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE )
  else
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );

  // Render selected object
  glColor4f(1.0, 0.0, 0.0, 1.0);

  glDisable(GL_TEXTURE_2D);

  t.RenderSelectionCubeGL(DEF_SCALE);

  // Render model
  glColor4f(1.0, 1.0, 1.0, 1.0);

  glEnable(GL_TEXTURE_2D);

  glDisable(GL_BLEND);
  glDisable(GL_ALPHA_TEST);

  i3d_rendredtriangles := t.RenderGL(DEF_SCALE);

  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_TEXTURE_2D);
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
end;

type
  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array[0..$FFFF] of LongWord;

function RGBSwap(const l: LongWord): LongWord;
var
  A: packed array[0..3] of byte;
  tmp: byte;
begin
  PLongWord(@A)^ := l;
  tmp := A[0];
  A[0] := A[2];
  A[2] := tmp;
  Result := PLongWord(@A)^;
end;

end.
