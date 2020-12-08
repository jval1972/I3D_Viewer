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
//  Main Form
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/speed-game/
//------------------------------------------------------------------------------

unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, Buttons, Menus,
  StdCtrls, AppEvnts, ExtDlgs, clipbrd, ToolWin, dglOpenGL, i3d_model, 
  i3d_filemenuhistory;

type
  TForm1 = class(TForm)
    ColorDialog1: TColorDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Open2: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ApplicationEvents1: TApplicationEvents;
    OpenDialog1: TOpenDialog;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    Options1: TMenuItem;
    SavePictureDialog1: TSavePictureDialog;
    N4: TMenuItem;
    N5: TMenuItem;
    N8: TMenuItem;
    Copy1: TMenuItem;
    ToolBar1: TToolBar;
    OpenGLScrollBox: TScrollBox;
    OpenGLPanel: TPanel;
    OpenButton1: TSpeedButton;
    NewButton1: TSpeedButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    AboutButton1: TSpeedButton;
    N7: TMenuItem;
    HistoryItem0: TMenuItem;
    HistoryItem1: TMenuItem;
    HistoryItem2: TMenuItem;
    HistoryItem3: TMenuItem;
    HistoryItem4: TMenuItem;
    HistoryItem5: TMenuItem;
    HistoryItem6: TMenuItem;
    HistoryItem7: TMenuItem;
    HistoryItem8: TMenuItem;
    HistoryItem9: TMenuItem;
    ExportScreenshot1: TMenuItem;
    Wireframe1: TMenuItem;
    Renderenviroment1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure NewButton1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AboutButton1Click(Sender: TObject);
    procedure ExitButton1Click(Sender: TObject);
    procedure OpenButton1Click(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure OpenGLPanelResize(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure OpenGLPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OpenGLPanelDblClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure Wireframe1Click(Sender: TObject);
    procedure Renderenviroment1Click(Sender: TObject);
    procedure ExportScreenshot1Click(Sender: TObject);
  private
    { Private declarations }
    ffilename: string;
    changed: Boolean;
    rc: HGLRC;   // Rendering Context
    dc: HDC;     // Device Context
    glpanx, glpany: integer;
    glmousedown: integer;
    filemenuhistory: TFileMenuHistory;
    glneedsupdate: boolean;
    needsrecalc: boolean;
    closing: boolean;
    model: TI3DModel;
    procedure Idle(Sender: TObject; var Done: Boolean);
    function CheckCanClose: boolean;
    procedure DoNew;
    function DoLoadModel(const fname: string): boolean;
    procedure SetFileName(const fname: string);
    procedure UpdateStausbar;
    procedure UpdateEnable;
    procedure OnLoadTreeFileMenuHistory(Sender: TObject; const fname: string);
    procedure DoRenderGL;
    procedure Get3dPreviewBitmap(const b: TBitmap);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  i3d_gl,
  i3d_defs,
  i3d_utils;

{$R *.dfm}

resourcestring
  rsTitle = 'I3D Model Viewer';

procedure TForm1.FormCreate(Sender: TObject);
var
  pfd: TPIXELFORMATDESCRIPTOR;
  pf: Integer;
  doCreate: boolean;
begin
  Randomize;
  
  i3d_LoadSettingFromFile(ChangeFileExt(ParamStr(0), '.ini'));

  closing := False;

  filemenuhistory := TFileMenuHistory.Create(self);
  filemenuhistory.MenuItem0 := HistoryItem0;
  filemenuhistory.MenuItem1 := HistoryItem1;
  filemenuhistory.MenuItem2 := HistoryItem2;
  filemenuhistory.MenuItem3 := HistoryItem3;
  filemenuhistory.MenuItem4 := HistoryItem4;
  filemenuhistory.MenuItem5 := HistoryItem5;
  filemenuhistory.MenuItem6 := HistoryItem6;
  filemenuhistory.MenuItem7 := HistoryItem7;
  filemenuhistory.MenuItem8 := HistoryItem8;
  filemenuhistory.MenuItem9 := HistoryItem9;
  filemenuhistory.OnOpen := OnLoadTreeFileMenuHistory;

  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory9));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory8));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory7));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory6));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory5));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory4));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory3));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory2));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory1));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory0));

  model := TI3DModel.Create;

  Scaled := False;

  OpenGLPanel.Width := 3 * Screen.Width div 4;
  OpenGLPanel.Height := 3 * Screen.Height div 4;
  OpenGLPanel.DoubleBuffered := True;

  glpanx := 0;
  glpany := 0;
  glmousedown := 0;

  InitOpenGL;
  ReadExtensions;
  ReadImplementationProperties;

  // OpenGL initialisieren
  dc := GetDC(OpenGLPanel.Handle);

  // PixelFormat
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;      // PFD_TYPE_RGBA or PFD_TYPEINDEX
  pfd.cColorBits := 32;

  pf := ChoosePixelFormat(dc, @pfd);   // Returns format that most closely matches above pixel format
  SetPixelFormat(dc, pf, @pfd);

  rc := wglCreateContext(dc);    // Rendering Context = window-glCreateContext
  wglMakeCurrent(dc, rc);        // Make the DC (Form1) the rendering Context

  // Initialize GL environment variables

  glInit;

  ResetCamera;

  OpenGLPanelResize(sender);    // sets up the perspective

  glneedsupdate := True;

  needsrecalc := True;

  doCreate := True;
  if ParamCount > 0 then
    if DoLoadModel(ParamStr(1)) then
      doCreate := False;

  if DoCreate then
  begin
    SetFileName('');
    changed := False;
    glneedsupdate := True;
    needsrecalc := True;
  end;

  // when the app has spare time, render the GL scene
  Application.OnIdle := Idle;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckCanClose;
end;

function TForm1.CheckCanClose: boolean;
var
  ret: integer;
begin
  if changed then
  begin
    ret := MessageBox(Handle, 'Do you want to save changes?', PChar(rsTitle), MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL);
    if ret = IDCANCEL	then
    begin
      Result := False;
      exit;
    end;
    if ret = IDNO	then
    begin
      Result := True;
      exit;
    end;
    if ret = IDYES then
    begin
//      SaveButton1Click(self);
      Result := not changed;
      exit;
    end;
  end;
  Result := True;
end;

procedure TForm1.NewButton1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  DoNew;
  ResetCamera;
end;

procedure TForm1.SetFileName(const fname: string);
begin
  ffilename := fname;
  Caption := rsTitle;
  if ffilename <> '' then
    Caption := Caption + ' - ' + MkShortName(ffilename);
end;

procedure TForm1.DoNew;
begin
  model.clear;
end;

function TForm1.DoLoadModel(const fname: string): boolean;
var
  s: string;
begin
  if not FileExists(fname) then
  begin
    s := Format('File %s does not exist!', [MkShortName(fname)]);
    MessageBox(Handle, PChar(s), PChar(rsTitle), MB_OK or MB_ICONEXCLAMATION or MB_APPLMODAL);
    Result := False;
    Exit;
  end;

  model.LoadFromFile(fname);

  filemenuhistory.AddPath(fname);
  SetFileName(fname);
  glneedsupdate := True;
  needsrecalc := True;
  Result := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  closing := True;
  Timer1.Enabled := False;
  wglMakeCurrent(0, 0);
  wglDeleteContext(rc);

  glDeleteTextures(1, @texture1);

  stringtobigstring(filemenuhistory.PathStringIdx(0), @opt_filemenuhistory0);
  stringtobigstring(filemenuhistory.PathStringIdx(1), @opt_filemenuhistory1);
  stringtobigstring(filemenuhistory.PathStringIdx(2), @opt_filemenuhistory2);
  stringtobigstring(filemenuhistory.PathStringIdx(3), @opt_filemenuhistory3);
  stringtobigstring(filemenuhistory.PathStringIdx(4), @opt_filemenuhistory4);
  stringtobigstring(filemenuhistory.PathStringIdx(5), @opt_filemenuhistory5);
  stringtobigstring(filemenuhistory.PathStringIdx(6), @opt_filemenuhistory6);
  stringtobigstring(filemenuhistory.PathStringIdx(7), @opt_filemenuhistory7);
  stringtobigstring(filemenuhistory.PathStringIdx(8), @opt_filemenuhistory8);
  stringtobigstring(filemenuhistory.PathStringIdx(9), @opt_filemenuhistory9);
  i3d_SaveSettingsToFile(ChangeFileExt(ParamStr(0), '.ini'));

  filemenuhistory.Free;

  model.Free;
end;

procedure TForm1.AboutButton1Click(Sender: TObject);
begin
  MessageBox(
    Handle,
    PChar(Format('%s'#13#10 +
    'Version ' + I_VersionBuilt + #13#10 +
    'Copyright (c) 2020, jvalavanis@gmail.com'#13#10 +
    #13#10'Speed Haste Model Viewer.'#13#10,
        [rsTitle])),
    PChar(rsTitle),
    MB_OK or MB_ICONINFORMATION or MB_APPLMODAL);
end;

procedure TForm1.ExitButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.OpenButton1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  if OpenDialog1.Execute then
  begin
    DoLoadModel(OpenDialog1.FileName);
    ResetCamera;
  end;
end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  r: TRect;
  z: glfloat;
begin
  pt := OpenGLPanel.Parent.ScreenToClient(MousePos);
  r := OpenGLPanel.ClientRect;
  if r.Right > OpenGLScrollBox.Width then
    r.Right := OpenGLScrollBox.Width;
  if r.Bottom > OpenGLScrollBox.Height then
    r.Bottom := OpenGLScrollBox.Height;
  if PtInRect(r, pt) then
  begin
    z := camera.z - 0.5;
    z := z / 0.99;
    camera.z := z + 0.5;
    if camera.z < -20.0 then
      camera.z := -20.0;
    glneedsupdate := True;
  end;
end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  r: TRect;
  z: glfloat;
begin
  pt := OpenGLPanel.Parent.ScreenToClient(MousePos);
  r := OpenGLPanel.ClientRect;
  if r.Right > OpenGLScrollBox.Width then
    r.Right := OpenGLScrollBox.Width;
  if r.Bottom > OpenGLScrollBox.Height then
    r.Bottom := OpenGLScrollBox.Height;
  if PtInRect(r, pt) then
  begin
    z := camera.z - 0.5;
    z := z * 0.99;
    camera.z := z + 0.5;
    if camera.z > 0.5 then
      camera.z := 0.5;
    glneedsupdate := True;
  end;
end;

procedure TForm1.OpenGLPanelResize(Sender: TObject);
begin
  glViewport(0, 0, OpenGLPanel.Width, OpenGLPanel.Height);    // Set the viewport for the OpenGL window
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity;                     // Reset View
  gluPerspective(45.0, OpenGLPanel.Width / OpenGLPanel.Height, 1.0, 500.0);  // Do the perspective calculations. Last value = max clipping depth

  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glneedsupdate := True;
end;

procedure TForm1.Idle(Sender: TObject; var Done: Boolean);
begin
  if closing then
    Exit;
    
  UpdateEnable;

  Done := False;

  if needsrecalc then
    glneedsupdate := True;

  if not glneedsupdate then
    // jval: We don't need to render
    Exit;

  UpdateStausbar;

  DoRenderGL;

  glneedsupdate := False;
end;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  Idle(Sender, Done);
end;

procedure TForm1.OpenGLPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button in [mbLeft, mbRight] then
  begin
    glpanx := X;
    glpany := Y;
    if Button = mbLeft then
      glmousedown := 1
    else
      glmousedown := 2;
    SetCapture(OpenGLPanel.Handle);
  end;
end;

procedure TForm1.OpenGLPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  glmousedown := 0;
  ReleaseCapture;
end;

procedure TForm1.OpenGLPanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if glmousedown = 0 then
    exit;

  if glmousedown = 1 then
  begin
    camera.ay := camera.ay + (glpanx - X) ;/// OpenGLPanel.Width {* 2 * pi};
    camera.ax := camera.ax + (glpany - Y) ; // / OpenGLPanel.Height {* 2 * pi};
  end
  else
  begin
    camera.x := camera.x + (glpanx - X) / OpenGLPanel.Width * (camera.z - 1.0);/// OpenGLPanel.Width {* 2 * pi};
    if camera.x < -6.0 then
      camera.x := -6.0
    else if camera.x > 6.0 then
      camera.x := 6.0;

    camera.y := camera.y - (glpany - Y) / OpenGLPanel.Width * (camera.z - 1.0); // / OpenGLPanel.Height {* 2 * pi};
    if camera.y < -6.0 then
      camera.y := -6.0
    else if camera.y > 6.0 then
      camera.y := 6.0;
  end;

  glneedsupdate := True;

  glpanx := X;
  glpany := Y;
end;

procedure TForm1.OpenGLPanelDblClick(Sender: TObject);
begin
  ResetCamera;
  glneedsupdate := True;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  glneedsupdate := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  glneedsupdate := True;
end;

procedure TForm1.UpdateStausbar;
begin
  StatusBar1.Panels[0].Text := Format('Camera(x=%2.2f, y=%2.2f, z=%2.2f)', [camera.x, camera.y, camera.z]);
  StatusBar1.Panels[1].Text := Format('Rendered triangles = %d', [i3d_rendredtriangles]);
end;

procedure TForm1.UpdateEnable;
begin

end;

procedure TForm1.OnLoadTreeFileMenuHistory(Sender: TObject; const fname: string);
begin
  if not CheckCanClose then
    Exit;

  DoLoadModel(fname);
  ResetCamera;
end;

procedure TForm1.File1Click(Sender: TObject);
begin
  filemenuhistory.RefreshMenuItems;
end;

procedure TForm1.DoRenderGL;
begin
  if glneedsupdate then
  begin
    glBeginScene(OpenGLPanel.Width, OpenGLPanel.Height);
    try
      if needsrecalc then
      begin
//        tree.generate;
        needsrecalc := False;
      end;
      glRenderEnviroment;
      glRenderI3D(model);
    finally
      glEndScene(dc);
    end;
  end;
end;

procedure TForm1.Get3dPreviewBitmap(const b: TBitmap);
type
  long_a = array[0..$FFFF] of LongWord;
  Plong_a = ^long_a;
var
  L, buf: Plong_a;
  w, h: integer;
  i, j: integer;
  idx: integer;
begin
  w := OpenGLPanel.Width;
  h := OpenGLPanel.Height;
  b.Width := w;
  b.Height := h;
  b.PixelFormat := pf32bit;

  GetMem(L, w * h * SizeOf(LongWord));
  glReadPixels(0, 0, w, h, GL_BGRA, GL_UNSIGNED_BYTE, L);

  idx := 0;
  for j := 0 to h - 1 do
  begin
    buf := b.ScanLine[h - j - 1];
    for i := 0 to w - 1 do
    begin
      buf[i] := L[idx];
      Inc(idx);
    end;
  end;

  FreeMem(L, w * h * SizeOf(LongWord));
end;

procedure TForm1.Copy1Click(Sender: TObject);
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  try
    DoRenderGL; // JVAL: For some unknown reason this must be called before glReadPixels
    Get3dPreviewBitmap(b);
    Clipboard.Assign(b);
  finally
    b.Free;
  end;
end;

procedure TForm1.Options1Click(Sender: TObject);
begin
  Renderenviroment1.Checked := opt_renderevniroment;
  Wireframe1.Checked := opt_renderwireframe;
end;

procedure TForm1.Wireframe1Click(Sender: TObject);
begin
  opt_renderwireframe := not opt_renderwireframe;
  glneedsupdate := True;
end;

procedure TForm1.Renderenviroment1Click(Sender: TObject);
begin
  opt_renderevniroment := not opt_renderevniroment;
  glneedsupdate := True;
end;

procedure TForm1.ExportScreenshot1Click(Sender: TObject);
var
  b: TBitmap;
begin
  if SavePictureDialog1.Execute then
  begin
    BackupFile(SavePictureDialog1.FileName);
    b := TBitmap.Create;
    try
      DoRenderGL;
      Get3dPreviewBitmap(b);
      Clipboard.Assign(b);
      b.SaveToFile(SavePictureDialog1.FileName);
    finally
      b.Free;
    end;
  end;
end;

end.

