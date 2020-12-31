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
//  OMain Program
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/i3dviewer/
//------------------------------------------------------------------------------

program I3DViewer;

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  Forms,
  main in 'main.pas' {Form1},
  dglOpenGL in 'dglOpenGL.pas',
  i3d_gl in 'i3d_gl.pas',
  i3d_filemenuhistory in 'i3d_filemenuhistory.pas',
  i3d_utils in 'i3d_utils.pas',
  i3d_model in 'i3d_model.pas',
  i3d_structs in 'i3d_structs.pas',
  i3d_palette in 'i3d_palette.pas',
  jcl_file in 'jcl_file.pas',
  frm_selectmodel in 'frm_selectmodel.pas' {SelectModelForm},
  i3d_scriptengine in 'i3d_scriptengine.pas',
  frm_editcorrection in 'frm_editcorrection.pas' {EditCorrectionForm},
  i3d_colorpickerbutton in 'i3d_colorpickerbutton.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'I3D Model Viewer';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

