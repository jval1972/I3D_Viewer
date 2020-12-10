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
//  Select Model Form
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/i3dviewer/
//------------------------------------------------------------------------------

unit frm_selectmodel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSelectModelForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function SelectModelFromList(const lst: TStringList; var modelname: string): boolean;

implementation

{$R *.dfm}

function SelectModelFromList(const lst: TStringList; var modelname: string): boolean;
var
  f: TSelectModelForm;
begin
  Result := False;
  f := TSelectModelForm.Create(nil);
  try
    f.ListBox1.Items.Clear;
    f.ListBox1.Items.AddStrings(lst);
    f.ListBox1.ItemIndex := f.ListBox1.Items.IndexOf(modelname);
    if f.ListBox1.ItemIndex < 0 then
      if f.ListBox1.Items.Count > 0 then
        f.ListBox1.ItemIndex := 0;
    f.ShowModal;
    if f.ModalResult = mrOK then
      if f.ListBox1.ItemIndex >= 0 then
      begin
        Result := True;
        modelname := f.ListBox1.Items[f.ListBox1.ItemIndex];
      end;
  finally
    f.Free;
  end;
end;

procedure TSelectModelForm.ListBox1DblClick(Sender: TObject);
var
  idx: integer;
begin
  idx := ListBox1.ItemIndex;
  if (idx < 0) or (idx >= ListBox1.Items.Count) then
    exit;

  Button1.Click;
end;

end.
