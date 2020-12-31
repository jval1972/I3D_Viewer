unit frm_editcorrection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, i3d_colorpickerbutton, i3d_model;

type
  TEditCorrectionForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    CancelButton: TButton;
    OKButton: TButton;
    Panel3: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Label11: TLabel;
    Panel9: TPanel;
    VertStringGrid: TStringGrid;
    Label9: TLabel;
    Label1: TLabel;
    FaceNumEdit: TEdit;
    VisibleCheckBox: TCheckBox;
    ColorPanel1: TPanel;
    ColorDialog1: TColorDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    ColorPickerButton1: TColorPickerButton;
    procedure ColorPickerButton1Click(Sender: TObject);
  public
    { Public declarations }
  end;

function OpenCorrectionForm(const model: TI3DModel; const faceid: integer): boolean;

implementation

{$R *.dfm}

uses
  i3d_structs,
  i3d_palette;

type
  savecdata_t = record
    x, y, z: integer;
    du, dv: single;
  end;
  savecdata_p = ^savecdata_t;
  savecdata_tArray = array[0..$FFF] of savecdata_t;
  Psavecdata_tArray = ^savecdata_tArray;

function OpenCorrectionForm(const model: TI3DModel; const faceid: integer): boolean;
var
  f: TEditCorrectionForm;
  face: O3DM_TFace_p;
  i: integer;
  du, dv: single;
  bck: Psavecdata_tArray;
begin
  Result := False;
  face := model.faces[faceid];
  if face = nil then
    Exit;

  f := TEditCorrectionForm.Create(nil);
  try
    f.FaceNumEdit.Text := IntToStr(faceid);
    f.VisibleCheckBox.Checked := face.h.visible;
    f.ColorPickerButton1.Color := I3DPalColorL(face.h.material.color);
    f.VertStringGrid.RowCount := face.h.nVerts + 1;
    DecimalSeparator := '.';
    GetMem(bck, face.h.nVerts * SizeOf(savecdata_t));
    for i := 0 to face.h.nVerts - 1 do
    begin
      f.VertStringGrid.Cells[0, i + 1] := IntToStr(face.verts[i].vert.x);
      f.VertStringGrid.Cells[1, i + 1] := IntToStr(face.verts[i].vert.y);
      f.VertStringGrid.Cells[2, i + 1] := IntToStr(face.verts[i].vert.z);
      model.UVtoGL(face.verts[i].tx, face.verts[i].ty, du, dv);
      f.VertStringGrid.Cells[3, i + 1] := Format('%2.6f', [du]);
      f.VertStringGrid.Cells[4, i + 1] := Format('%2.6f', [dv]);
      bck[i].x := face.verts[i].vert.x;
      bck[i].y := face.verts[i].vert.y;
      bck[i].z := face.verts[i].vert.z;
      bck[i].du := du;
      bck[i].dv := dv;
    end;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      for i := 0 to face.h.nVerts - 1 do
      begin
        model.AddCorrection(faceid, i, f.VisibleCheckBox.Checked,
          StrToIntDef(f.VertStringGrid.Cells[0, i + 1], bck[i].x),
          StrToIntDef(f.VertStringGrid.Cells[1, i + 1], bck[i].y),
          StrToIntDef(f.VertStringGrid.Cells[2, i + 1], bck[i].z),
          StrToFloatDef(f.VertStringGrid.Cells[3, i + 1], bck[i].du),
          StrToFloatDef(f.VertStringGrid.Cells[4, i + 1], bck[i].dv),
          f.ColorPickerButton1.Color
        );
      end;
      Result := True;
    end;
    FreeMem(bck, face.h.nVerts * SizeOf(savecdata_t));
  finally
    f.Free;
  end;
end;

procedure TEditCorrectionForm.FormCreate(Sender: TObject);
begin
  ColorPickerButton1 := TColorPickerButton.Create(nil);
  ColorPickerButton1.Parent := ColorPanel1;
  ColorPickerButton1.Align := alClient;
  ColorPickerButton1.OnClick := ColorPickerButton1Click;

  VertStringGrid.RowCount := 1;
  VertStringGrid.ColCount := 5;
  VertStringGrid.ColWidths[0] := 32;
  VertStringGrid.ColWidths[1] := 32;
  VertStringGrid.ColWidths[2] := 32;
  VertStringGrid.ColWidths[3] := 72;
  VertStringGrid.ColWidths[4] := 72;
  VertStringGrid.Cells[0, 0] := 'x';
  VertStringGrid.Cells[1, 0] := 'y';
  VertStringGrid.Cells[2, 0] := 'z';
  VertStringGrid.Cells[3, 0] := 'du';
  VertStringGrid.Cells[4, 0] := 'dv';
end;

procedure TEditCorrectionForm.FormDestroy(Sender: TObject);
begin
  ColorPickerButton1.Free;
end;

procedure TEditCorrectionForm.ColorPickerButton1Click(Sender: TObject);
begin
  ColorDialog1.Color := ColorPickerButton1.Color;
  if ColorDialog1.Execute then
    ColorPickerButton1.Color := ColorDialog1.Color;
end;

end.
