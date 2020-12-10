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
