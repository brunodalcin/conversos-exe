unit UConversos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, FileCtrl, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, ATPanelColor, ATPanelSimple, ATListbox,
  RTTICtrls;

type

  { TFrmConverterCompressor }

  TFrmConverterCompressor = class(TForm)
    btnConverter1: TBitBtn;
    cbFrom: TComboBox;
    cbTo: TComboBox;
    lbClose: TLabel;
    lbDropImages: TLabel;
    lbFrom: TLabel;
    lbTo: TLabel;
    listbImages: TListBox;
    panel: TATPanelSimple;
    btnConvert: TPanel;
    panelConverter: TPanel;
    panelCompressor: TPanel;
    pgControl: TPageControl;
    tsCompressor: TTabSheet;
    tsConverter: TTabSheet;
    procedure btnConvertEnter(Sender: TObject);
    procedure btnConvertMouseEnter(Sender: TObject);
    procedure btnConvertMouseLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyPress(Sender: TObject; var Key: char);



  private
    const ESC = #27;
    const panelBg = '#1E1E1E';
    const dropBg = '#2D2A26';
    const txt = '#F2EBD5';
    const border = '#4A453E';
    const highlight = '#EAC65A';
    function HexToColor(Hex: string): TColor;
  public

  end;

var
  FrmConverterCompressor: TFrmConverterCompressor;

implementation

{$R *.lfm}

{ TFrmConverterCompressor }

procedure TFrmConverterCompressor.FormCreate(Sender: TObject);
begin
  pgControl.ActivePageIndex:=0;
  panel.Color:= HexToColor(panelBg);
  listbImages.Color := HexToColor(dropBg);
  listbImages.Font.Color := HexToColor(txt);
  lbDropImages.Font.Color := HexToColor(txt);
  lbClose.Font.Color := HexToColor(txt);
  panelConverter.Color := HexToColor(dropBg);
  panelCompressor.Color := HexToColor(dropBg);
  cbFrom.Color := HexToColor(dropBg);
  cbFrom.Font.Color := HexToColor(txt);
  lbFrom.Font.Color := HexToColor(txt);
  cbTo.Color := HexToColor(dropBg);
  cbTo.Font.Color := HexToColor(txt);
  lbTo.Font.Color := HexToColor(txt);
  btnConvert.Font.Color := HexToColor(txt);
  btnConvert.Color := HexToColor(dropBg);

end;

procedure TFrmConverterCompressor.btnConvertEnter(Sender: TObject);
begin

end;

procedure TFrmConverterCompressor.btnConvertMouseEnter(Sender: TObject);
begin
  (Sender as TPanel).Color := RGBToColor(140,160,120); // hover
end;

procedure TFrmConverterCompressor.btnConvertMouseLeave(Sender: TObject);
begin
  (Sender as TPanel).Color := HexToColor(dropBg);
end;

procedure TFrmConverterCompressor.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
var
  filename : string;
begin
  for filename in FileNames do
  begin
    try
      listbImages.Items.Add(filename);
    except
      showmessage('error');
    end;
  end;
end;

procedure TFrmConverterCompressor.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = ESC then
    Close;
end;


function TFrmConverterCompressor.HexToColor(Hex: string): TColor;
var
  R, G, B: Byte;
begin
  if Hex[1] = '#' then
    Delete(Hex, 1, 1);

  R := StrToInt('$' + Copy(Hex, 1, 2));
  G := StrToInt('$' + Copy(Hex, 3, 2));
  B := StrToInt('$' + Copy(Hex, 5, 2));

  Result := RGBToColor(R, G, B);
end;


end.

