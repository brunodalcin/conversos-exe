unit UConversos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, FileCtrl, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, Arrow, ATPanelColor, ATPanelSimple, ATListbox,
  RTTICtrls,
  //converters
  FPImage,
  FPReadBMP, FPWriteBMP,
  FPReadJPEG, FPWriteJPEG,
  FPReadPNG, FPWritePNG;

type

  { TFrmConverterCompressor }

  TFrmConverterCompressor = class(TForm)
    arrow: TArrow;
    btnCompress: TPanel;
    cbFrom: TComboBox;
    cbTo: TComboBox;
    lbClose: TLabel;
    lbDrop: TLabel;
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
    procedure arrowClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure btnConvertEnter(Sender: TObject);
    procedure btnConvertMouseEnter(Sender: TObject);
    procedure btnConvertMouseLeave(Sender: TObject);
    procedure cbFromChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure pgControlChange(Sender: TObject);

  private
    const ESC = #27;
    const panelBg = '#1E1E1E';
    const dropBg = '#2D2A26';
    const txt = '#F2EBD5';
    const border = '#4A453E';
    const highlight = '#EAC65A';
    function HexToColor(Hex: string): TColor;
    procedure AttToCombobox;
    //procedure Convert(const InputFile, OutputFile : string);
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
  lbDrop.Font.Color := HexToColor(txt);
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
  btnCompress.Font.Color := HexToColor(txt);
  btnCompress.Color := HexToColor(dropBg);
  arrow.ArrowColor := HexToColor(txt);
  arrow.Color := HexToColor(PanelBg);

  cbFrom.Items.AddStrings(['PNG', 'JPEG', 'BMP']);
  cbFrom.ItemIndex := 0;
  AttToCombobox;
end;

procedure TFrmConverterCompressor.btnConvertEnter(Sender: TObject);
begin

end;

procedure TFrmConverterCompressor.btnConvertClick(Sender: TObject);
var
  img : TFPCustomImage;
  reader : TFPCustomImageReader;
  writer: TFPWriterPNG;
  i,pos_dot : integer;
  filepath : string;
begin
  //Convert()
  //testing just jpeg to png for now
  img    := TFPMemoryImage.Create(0,0);
  reader := TFPReaderJPEG.Create;
  writer := TFPWriterPNG.Create;

  for i:=0 to listbImages.Count - 1 do begin
    filepath := listbImages.Items[i];
    img.LoadFromFile(filepath,reader);
    //MUST have .filetype
    pos_dot := LastDelimiter('.',listbImages.Items[i]);
    if pos_dot > 0 then
      filepath := Copy(listbImages.Items[i], 1, pos_dot - 1) //getting just path+filename without filetype ex: 'c:/images/img01.jpeg' TO 'c:/images/img01'
    else
      exit;

    //saving in the same path as the image loaded
    img.SaveToFile(filepath+'.png',writer);
  end;

  showmessage('done');
  listbImages.Clear;

end;

procedure TFrmConverterCompressor.arrowClick(Sender: TObject);
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

procedure TFrmConverterCompressor.cbFromChange(Sender: TObject);
begin
  //when changing FROM combobox, the TO cannot have the same value ex FROM 'jpg' TO 'jpg'
  AttToCombobox;
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

procedure TFrmConverterCompressor.pgControlChange(Sender: TObject);
begin
  if pgControl.ActivePageIndex = 0 then
    lbDrop.Caption := 'Drop Images'
  else
    lbDrop.Caption := 'Drop Files';
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

procedure TFrmConverterCompressor.AttToCombobox;
var
  i: Integer;
begin
  cbTo.Items.BeginUpdate;
  try
    // always clear to update
    cbTo.Items.Clear;
    for i := 0 to cbFrom.Items.Count - 1 do
    begin
      // do not add same IMAGETYPE as selected EX: FROM png TO png
      if cbFrom.Items[i] <> cbFrom.Text then
        cbTo.Items.Add(cbFrom.Items[i]);
    end;
    // select first index
    if cbTo.Items.Count > 0 then
      cbTo.ItemIndex := 0;
  finally
    cbTo.Items.EndUpdate;
  end;
end;

{procedure TFrmConverterCompressor.Convert(const InputFile, OutputFile : string);
var
  Img: TFPMemoryImage;
  Reader: TFPCustomImageReader;
  Writer: TFPCustomImageWriter;
begin

end;}

end.

