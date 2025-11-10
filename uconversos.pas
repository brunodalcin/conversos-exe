unit UConversos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, Arrow, ATPanelSimple, RTTICtrls,
  //convert and compress
  FPImage,
  FPReadBMP, FPWriteBMP,
  FPReadJPEG, FPWriteJPEG,
  FPReadPNG, FPWritePNG,
  zstream;

type

  { TFrmConverterCompressor }

  TFrmConverterCompressor = class(TForm)
    arrow: TArrow;
    btnCompress: TPanel;
    btnSelectFiles: TPanel;
    cbFrom: TComboBox;
    cbTo: TComboBox;
    lbCompressionLevel: TLabel;
    lbCompressionValue: TLabel;
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
    tbCompressionLevel: TTrackBar;
    tsCompressor: TTabSheet;
    tsConverter: TTabSheet;
    procedure btnCompressClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure btnConvertEnter(Sender: TObject);
    procedure btnConvertMouseEnter(Sender: TObject);
    procedure btnConvertMouseLeave(Sender: TObject);
    procedure btnSelectFilesClick(Sender: TObject);
    procedure cbFromChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure pgControlChange(Sender: TObject);
    procedure tbCompressionLevelChange(Sender: TObject);

  private
    const ESC = #27;
    const panelBg = '#1E1E1E';
    const dropBg = '#2D2A26';
    const txt = '#F2EBD5';
    const border = '#4A453E';
    const highlight = '#EAC65A';
    function GetImageReader : TFPCustomImageReader;
    function GetImageWriter : TFPCustomImageWriter ;
    function HexToColor(Hex: string): TColor;
    function GetImageReaderFromPath(filetype : string) : TFPCustomImageReader;
    procedure AttToCombobox;
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
  btnSelectFiles.Font.Color := HexToColor(txt);
  btnSelectFiles.Color := HexToColor(dropBg);
  btnSelectFiles.Font.Color := HexToColor(txt);
  btnSelectFiles.Color := HexToColor(dropBg);
  lbCompressionValue.Font.Color := HexToColor(txt);
  lbCompressionLevel.Font.Color := HexToColor(txt);

  cbFrom.Items.AddStrings(['PNG', 'JPEG', 'BMP']);
  cbFrom.ItemIndex := 0;
  AttToCombobox;
end;

procedure TFrmConverterCompressor.btnConvertEnter(Sender: TObject);
begin

end;

procedure TFrmConverterCompressor.btnConvertClick(Sender: TObject);
var
  i,pos_dot : integer;
  filepath : string;
  img : TFPCustomImage;

  // for now, i create as TFPCustom, so i can give any imagetypeclass
  reader : TFPCustomImageReader;
  writer: TFPCustomImageWriter;
begin
  if listbImages.Count = 0 then begin
    MessageDlg('Warning', ' No images were uploaded to be converter. ', mtWarning, [mbOK], 0);
    Exit;
  end;

  img    := TFPMemoryImage.Create(0,0);
  // get the TFP unit depending on what was selected
  reader := GetImageReader;
  writer := GetImageWriter;

  try
    for i:=0 to listbImages.Count - 1 do begin
    filepath := listbImages.Items[i];
    img.LoadFromFile(filepath,reader);
    pos_dot := LastDelimiter('.',listbImages.Items[i]);
    if pos_dot > 0 then
      filepath := Copy(listbImages.Items[i], 1, pos_dot - 1) //getting just path+filename without filetype ex: 'c:/images/img01.jpeg' TO 'c:/images/img01'
    else
      exit;

    //saving in the same path as the image loaded
    img.SaveToFile(filepath+'.'+LowerCase(cbTo.Text),writer);
  end;
  finally
    MessageDlg('Success', 'Images converted.. ', mtCustom, [mbOK], 0);
  end;
  listbImages.Clear;
end;

procedure TFrmConverterCompressor.btnCompressClick(Sender: TObject);
var
  img : TFPCustomImage;
  // for now, i create as TFPCustom, so i can give any imagetypeclass
  reader : TFPCustomImageReader;
  writer_png : TFPWriterPNG;
  writer_jpg : TFPWriterJPEG;
  filepath_origin,
  filepath_destiny,
  filetype: string;
  i,
  quality: integer;
begin
  if listbImages.Count = 0 then begin
    MessageDlg('Warning', ' No files were uploaded to be compressed. ', mtWarning, [mbOK], 0);
    Exit;
  end;

  case tbCompressionLevel.Position of
    1: quality := 25;
    2: quality := 50;
    3: quality := 75;
  end;

  img := TFPMemoryImage.Create(0,0);
  try
    for i:=0 to listbImages.Count - 1 do begin
      filepath_origin := listbImages.Items[i];
      filepath_destiny := ChangeFileExt(listbImages.Items[i], '') + '-compressed' + ExtractFileExt(filepath_origin);
      filetype := LowerCase(ExtractFileExt(filepath_origin));

      reader := GetImageReaderFromPath(filetype);
      try
        img.LoadFromFile(filepath_origin,reader);
      finally
        //reader already done the job
        reader.Free;
      end;

      if filetype = '.jpg' then
      begin
        writer_jpg := TFPWriterJPEG.Create;
        try
          writer_jpg.CompressionQuality := quality;
          img.SaveToFile(filepath_destiny, writer_jpg);
        finally
          writer_jpg.Free;
        end;
      end
      else if filetype = '.png' then
      begin
        writer_png := TFPWriterPNG.Create;
        try
          writer_png.CompressionLevel := TCompressionLevel(Round(quality / 100 * 9));
          img.SaveToFile(filepath_destiny, writer_png);
        finally
          writer_png.Free;
        end;
      end
      else
        raise Exception.Create('Cannot compress this format.');
    end;

  finally
    MessageDlg('Success', 'Images compressed.. ', mtCustom, [mbOK], 0);
  end;
  listbImages.Clear;
end;

procedure TFrmConverterCompressor.btnConvertMouseEnter(Sender: TObject);
begin
  (Sender as TPanel).Color := RGBToColor(140,160,120); // hover
end;

procedure TFrmConverterCompressor.btnConvertMouseLeave(Sender: TObject);
begin
  (Sender as TPanel).Color := HexToColor(dropBg);
end;

procedure TFrmConverterCompressor.btnSelectFilesClick(Sender: TObject);
var
  dialog : TOpenDialog;
begin
  dialog := TOpenDialog.Create(nil);
  try
    dialog.Title := 'Select a file';
    dialog.Filter := 'Image Files|*.png;*.jpeg;*.jpg;*.bmp';
    dialog.FilterIndex := 1; // default index
    dialog.Options := [ofFileMustExist, ofHideReadOnly]; // ensure file exists
    if dialog.Execute then
      listbImages.Items.Add(dialog.Filename)
    else
      MessageDlg('Warning', 'No file selected.. ', mtInformation, [mbOK], 0);
  finally
    dialog.Free
  end;
end;

procedure TFrmConverterCompressor.cbFromChange(Sender: TObject);
begin
  //when changing FROM combobox, the TO cannot have the same value ex FROM 'png' TO 'png'
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
        MessageDlg('Error', 'Unable to upload the image.. ', mtError, [mbOK], 0);
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
  listbImages.Clear;
end;

procedure TFrmConverterCompressor.tbCompressionLevelChange(Sender: TObject);
begin
  case tbCompressionLevel.Position of
    1: lbCompressionValue.Caption := 'Low (25%)';
    2: lbCompressionValue.Caption := 'Medium (50%)';
    3: lbCompressionValue.Caption := 'High (75%)';
  end;
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

function TFrmConverterCompressor.GetImageReader : TFPCustomImageReader;
begin
  case UpperCase(cbFrom.Text) of
    'PNG':  Result := TFPReaderPNG.Create;
    'JPEG': Result := TFPReaderJPEG.Create;
    'BMP':  Result := TFPReaderBMP.Create;
  else
    Result := nil;
  end;

end;

function TFrmConverterCompressor.GetImageWriter : TFPCustomImageWriter;
begin
  case UpperCase(cbTo.Text) of
    'PNG':  Result := TFPWriterPNG.Create;
    'JPEG': Result := TFPWriterJPEG.Create;
    'BMP':  Result := TFPWriterBMP.Create;
  else
    Result := nil;
  end;
end;

function TFrmConverterCompressor.GetImageReaderFromPath(filetype : string) : TFPCustomImageReader;
begin
  if filetype = '.jpg' then
    Result := TFPReaderJPEG.Create
  else if filetype = '.png' then
    Result := TFPReaderPNG.Create
  else
    raise Exception.Create('Unsupported format: ' + filetype);
end;

end.



