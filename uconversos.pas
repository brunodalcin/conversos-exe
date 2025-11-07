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
  FPReadPNG, FPWritePNG,
  //compress
  Zstream;

type

  { TFrmConverterCompressor }

  TFrmConverterCompressor = class(TForm)
    arrow: TArrow;
    btnCompress: TPanel;
    btnSelectFiles: TPanel;
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
  originStream,
  destinyStream : TFileStream;
  compressor : TcompressionStream;
  i : integer;
  filepath_o,
  filepath_d : string;
begin
  if listbImages.Count = 0 then begin
    MessageDlg('Warning', ' No files were uploaded to be compressed. ', mtWarning, [mbOK], 0);
    Exit;
  end;
  for i:=0 to listbImages.Count - 1 do begin
    filepath_o := listbImages.Items[i];
    filepath_d := ChangeFileExt(listbImages.Items[i], '') + '-compressed' + ExtractFileExt(listbImages.Items[i]);

    if not FileExists(filepath_o) then
      raise Exception.Create('Source file not found: '+filepath_o);
    try
      originStream := TFileStream.Create(filepath_o,fmOpenRead);
      try
        destinyStream := TFileStream.Create(filepath_d,fmCreate);
        try
          compressor := TCompressionStream.create(clMax,destinyStream);
          try
            compressor.CopyFrom(originStream,originStream.Size);
          finally
            compressor.Free;
          end;
        finally
          destinyStream.Free;
        end;
      finally
        originStream.Free;
      end;

      Writeln('success ' + filepath_d);

    except
        on E: Exception do begin
          WriteLn('Error tying to compress the file: '+E.Message);
        end;
    end;
    listbImages.Clear;
  end;

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
      ShowMessage('No file selected');
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
  if pgControl.ActivePageIndex = 0 then
    lbDrop.Caption := 'Drop Images'
  else
    lbDrop.Caption := 'Drop Files';
  listbImages.Clear;
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

end.



