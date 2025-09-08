unit USendFile;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, IdGlobal,
  IdContext, System.Threading, System.IOUtils, FMX.ListBox, FMX.Menus;

type
  TFSendFile = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    ProgressFile: TProgressBar;
    RecFileUpload: TRectangle;
    Image1: TImage;
    Label1: TLabel;
    ListClientFiles: TListBox;
    PopupMenu1: TPopupMenu;
    PopMenuItemRefresh: TMenuItem;
    PopMenuItemDownload: TMenuItem;
    Text1: TText;
    procedure Image1DragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure Image1DragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PopMenuItemRefreshClick(Sender: TObject);
    procedure PopMenuItemDownloadClick(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    procedure SendFile(const AArquivoLocal: string);
    procedure SendThreadSafeFile(const AArquivo: string);
    function SendWriteLnCommand(const command: string): Boolean;
  end;

var
  FSendFile: TFSendFile;
  FileUploadSelectedContext: TIdContext;
  EnvioLock: TObject;

implementation

uses
  UPrincipal, Server.Types, Functions;

{$R *.fmx}

function TFSendFile.SendWriteLnCommand(const command: string): Boolean;
begin
  Result := False;
  if not FileUploadSelectedContext.Connection.Connected then
    Exit;

  try
    FileUploadSelectedContext.Connection.IOHandler.WriteLn(command,
      IndyTextEncoding_UTF8);
    Result := True;
  except
    on E: Exception do
    begin
      TThread.Queue(nil,
        procedure
        begin
          showMessage('Erro ao enviar comando: ' + E.Message);
        end);

    end;
  end;
end;

procedure TFSendFile.SendFile(const AArquivoLocal: string);
var
  FileStream: TFileStream;
  Buffer: TIdBytes;
  BytesRead: Integer;
  TotalSent, FileSize: Int64;
  ChunkSize: Integer;
begin
  if not FileUploadSelectedContext.Connection.Connected then
    Exit;

  FileSize := TFile.GetSize(AArquivoLocal);
  if FileSize > FILE_SIZE_LIMIT then // limite de 1GB (ajuste se quiser)
    raise Exception.Create('Arquivo muito grande');

  FileStream := TFileStream.Create(AArquivoLocal, fmOpenRead or
    fmShareDenyWrite);
  try
    // Protocolo: comando + nome + tamanho + bytes
    if not SendWriteLnCommand(CMD_SEND_FILE) then
      Exit;
    if not SendWriteLnCommand(ExtractFileName(AArquivoLocal)) then
      Exit;

    FileUploadSelectedContext.Connection.IOHandler.Write(FileStream.Size);

    // Envia em chunks de 256KB
    ChunkSize := 262144;
    SetLength(Buffer, ChunkSize);
    TotalSent := 0;

    TThread.Queue(nil,
      procedure
      begin
        ProgressFile.Max := FileStream.Size;
        ProgressFile.Value := 0;
      end);

    while TotalSent < FileStream.Size do
    begin
      BytesRead := FileStream.Read(Buffer[0], ChunkSize);
      if BytesRead <= 0 then
        Break;

      FileUploadSelectedContext.Connection.IOHandler.Write(Buffer, BytesRead);
      Inc(TotalSent, BytesRead);

      // Atualiza UI com progresso
      TThread.Queue(nil,
        procedure
        begin
          ProgressFile.Value := TotalSent;
        end);
    end;
  finally
    FileStream.Free;
    SetLength(Buffer, 0);
  end;
end;

procedure TFSendFile.SendThreadSafeFile(const AArquivo: string);
begin
  TTask.Run(
    procedure
    begin
      TMonitor.Enter(EnvioLock);
      try
        try
          SendFile(AArquivo);
          TThread.Queue(nil,
            procedure
            begin
              FileUploadSelectedContext.Connection.IOHandler.WriteLn
                (CMD_GET_LIST_FILES);
              FServidor.MemoLog.Lines.Add('✅ Arquivo enviado: ' + AArquivo);
              ProgressFile.Max := 100;
              ProgressFile.Value := 0;
            end);
        except
          on E: Exception do
            TThread.Queue(nil,
              procedure
              begin
                showMessage('❌ Erro ao enviar ' + AArquivo + ': ' + E.Message);
                ProgressFile.Max := 100;
                ProgressFile.Value := 0;
              end);
        end;
      finally
        TMonitor.Exit(EnvioLock);
      end;
    end);
end;

procedure TFSendFile.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  freeandnil(EnvioLock);
end;

procedure TFSendFile.FormShow(Sender: TObject);
begin
  EnvioLock := TObject.Create;
end;

procedure TFSendFile.Image1DragDrop(Sender: TObject; const Data: TDragObject;
const Point: TPointF);
var
  i: Integer;
  FileName: string;
begin
  if Length(Data.Files) > 0 then
  begin
    for i := Low(Data.Files) to High(Data.Files) do
    begin
      FileName := Data.Files[i];
      DelayAsync(300,
        procedure
        begin
          SendThreadSafeFile(FileName);
        end);
    end;
  end;
end;

procedure TFSendFile.Image1DragOver(Sender: TObject; const Data: TDragObject;
const Point: TPointF; var Operation: TDragOperation);
begin
  if Length(Data.Files) > 0 then
    Operation := TDragOperation.Copy
  else
    Operation := TDragOperation.None;
end;

procedure TFSendFile.PopMenuItemDownloadClick(Sender: TObject);
begin
  FileUploadSelectedContext.Connection.IOHandler.WriteLn(CMD_DOWNLOAD_FILE);
  FileUploadSelectedContext.Connection.IOHandler.WriteLn
    (ListClientFiles.Items[ListClientFiles.ItemIndex]);
end;

procedure TFSendFile.PopMenuItemRefreshClick(Sender: TObject);
begin
  FileUploadSelectedContext.Connection.IOHandler.WriteLn(CMD_GET_LIST_FILES);
end;

end.
