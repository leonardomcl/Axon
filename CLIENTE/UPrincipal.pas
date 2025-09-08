unit UPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.Memo, FMX.Edit, FMX.Layouts, FMX.ListBox, FMX.Menus, FMX.ScrollBox,
  System.IOUtils, UClientManager, UTypes, System.Threading;

type
  TFPrincipal = class(TForm)
    StatusBar1: TStatusBar;
    MLog: TMemo;
    GroupBoxServidor: TGroupBox;
    EdtServer: TEdit;
    LbHost: TLabel;
    LbPort: TLabel;
    EdtPort: TEdit;
    LbPass: TLabel;
    EdtPass: TEdit;
    BtnCon: TCornerButton;
    BtnDesc: TCornerButton;
    GroupBoxFiles: TGroupBox;
    ListServerFiles: TListBox;
    RecFileUpload: TRectangle;
    ProgressBar1: TProgressBar;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    StyleBook1: TStyleBook;
    ImgFileUpload: TImage;
    MenuItem3: TMenuItem;
    PopupMenuServerFiles: TPopupMenu;
    PopMenuItemDownload: TMenuItem;
    PopMenuItemRefresh: TMenuItem;
    TimerLogEnd: TTimer;
    Label1: TLabel;

    procedure BtnConClick(Sender: TObject);
    procedure BtnDescClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure ImgFileUploadDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure ImgFileUploadDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure FormShow(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure PopMenuItemDownloadClick(Sender: TObject);
    procedure PopMenuItemRefreshClick(Sender: TObject);
    procedure TimerLogEndTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private

  public
    // Instância da nova classe
    FClient: TClientManager;
    procedure LogMessage(const AMessage: string);
    procedure UpdateConnectionState(AConnected: Boolean; const AReason: string);
    procedure UpdateFilesList(const AJsonFiles: string);
    procedure AddChatMessage(const AChatMessage: string);
    procedure FileUploadSuccess;
    procedure FileUploadError(const AMessage: string);
    procedure UpdateFileProgress(const ATotal, ATransferred: Int64);
    property Client: TClientManager read FClient;

  end;

var
  FPrincipal: TFPrincipal;
  DiretorioSelecionado: String;

implementation

uses
  UFunctions, UChat, UConfig;

{$R *.fmx}
{ TFPrincipal }

procedure TFPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (FClient.IsConnected) then
  begin
    FClient.SendWriteLnCommand(CMD_BYE);
  end;
end;

procedure TFPrincipal.FormCreate(Sender: TObject);
begin
  // Cria a instância do gerenciador de cliente
  FClient := TClientManager.Create;

  // Atribui os handlers dos eventos para atualizar a UI
  FClient.OnLogMessage := LogMessage;
  FClient.OnConnectionStateChange := UpdateConnectionState;
  FClient.OnFilesListReceived := UpdateFilesList;
  FClient.OnChatMessageReceived := AddChatMessage;
  FClient.OnFileUploadSuccess := FileUploadSuccess;
  FClient.OnFileUploadError := FileUploadError;
  FClient.OnFileProgress := UpdateFileProgress;

end;

procedure TFPrincipal.FormDestroy(Sender: TObject);
begin
  FClient.Free;
end;

procedure TFPrincipal.FormShow(Sender: TObject);
begin
  DiretorioSelecionado := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)),
    'Recebidos');
  ForceDirectories(DiretorioSelecionado);
  if Assigned(FConfig) then
    FConfig.EdtPath.Text := DiretorioSelecionado;
end;

// ===================================
// Handlers de eventos do TClientManager
// (aqui a UI é atualizada)
// ===================================

procedure TFPrincipal.LogMessage(const AMessage: string);
begin
  MLog.Lines.Add(AMessage);
end;

procedure TFPrincipal.UpdateConnectionState(AConnected: Boolean;
  const AReason: string);
begin
  if not AConnected then
  begin
    if AReason <> '' then
      MLog.Lines.Add(AReason);
  end;
  BtnCon.Enabled := not AConnected;
  BtnDesc.Enabled := AConnected;
  if not AConnected then
    ListServerFiles.Clear;
end;

procedure TFPrincipal.AddChatMessage(const AChatMessage: string);
begin
  if Assigned(FChat) then
    FChat.MReceivedMessage.Lines.Add(AChatMessage.Trim);
end;

procedure TFPrincipal.UpdateFilesList(const AJsonFiles: string);
var
  JsonValue: TJSONValue;
  JsonArray: TJSONArray;
  Item: TJSONValue;
begin
  ListServerFiles.Clear;
  JsonValue := TJSONObject.ParseJSONValue(AJsonFiles);
  try
    if Assigned(JsonValue) and (JsonValue is TJSONArray) then
    begin
      JsonArray := JsonValue as TJSONArray;
      ListServerFiles.BeginUpdate;
      for Item in JsonArray do
        ListServerFiles.Items.Add(Item.Value);
      ListServerFiles.EndUpdate;
    end
    else
    begin
      showMessage('Erro ao parsear a string JSON de arquivos.');
    end;
  finally
    JsonValue.Free;
  end;
end;

procedure TFPrincipal.FileUploadSuccess;
begin
  ProgressBar1.Max := 100;
  ProgressBar1.Value := 0;
end;

procedure TFPrincipal.FileUploadError(const AMessage: string);
begin
  MLog.Lines.Add(AMessage);
  ProgressBar1.Max := 100;
  ProgressBar1.Value := 0;
end;

// ==========================================================
// NOVO MÉTODO IMPLEMENTADO PARA ATUALIZAR A PROGRESSBAR
// ==========================================================
procedure TFPrincipal.UpdateFileProgress(const ATotal, ATransferred: Int64);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      if ATotal > 0 then
      begin
        // Atualiza a barra de progresso
        ProgressBar1.Max := ATotal;
        ProgressBar1.Value := ATransferred;

      end
      else
      begin
        // Zera a barra de progresso
        ProgressBar1.Max := 100;
        ProgressBar1.Value := 0;
      end;
    end);
end;

// ===================================
// Eventos dos botões e componentes da UI
// ===================================

procedure TFPrincipal.BtnConClick(Sender: TObject);
begin
  if EdtServer.Text.Trim = '' then
  begin
    showMessage('⚠ Coloque um IP válido');
    Exit;
  end;
  if EdtPort.Text.Trim = '' then
  begin
    showMessage('⚠ Coloque uma Porta válida');
    Exit;
  end;

  FClient.Connect(EdtServer.Text.Trim, EdtPort.Text.ToInteger,
    EdtPass.Text.Trim, GetComputerName);
end;

procedure TFPrincipal.BtnDescClick(Sender: TObject);
begin
  FClient.Disconnect('🔴 Desconexão manual pelo usuário.');
end;

procedure TFPrincipal.ImgFileUploadDragDrop(Sender: TObject;
const Data: TDragObject; const Point: TPointF);
var
  i: Integer;
begin
  for i := Low(Data.Files) to High(Data.Files) do
  begin
    // O progresso pode ser implementado via eventos do ClientManager
    ProgressBar1.Max := 100; // Simplesmente para mostrar atividade
    ProgressBar1.Value := 50;
    FClient.SendFile(Data.Files[i]);
  end;
end;

procedure TFPrincipal.ImgFileUploadDragOver(Sender: TObject;
const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if (Length(Data.Files) > 0) and FClient.IsConnected then
    Operation := TDragOperation.Copy
  else
    Operation := TDragOperation.None;
end;

procedure TFPrincipal.MenuItem1Click(Sender: TObject);
begin
  if FClient.IsConnected then
    FChat.show;
end;

procedure TFPrincipal.MenuItem2Click(Sender: TObject);
begin
  if FClient.IsConnected then
  begin
    FClient.SendWriteLnCommand(CMD_PING);
    MLog.Lines.Add('➡ Ping enviado');
  end;
end;

procedure TFPrincipal.MenuItem3Click(Sender: TObject);
begin
  FConfig.show;
end;

procedure TFPrincipal.PopMenuItemDownloadClick(Sender: TObject);
begin
  if (ListServerFiles.Selected <> nil) and FClient.IsConnected then
  begin
    FClient.SendWriteLnCommand(CMD_DOWNLOAD_FILE);
    FClient.SendWriteLnCommand(ListServerFiles.Items
      [ListServerFiles.ItemIndex]);
  end;
end;

procedure TFPrincipal.PopMenuItemRefreshClick(Sender: TObject);
begin
  FClient.SendWriteLnCommand(CMD_GET_LIST_FILES);
end;

procedure TFPrincipal.TimerLogEndTimer(Sender: TObject);
begin

  if MLog.Lines.Count > 1000 then
    MLog.Lines.Delete(0);
  MLog.GoToTextEnd;

end;

end.
