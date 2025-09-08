unit UPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.Edit, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Menus, System.IOUtils,
  IdContext, System.JSON,
  // Nossas novas units
  Server.Core, Server.Types;

type
  TFServidor = class(TForm)
    Layout1: TLayout;
    GroupBox1: TGroupBox;
    Switch1: TSwitch;
    LbEdtPort: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LbEdtPass: TEdit;
    Label3: TLabel;
    EdtPath: TEdit;
    BtnSelDir: TButton;
    PopupMenu1: TPopupMenu;
    PopSendPing: TMenuItem;
    TClearLoginAttpts: TTimer;
    PopSendMessages: TMenuItem;
    PopSendFile: TMenuItem;
    PopSendDisconnect: TMenuItem;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuClearBanList: TMenuItem;
    StyleBook1: TStyleBook;
    GbLog: TGroupBox;
    MemoLog: TMemo;
    GroupBox2: TGroupBox;
    ListViewClientes: TListView;
    procedure Switch1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnSelDirClick(Sender: TObject);
    procedure TClearLoginAttptsTimer(Sender: TObject);
    procedure MenuClearBanListClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PopSendDisconnectClick(Sender: TObject);
    procedure PopSendPingClick(Sender: TObject);
    procedure PopSendMessagesClick(Sender: TObject);
    procedure PopSendFileClick(Sender: TObject);
  private
    FServer: TServerCore;
    procedure ServerLogHandler(const AMessage: string);
    procedure ServerListFilesHandler(const AJsonFiles: string);
    procedure ServerClientConnectedHandler(AContext: TIdContext;
      const AComputerName, AIP: string);
    procedure ServerClientDisconnectedHandler(AContext: TIdContext);
  public
    { Public declarations }
  end;

var
  FServidor: TFServidor;

implementation

{$R *.fmx}

uses System.DateUtils, UChat, USendFile, Functions;

{ TFServidor }

procedure TFServidor.FormCreate(Sender: TObject);
begin
  // Cria a instância do nosso core do servidor
  FServer := TServerCore.Create(Self);
  // Conecta os eventos do core aos handlers do formulário
  FServer.OnLog := ServerLogHandler;
  FServer.OnFileList := ServerListFilesHandler;

  FServer.OnClientConnected := ServerClientConnectedHandler;
  FServer.OnClientDisconnected := ServerClientDisconnectedHandler;

  // Configurações iniciais da UI
  LbEdtPass.Text := GerarStringAleatoria(12, true, true);
  // Mantenha sua lógica de geração de senha
  EdtPath.Text := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)),
    'Recebidos');
  ForceDirectories(EdtPath.Text);

  TClearLoginAttpts.Interval := 60000; // 1 minuto
end;

procedure TFServidor.FormDestroy(Sender: TObject);
begin
  FServer.Free;
end;

procedure TFServidor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FServer.Stop;
end;

procedure TFServidor.Switch1Click(Sender: TObject);
begin
  if Switch1.IsChecked then
  begin
    FServer.Port := StrToIntDef(LbEdtPort.Text, 58960);
    FServer.Password := LbEdtPass.Text;
    FServer.SavePath := EdtPath.Text;
    FServer.Start;
    TClearLoginAttpts.Enabled := true;
  end
  else
  begin
    FServer.Stop;
    TClearLoginAttpts.Enabled := False;
  end;
end;

procedure TFServidor.ServerListFilesHandler(const AJsonFiles: string);
var
  JsonValue: TJSONValue;
  JsonArray: TJSONArray;
  Item: TJSONValue;
begin
  FSendFile.ListClientFiles.Clear;
  JsonValue := TJSONObject.ParseJSONValue(AJsonFiles);

  if Assigned(JsonValue) and (JsonValue is TJSONArray) then
  begin
    JsonArray := JsonValue as TJSONArray;

    // Itera sobre cada item do array JSON
    FSendFile.ListClientFiles.BeginUpdate;
    for Item in JsonArray do
    begin
      // Adiciona o valor de cada item (que é uma string) ao ListBox.
      // A propriedade AsString extrai o valor de um TJSONString.
      FSendFile.ListClientFiles.Items.Add(Item.Value);
    end;

    FSendFile.ListClientFiles.EndUpdate;

    // Libera a memória alocada para o objeto JSON
    JsonValue.Free;
  end
  else
  begin
    showMessage
      ('Erro ao parsear a string JSON ou a string não é um array válido.');
    if Assigned(JsonValue) then
      JsonValue.Free;
    // Garante que a memória seja liberada em caso de erro.
  end;

end;

procedure TFServidor.ServerLogHandler(const AMessage: string);
begin
  // Este método agora é chamado pelo Server.Core
  MemoLog.Lines.Add(Format('[%s] %s', [FormatDateTime('hh:nn:ss', Now),
    AMessage]));
  if MemoLog.Lines.Count > 1000 then
    MemoLog.Lines.Delete(0);
  MemoLog.GoToTextEnd;
end;

procedure TFServidor.ServerClientConnectedHandler(AContext: TIdContext;
  const AComputerName, AIP: string);
var
  ListItem: TListViewItem;
begin
  // Lógica para adicionar item na lista
  ListItem := ListViewClientes.Items.Add;
  ListItem.TagObject := AContext;
  ListItem.Objects.FindObjectT<TListItemText>('TxtPc').Text := AComputerName;
  ListItem.Objects.FindObjectT<TListItemText>('TxtIp').Text := AIP;
  ListItem.Objects.FindObjectT<TListItemText>('TxtData').Text :=
    FormatDateTime('dd/mm hh:nn', Now);
end;

procedure TFServidor.ServerClientDisconnectedHandler(AContext: TIdContext);
var
  i: Integer;
begin
  // Lógica para remover item da lista
  for i := ListViewClientes.Items.Count - 1 downto 0 do
  begin
    if TIdContext(ListViewClientes.Items[i].TagObject) = AContext then
    begin
      ListViewClientes.Items.Delete(i);
      Break;
    end;
  end;
end;

procedure TFServidor.BtnSelDirClick(Sender: TObject);
var
  SelectedDir: string;
begin
  if SelectDirectory('Selecione um diretório', '', SelectedDir) then
  begin
    EdtPath.Text := SelectedDir;
    FServer.SavePath := EdtPath.Text;
  end;
end;

procedure TFServidor.PopSendFileClick(Sender: TObject);
var
  SelectedContext: TIdContext;
begin
  if (ListViewClientes.Selected <> nil) then
  begin
    SelectedContext := TIdContext(ListViewClientes.Selected.TagObject);
    if (SelectedContext <> nil) and SelectedContext.Connection.Connected then
    begin
      FileUploadSelectedContext := SelectedContext;
      FileUploadSelectedContext.Connection.IOHandler.WriteLn
        (CMD_GET_LIST_FILES);
      FSendFile.Show;
    end;
  end;

end;

procedure TFServidor.MenuClearBanListClick(Sender: TObject);
begin
  // A UI agora interage com o core do servidor
  FServer.Security.ClearAllAttempts;
  ServerLogHandler('Lista de tentativas de login foi limpa.');
end;

procedure TFServidor.TClearLoginAttptsTimer(Sender: TObject);
begin
  // A UI comanda a limpeza, mas a lógica está na classe de segurança
  FServer.Security.ClearExpiredAttempts;
end;

procedure TFServidor.PopSendDisconnectClick(Sender: TObject);
var
  SelectedContext: TIdContext;
begin
  if (ListViewClientes.Selected <> nil) then
  begin
    SelectedContext := TIdContext(ListViewClientes.Selected.TagObject);
    if (SelectedContext <> nil) and SelectedContext.Connection.Connected then
    begin
      SelectedContext.Connection.IOHandler.WriteLn(CMD_BYE);
      // SelectedContext.Connection.Disconnect;
    end;
  end;
end;

procedure TFServidor.PopSendMessagesClick(Sender: TObject);
var
  SelectedContext: TIdContext;
begin
  if (ListViewClientes.Selected <> nil) then
  begin
    SelectedContext := TIdContext(ListViewClientes.Selected.TagObject);

    if SelectedContext.Connection.Connected then
    begin
      // Se a instância do FChat não existir, crie-a
      if not Assigned(FChat) then
        Application.CreateForm(TFChat, FChat);

      FChat.Setup(FServer, SelectedContext); // <-- PASSA AS INFORMAÇÕES
      FChat.Show;
    end
    else
    begin
      ListViewClientes.Items.Delete(ListViewClientes.Selected.Index);
    end;
  end;

end;

procedure TFServidor.PopSendPingClick(Sender: TObject);
var
  SelectedContext: TIdContext;
begin
  if (ListViewClientes.Selected <> nil) then
  begin
    SelectedContext := TIdContext(ListViewClientes.Selected.TagObject);
    if (SelectedContext <> nil) and SelectedContext.Connection.Connected then
    begin
      SelectedContext.Connection.IOHandler.WriteLn(CMD_PING);
    end;
  end;
end;

end.
