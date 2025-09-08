unit UChat;

interface

uses
  System.SysUtils, System.Types, System.Classes, FMX.Memo,
  FMX.StdCtrls, FMX.Controls, FMX.Layouts, IdContext, IdGlobal,
  Server.Core, FMX.Memo.Types, FMX.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit, FMX.Menus,
  FMX.Objects, System.UITypes;

type
  TFChat = class(TForm)
    MReceivedMessage: TMemo;
    GroupBox1: TGroupBox;
    MMessage: TMemo;
    SendTextMessage: TCornerButton;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure SendTextMessageClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
  private
    { Private declarations }
    FContext: TIdContext;
    FServerCore: TServerCore;
    procedure ServerMessageReceived(AContext: TIdContext;
      const AFormattedMessage: string); // <-- NOVO

  public
    { Public declarations }
    procedure Setup(AServerCore: TServerCore; AContext: TIdContext);
  end;

var
  FChat: TFChat;

implementation

uses System.DateUtils, Server.Types;

{$R *.fmx}

procedure TFChat.Setup(AServerCore: TServerCore; AContext: TIdContext);
begin
  FServerCore := AServerCore;
  FContext := AContext;
  // Inscreve este formulário para receber notificações de novas mensagens
  if Assigned(FServerCore) then
    FServerCore.OnUIMessageReceived := ServerMessageReceived;
end;

procedure TFChat.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FServerCore) then
    FServerCore.OnUIMessageReceived := nil;

  // Limpar referências
  FServerCore := nil;
  FContext := nil;

  Action := TCloseAction.caFree;
  FChat := nil;
end;

procedure TFChat.ServerMessageReceived(AContext: TIdContext;
  const AFormattedMessage: string);
begin
  // Verifica se a mensagem é para o cliente desta janela de chat
  if AContext = FContext then
  begin
    // Atualiza a UI em tempo real
    MReceivedMessage.Lines.Add(AFormattedMessage);
    MReceivedMessage.GoToTextEnd;
  end;
end;

procedure TFChat.FormShow(Sender: TObject);
var
  MessageList: TStringList;
begin
  MReceivedMessage.Lines.Clear;

  // Verifica se foi configurado corretamente
  if (FServerCore = nil) or (FContext = nil) then
    Exit;

  // Agora busca as mensagens através do MessageManager
  MessageList := FServerCore.Messages.GetMessages(FContext);
  try
    MReceivedMessage.Lines.AddStrings(MessageList);
  finally
    MessageList.Free;
  end;
end;

procedure TFChat.MenuItem1Click(Sender: TObject);
begin
  MReceivedMessage.Lines.Clear;
end;

procedure TFChat.MenuItem2Click(Sender: TObject);
begin
  MReceivedMessage.Font.Size := MReceivedMessage.Font.Size - 1;
end;

procedure TFChat.MenuItem3Click(Sender: TObject);
begin
  MReceivedMessage.Font.Size := MReceivedMessage.Font.Size + 1;
end;

procedure TFChat.SendTextMessageClick(Sender: TObject);
var
  MsgToSend, FormattedMsg: string;
begin
  // Verifica se foi configurado corretamente
  if (FServerCore = nil) or (FContext = nil) or (MMessage.Lines.Text.IsEmpty)
  then
    Exit;

  MsgToSend := MMessage.Lines.Text.Trim;

  // Envia a mensagem para o cliente
  FContext.Connection.IOHandler.WriteLn(CMD_MESSAGE, IndyTextEncoding_UTF8);
  FContext.Connection.IOHandler.WriteLn(MsgToSend, IndyTextEncoding_UTF8);
  FContext.Connection.IOHandler.WriteLn(CMD_END, IndyTextEncoding_UTF8);

  // Adiciona a mensagem localmente através do MessageManager
  FormattedMsg := FormatDateTime('[dd/mm/yy hh:nn]', Now) + ' ' +
    MsgToSend;
  FServerCore.Messages.AddMessage(FContext, FormattedMsg);

  // Atualiza a UI
  MReceivedMessage.Lines.Add(FormattedMsg);
  MMessage.Lines.Clear;
  MMessage.SetFocus;
end;

end.
