unit UChat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.Threading, FMX.Menus;

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
    procedure SendTextMessageClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FChat: TFChat;

implementation

Uses UPrincipal, UTypes;

{$R *.fmx}

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
begin

  if MMessage.Lines.Text.IsEmpty then
    exit;

  if FPrincipal.Client.SendWriteLnEndCommand(CMD_MESSAGE,
    MMessage.Lines.Text.trim) then
  begin
    MReceivedMessage.Lines.Add(FormatDateTime('[dd/mm/yy hh:nn]', Now) + ' ' +
      MMessage.Lines.Text);
    MMessage.Lines.Clear;
    MReceivedMessage.GoToTextEnd;
    MMessage.SetFocus;
  end;

end;

end.
