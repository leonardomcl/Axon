program ShareFilesServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  UPrincipal in 'UPrincipal.pas' {FServidor},
  Functions in 'Functions.pas',
  UChat in 'UChat.pas' {FChat},
  USendFile in 'USendFile.pas' {FSendFile},
  Server.Types in 'Server.Types.pas',
  Server.ClientManager in 'Server.ClientManager.pas',
  Server.Security in 'Server.Security.pas',
  Server.ProtocolHandler in 'Server.ProtocolHandler.pas',
  Server.Core in 'Server.Core.pas',
  Server.MessageManager in 'Server.MessageManager.pas',
  FileStreamHelper in 'FileStreamHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFServidor, FServidor);
  Application.CreateForm(TFChat, FChat);
  Application.CreateForm(TFSendFile, FSendFile);
  Application.Run;
end.
