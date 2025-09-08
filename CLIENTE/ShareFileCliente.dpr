program ShareFileCliente;

uses
  System.StartUpCopy,
  FMX.Forms,
  UPrincipal in 'UPrincipal.pas' {FPrincipal},
  UFunctions in 'UFunctions.pas',
  UChat in 'UChat.pas' {FChat},
  UClientThread in 'UClientThread.pas',
  UConfig in 'UConfig.pas' {FConfig},
  UClientManager in 'UClientManager.pas',
  UTypes in 'UTypes.pas',
  FileStreamHelper in 'FileStreamHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFPrincipal, FPrincipal);
  Application.CreateForm(TFChat, FChat);
  Application.CreateForm(TFConfig, FConfig);
  Application.Run;
end.
