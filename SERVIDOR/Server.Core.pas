unit Server.Core;

interface

uses
  System.SysUtils, System.Classes, IdBaseComponent, IdComponent,
  IdCustomTCPServer,
  IdTCPServer, IdContext, Server.ClientManager, Server.Security,
  Server.ProtocolHandler, Server.MessageManager, IdSSLOpenSSL, IdSSL, TaurusTLS,
  IdIntercept, IdCompressionIntercept, IdSchedulerOfThreadPool;

type
  TLogEvent = procedure(const AMessage: string) of object;
  TFileListEvent = procedure(const AJsonFiles: string) of object;
  TClientEvent = procedure(AContext: TIdContext;
    const AComputerName, AIP: string) of object;
  TClientDisconnectEvent = procedure(AContext: TIdContext) of object;
  TUIMessageReceivedEvent = procedure(AContext: TIdContext;
    const AFormattedMessage: string) of object;

  TServerCore = class(TComponent)
  private
    FIdTCPServer: TIdTCPServer;
    FIOHandler: TTaurusTLSServerIOHandler;
    FCompressionIntercept: TIdServerCompressionIntercept;
    FScheduler: TIdSchedulerOfThreadPool;
    FClientManager: TClientManager;
    FSecurityManager: TSecurityManager;
    FProtocolHandler: TProtocolHandler;
    FMessageManager: TMessageManager;
    FPort: Word;
    FPassword: string;
    FSavePath: string;
    FActive: Boolean;

    // Eventos para UI
    FOnLog: TLogEvent;
    FOnListFiles: TFileListEvent;
    FOnClientConnected: TClientEvent;
    FOnClientDisconnected: TClientDisconnectEvent;
    FOnUIMessageReceived: TUIMessageReceivedEvent;

    procedure DoConnect(AContext: TIdContext);
    procedure DoDisconnect(AContext: TIdContext);
    procedure DoExecute(AContext: TIdContext);
    procedure Log(const AMessage: string);
    procedure ListFilesReceived(const AJsonFiles: string);
    procedure DoMessageReceived(AContext: TIdContext;
      const AComputerName, AMessage: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Active: Boolean read FActive;
    property Port: Word read FPort write FPort;
    property Password: string read FPassword write FPassword;
    property SavePath: string read FSavePath write FSavePath;

    // Propriedades de Eventos
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnFileList: TFileListEvent read FOnListFiles write FOnListFiles;
    property OnClientConnected: TClientEvent read FOnClientConnected
      write FOnClientConnected;
    property OnClientDisconnected: TClientDisconnectEvent
      read FOnClientDisconnected write FOnClientDisconnected;

    property OnUIMessageReceived: TUIMessageReceivedEvent
      read FOnUIMessageReceived write FOnUIMessageReceived;

    // Acesso aos managers para funcionalidades da UI
    property Security: TSecurityManager read FSecurityManager;
    property Clients: TClientManager read FClientManager;
    property Messages: TMessageManager read FMessageManager;

  end;

implementation

uses
  IdGlobal, IdException, Server.Types, System.IOUtils, Functions;

{ TServerCore }

constructor TServerCore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClientManager := TClientManager.Create;
  FSecurityManager := TSecurityManager.Create;
  FMessageManager := TMessageManager.Create;
  FProtocolHandler := TProtocolHandler.Create(FClientManager, FMessageManager);
  // <-- ALTERE AQUI
  FProtocolHandler.OnLog := Log; // Conecta o log do handler ao log do core
  FProtocolHandler.OnFileList := ListFilesReceived;
  FProtocolHandler.OnMessageReceived := DoMessageReceived;

  // Configuração do Indy Server
  FIdTCPServer := TIdTCPServer.Create(Self);
  FIdTCPServer.OnConnect := DoConnect;
  FIdTCPServer.OnDisconnect := DoDisconnect;
  FIdTCPServer.OnExecute := DoExecute;

  FIdTCPServer.ReuseSocket := rsTrue;
  FIdTCPServer.MaxConnections := 10;
  FIdTCPServer.TerminateWaitTime := 5000;
  FIdTCPServer.UseNagle := false;

  // --- INÍCIO DA CONFIGURAÇÃO DA COMPRESSÃO ---
  FCompressionIntercept := TIdServerCompressionIntercept.Create(FIdTCPServer);
  FCompressionIntercept.CompressionLevel := 5;
  FIdTCPServer.Intercept := FCompressionIntercept;
  // --- FIM DA CONFIGURAÇÃO DA COMPRESSÃO ---

  // --- INÍCIO DA CONFIGURAÇÃO SCHEDULER ---
  FScheduler := TIdSchedulerOfThreadPool.Create(FIdTCPServer);
  FScheduler.PoolSize := CPUCount * 2;
  FScheduler.MaxThreads := CPUCount * 4;
  FScheduler.ThreadPriority := tpNormal;
  FIdTCPServer.Scheduler := FScheduler;
  // --- FIM DA CONFIGURAÇÃO DA SCHEDULER ---

  FIOHandler := TTaurusTLSServerIOHandler.Create(FIdTCPServer);
  FIOHandler.SSLOptions.Mode := sslmServer;
  // FIOHandler.SSLOptions.VerifyMode := [sslvrfPeer, ssl];
  FIOHandler.SSLOptions.VerifyDepth := 2;
  FIOHandler.SSLOptions.CipherList := 'ECDHE-ECDSA-AES256-GCM-SHA384:' +
    'ECDHE-RSA-AES256-GCM-SHA384:' + 'DHE-RSA-AES256-GCM-SHA384:' +
    'ECDHE-ECDSA-CHACHA20-POLY1305:' + 'ECDHE-RSA-CHACHA20-POLY1305';

  FIdTCPServer.IOHandler := FIOHandler;

end;

procedure TServerCore.DoMessageReceived(AContext: TIdContext;
  const AComputerName, AMessage: string);
begin
  // Retransmita o evento para a camada de UI
  if Assigned(FOnUIMessageReceived) then
    TThread.Queue(nil,
      procedure
      begin
        FOnUIMessageReceived(AContext, AMessage);
      end);
end;

destructor TServerCore.Destroy;
begin
  Stop;
  // Esperar desconexão completa
  Sleep(1000);
  FreeAndNil(FProtocolHandler);
  FreeAndNil(FSecurityManager);
  FreeAndNil(FMessageManager);
  FreeAndNil(FClientManager);
  // Componentes Indy serão destruídos pelo Owner (Self)
  inherited Destroy;
end;

procedure TServerCore.Log(const AMessage: string);
begin
  if Assigned(FOnLog) then
    TThread.Queue(nil,
      procedure
      begin
        FOnLog(AMessage);
      end);
end;

procedure TServerCore.ListFilesReceived(const AJsonFiles: string);
begin
  if Assigned(FOnListFiles) then
    TThread.Queue(nil,
      procedure
      begin
        FOnListFiles(AJsonFiles);
      end);
end;

procedure TServerCore.Start;
begin
  if FActive then
    Exit;
  try
    if not FileExists('server.crt') or not FileExists('server.key') then
    begin
      Log('⚠ Erro: Arquivos de certificado (server.crt, server.key) não encontrados.');
      Exit;
    end;

    FIOHandler.DefaultCert.PublicKey := 'server.crt';
    FIOHandler.DefaultCert.PrivateKey := 'server.key';
    //FIOHandler.DefaultCert.RootKey := 'root.pem'; // Opcional, mas recomendado

    TaurusTLS.LoadOpenSSLLibrary;
    FIdTCPServer.DefaultPort := FPort;
    FIdTCPServer.Active := True;
    FActive := True;
    Log('🖥️ Servidor iniciado na porta ' + FPort.ToString);
  except
    on E: Exception do
    begin
      FActive := false;
      Log('⚠ Erro ao iniciar servidor: ' + E.Message);
    end;
  end;
end;

procedure TServerCore.Stop;
var
  i: Integer;
  Contexts: TList;
begin
  if not FActive then
    Exit;

  if FIdTCPServer.Active then
  begin
    Contexts := FIdTCPServer.Contexts.LockList;
    try
      for i := 0 to Contexts.Count - 1 do
      begin
        try
          TIdContext(Contexts[i]).Connection.IOHandler.WriteLn
            (CMD_SERVER_SHUTDOWN, IndyTextEncoding_UTF8);
          TIdContext(Contexts[i]).Connection.Disconnect;
        except
          // Ignora erros ao desconectar clientes já desconectados
        end;
      end;
    finally
      FIdTCPServer.Contexts.UnlockList;
    end;
    Sleep(500); // Dá um tempo para as mensagens serem enviadas
    FIdTCPServer.Active := false;
  end;
  FActive := false;
  Log('❗Servidor parado');
end;

procedure TServerCore.DoConnect(AContext: TIdContext);
var
  ClientIP, ReceivedData, Password, ComputerName: string;
  SeparatorPos: Integer;
  LIOHandler: TIdSSLIOHandlerSocketBase;
begin
  ClientIP := AContext.Binding.PeerIP;

  LIOHandler := AContext.Connection.IOHandler as TIdSSLIOHandlerSocketBase;

  If AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase then
    TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler)
      .PassThrough := false;

  if LIOHandler.PassThrough then
  begin
    // Conexão não está usando SSL/TLS
    AContext.Connection.IOHandler.WriteLn(CMD_SSL_ERROR, IndyTextEncoding_UTF8);
    DelayAsync(100,
      procedure
      begin
        AContext.Connection.Disconnect;
      end);
    Exit;
  end;

  if FSecurityManager.IsIPBlocked(ClientIP) then
  begin
    AContext.Connection.IOHandler.WriteLn(CMD_USER_BANNED,
      IndyTextEncoding_UTF8);
    DelayAsync(100,
      procedure
      begin
        AContext.Connection.Disconnect;
      end);

    Log(Format('🚨 Conexão bloqueada de %s - IP na lista negra', [ClientIP]));
    Exit;
  end;

  try
    ReceivedData := AContext.Connection.IOHandler.ReadLn(IndyTextEncoding_UTF8);
  except
    // Cliente desconectou antes de enviar dados
    AContext.Connection.Disconnect;
    Exit;
  end;

  SeparatorPos := Pos(':', ReceivedData);
  if SeparatorPos = 0 then
  begin
    AContext.Connection.IOHandler.WriteLn(CMD_LOGIN_ERROR,
      IndyTextEncoding_UTF8);
    FSecurityManager.RegisterFailedAttempt(ClientIP);
    Log(Format('❗ Tentativa falha de %s (Formato incorreto)', [ClientIP]));
    DelayAsync(100,
      procedure
      begin
        AContext.Connection.Disconnect;
      end);
    Exit;
  end;

  Password := Copy(ReceivedData, 1, SeparatorPos - 1).Trim;
  ComputerName := Copy(ReceivedData, SeparatorPos + 1,
    Length(ReceivedData)).Trim;

  if Password <> FPassword then
  begin
    AContext.Connection.IOHandler.WriteLn(CMD_LOGIN_ERROR,
      IndyTextEncoding_UTF8);
    FSecurityManager.RegisterFailedAttempt(ClientIP);
    Log(Format('❗ Tentativa falha de %s (Senha incorreta)', [ClientIP]));
    DelayAsync(100,
      procedure
      begin
        AContext.Connection.Disconnect;
      end);
    Exit;
  end;

  // Autenticação bem-sucedida
  FClientManager.Add(AContext, ComputerName);
  FMessageManager.RegisterClient(AContext);
  AContext.Connection.IOHandler.WriteLn(CMD_LOGIN_SUCCESS,
    IndyTextEncoding_UTF8);
  AContext.Connection.IOHandler.RecvBufferSize := 32768;
  AContext.Connection.IOHandler.SendBufferSize := 32768;
  AContext.Connection.IOHandler.LargeStream := True;
  Log(Format('🟢 Cliente conectado: %s (%s)', [ComputerName, ClientIP]));

  if Assigned(FOnClientConnected) then
    TThread.Queue(nil,
      procedure
      begin
        FOnClientConnected(AContext, ComputerName, ClientIP);
      end);
end;

procedure TServerCore.DoDisconnect(AContext: TIdContext);
var
  ClientInfo: TAuthenticatedClient;
begin
  if FClientManager.GetClientInfo(AContext, ClientInfo) then
  begin
    Log(Format('🔴 Cliente desconectado: %s (%s)', [ClientInfo.ComputerName,
      ClientInfo.IP]));
  end;

  FClientManager.Remove(AContext);
  FMessageManager.UnregisterClient(AContext);

  if Assigned(FOnClientDisconnected) then
    TThread.Queue(nil,
      procedure
      begin
        FOnClientDisconnected(AContext);
      end);
end;

procedure TServerCore.DoExecute(AContext: TIdContext);
var
  ReceivedData: string;
begin
  try
    if AContext.Connection.IOHandler.InputBufferIsEmpty then
    begin
      AContext.Connection.IOHandler.CheckForDataOnSource(100);
      if AContext.Connection.IOHandler.InputBufferIsEmpty then
        Exit;
    end;

    ReceivedData := AContext.Connection.IOHandler.ReadLn
      (IndyTextEncoding_UTF8).Trim;
    if ReceivedData <> '' then
    begin
      FProtocolHandler.Handle(AContext, ReceivedData, FSavePath);
    end;
  except
    on E: EIdConnClosedGracefully do; // Conexão fechada normalmente
    on E: Exception do
    begin
      Log(Format('Erro com %s: %s', [AContext.Binding.PeerIP, E.Message]));
      AContext.Connection.Disconnect;
    end;
  end;
end;

end.
