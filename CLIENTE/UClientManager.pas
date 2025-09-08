unit UClientManager;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.JSON,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdGlobal,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL,
  IdIntercept, IdCompressionIntercept, TaurusTLS, UClientThread, UFunctions,
  System.Threading, System.IOUtils, IdGlobalProtocols, UTypes;

type
  TClientManager = class(TObject)
  private
    FClient: TIdTCPClient;
    FIOHandler: TTaurusTLSIOHandlerSocket;
    FCompression: TIdCompressionIntercept;
    FReceiverThread: TReceiveThread;
    FEnvioLock: TObject;
    FDisconnectOnce: Integer;
    FPass: string;
    FCurrentTransferSize: Int64;

    // Event handlers
    FOnLogMessage: TLogEvent;
    FOnConnectionStateChange: TConnectionStateChangeEvent;
    FOnFilesListReceived: TFilesListEvent;
    FOnChatMessageReceived: TChatMessageEvent;
    FOnFileUploadSuccess: TGenericEvent;
    FOnFileUploadError: TLogEvent;
    FOnFileProgress: TFileProgressEvent;

    // Configuration methods
    procedure ConfigureSSL;
    procedure ConfigureClientSettings;
    procedure SetupIOHandlerEvents;

    // Connection lifecycle methods
    procedure InitializeClientComponents;
    procedure PerformAuthentication;
    procedure StartReceiverThread;
    procedure CleanupReceiverThread;

    // SSL/TLS event handlers
    procedure IOHandlerWorkBegin(Sender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure IOHandlerWork(Sender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
    procedure IOHandlerWorkEnd(Sender: TObject; AWorkMode: TWorkMode);

    // Thread event handlers
    procedure HandleThreadDisconnect(const Reason: string);
    procedure HandleThreadLogMessage(const AMsg: string);
    procedure HandleThreadFilesList(const AJsonFiles: string);
    procedure HandleThreadChatMessage(const AMsg: string);

    // Utility methods
    procedure RaiseSSLInitializationError;
    procedure HandleConnectException(E: Exception);
    procedure ProcessAuthenticationResponse(const Response: string);
    procedure SendFileInternal(const ALocalFile: string);
    function ValidateFileSize(FileSize: Int64): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    // Public API
    procedure Connect(AHost: string; APort: Word; APass: string;
      AComputerName: string);
    procedure Disconnect(const AReason: string);
    function SendWriteLnCommand(const ACommand: string): Boolean;
    function SendWriteLnEndCommand(const ACommand: string;
      AMensagem: string): Boolean;
    procedure SendFile(const ALocalFile: string);
    function IsConnected: Boolean;

    // Properties
    property OnLogMessage: TLogEvent read FOnLogMessage write FOnLogMessage;
    property OnConnectionStateChange: TConnectionStateChangeEvent
      read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnFilesListReceived: TFilesListEvent read FOnFilesListReceived
      write FOnFilesListReceived;
    property OnChatMessageReceived: TChatMessageEvent
      read FOnChatMessageReceived write FOnChatMessageReceived;
    property OnFileUploadSuccess: TGenericEvent read FOnFileUploadSuccess
      write FOnFileUploadSuccess;
    property OnFileUploadError: TLogEvent read FOnFileUploadError
      write FOnFileUploadError;
    property OnFileProgress: TFileProgressEvent read FOnFileProgress
      write FOnFileProgress;
  end;

implementation

{ TClientManager }

constructor TClientManager.Create;
begin
  inherited;
  FEnvioLock := TObject.Create;
  FDisconnectOnce := 1;

  if not TaurusTLS.LoadOpenSSLLibrary then
    RaiseSSLInitializationError;

  InitializeClientComponents;
  ConfigureClientSettings;
  ConfigureSSL;
  SetupIOHandlerEvents;
end;

destructor TClientManager.Destroy;
begin
  Disconnect('Aplicação encerrada.');
  FreeAndNil(FEnvioLock);
  FreeAndNil(FClient);
  inherited;
end;

procedure TClientManager.InitializeClientComponents;
begin
  FClient := TIdTCPClient.Create(nil);
  FIOHandler := TTaurusTLSIOHandlerSocket.Create(FClient);
  FCompression := TIdCompressionIntercept.Create(FClient);
end;

procedure TClientManager.ConfigureClientSettings;
begin
  FClient.IOHandler := FIOHandler;
  FClient.Intercept := FCompression;
  FCompression.CompressionLevel := 5;
  FClient.ConnectTimeout := 10000;
  FClient.IOHandler.ReadTimeout := 15000;
  FClient.IOHandler.SendBufferSize := 32768;
  FClient.IOHandler.RecvBufferSize := 32768;
  FClient.IOHandler.LargeStream := True;
  FClient.ReuseSocket := rsTrue;
  FClient.UseNagle := False;
end;

procedure TClientManager.SetupIOHandlerEvents;
begin
  FIOHandler.OnWorkBegin := IOHandlerWorkBegin;
  FIOHandler.OnWork := IOHandlerWork;
  FIOHandler.OnWorkEnd := IOHandlerWorkEnd;
end;

procedure TClientManager.ConfigureSSL;
begin
  FIOHandler.PassThrough := False;
  FIOHandler.SSLOptions.Mode := sslmClient;
  FIOHandler.SSLOptions.CipherList := 'ECDHE-ECDSA-AES256-GCM-SHA384:' +
    'ECDHE-RSA-AES256-GCM-SHA384:' + 'ECDHE-ECDSA-CHACHA20-POLY1305:' +
    'ECDHE-RSA-CHACHA20-POLY1305:' + 'ECDHE-ECDSA-AES128-GCM-SHA256:' +
    'ECDHE-RSA-AES128-GCM-SHA256';
end;

procedure TClientManager.RaiseSSLInitializationError;
begin
  if Assigned(FOnLogMessage) then
    FOnLogMessage('❌ Não foi possível carregar as DLLs do TaurusSSL.');
  raise Exception.Create('Não foi possível carregar as DLLs do TaurusSSL.');
end;

procedure TClientManager.Connect(AHost: string; APort: Word; APass: string;
  AComputerName: string);
begin
  if IsConnected then
    Exit;

  Disconnect(''); // Garante limpeza antes de conectar

  FClient.Host := AHost;
  FClient.Port := APort;
  FPass := APass;

  try
    if Assigned(FOnLogMessage) then
      FOnLogMessage('🖥️ Conectando em ' + AHost + ':' + APort.ToString);

    FClient.Connect;
    PerformAuthentication;

  except
    on E: Exception do
      HandleConnectException(E);
  end;
end;

procedure TClientManager.HandleConnectException(E: Exception);
var
  LMsg: string;
begin
  LMsg := 'Erro ao conectar-se: ' + E.Message;

  if (Pos('SSL_read_ex failed', E.Message) > 0) and
    (Pos('EOF was observed', E.Message) > 0) then
    LMsg := 'Erro crítico. Verifique o servidor e reinicie a aplicação.';

  if Assigned(FOnConnectionStateChange) then
    FOnConnectionStateChange(False, LMsg);
end;

procedure TClientManager.PerformAuthentication;
var
  Cmd: string;
begin
  if not IsConnected then
    Exit;

  TInterlocked.Exchange(FDisconnectOnce, 0);

  if not FIOHandler.PassThrough then
  begin
    if Assigned(FOnLogMessage) then
    begin
      FOnLogMessage('✅ Conexão TLS segura estabelecida; ' + '🌐 Protocolo: ' +
        FIOHandler.SSLSocket.SSLProtocolVersionStr);
      FOnLogMessage('🔐 Iniciando autenticação');
    end;

    // Autenticação após TLS
    FClient.IOHandler.WriteLn(FPass + ':' + GetComputerName);
    Cmd := FClient.IOHandler.ReadLn(IndyTextEncoding_UTF8);

    ProcessAuthenticationResponse(Cmd);
  end
  else
  begin
    Disconnect('⚠️ Conexão estabelecida mas SEM TLS!');
  end;
end;

procedure TClientManager.ProcessAuthenticationResponse(const Response: string);
begin
  if (Response = CMD_USER_BANNED) then
  begin
    Disconnect('⚠ VOCÊ ESTÁ BANIDO NESSE SERVIDOR! TENTE NOVAMENTE MAIS TARDE');
  end
  else if (Response = CMD_LOGIN_ERROR) then
  begin
    Disconnect('⚠ SENHA INVÁLIDA, TENTE NOVAMENTE!');
  end
  else if (Response = CMD_LOGIN_SUCCESS) then
  begin
    if Assigned(FOnLogMessage) then
      FOnLogMessage('🟢 Conectado com sucesso!');

    if Assigned(FOnConnectionStateChange) then
      FOnConnectionStateChange(True, '');

    StartReceiverThread;
    SendWriteLnCommand(CMD_GET_LIST_FILES);
  end;
end;

procedure TClientManager.StartReceiverThread;
begin
  FReceiverThread := TReceiveThread.Create(FClient, HandleThreadLogMessage,
    HandleThreadFilesList, HandleThreadChatMessage);
  FReceiverThread.OnDisconnect := HandleThreadDisconnect;
  FReceiverThread.Start;
end;

procedure TClientManager.CleanupReceiverThread;
begin
  if Assigned(FReceiverThread) then
  begin
    FReceiverThread.Terminate;
    FReceiverThread.WaitFor;
    FreeAndNil(FReceiverThread);
  end;
end;

procedure TClientManager.Disconnect(const AReason: string);
begin
  if TInterlocked.Exchange(FDisconnectOnce, 1) = 0 then
  begin
    try
      CleanupReceiverThread;

      if IsConnected then
      begin
        FClient.IOHandler.WriteLn(CMD_BYE);
        FIOHandler.CloseGracefully;
        Sleep(100);
      end;
    finally
      if Assigned(FOnConnectionStateChange) then
        TThread.Queue(nil,
          procedure
          begin
            FOnConnectionStateChange(False, AReason);
          end);
    end;
  end;
end;

function TClientManager.IsConnected: Boolean;
begin
  Result := (FClient <> nil) and FClient.Connected;
end;

function TClientManager.SendWriteLnCommand(const ACommand: string): Boolean;
begin
  Result := False;
  if not IsConnected then
    Exit;

  try
    TMonitor.Enter(FEnvioLock);
    try
      FClient.IOHandler.WriteLn(ACommand, IndyTextEncoding_UTF8);
      Result := True;
    finally
      TMonitor.Exit(FEnvioLock);
    end;
  except
    on E: Exception do
    begin
      Disconnect('Erro ao enviar comando: ' + E.Message);
    end;
  end;
end;

function TClientManager.SendWriteLnEndCommand(const ACommand: string;
AMensagem: string): Boolean;
begin
  Result := False;
  if not IsConnected then
    Exit;

  try
    TMonitor.Enter(FEnvioLock);
    try
      FClient.IOHandler.WriteLn(ACommand, IndyTextEncoding_UTF8);
      FClient.IOHandler.WriteLn(AMensagem, IndyTextEncoding_UTF8);
      FClient.IOHandler.WriteLn(CMD_END, IndyTextEncoding_UTF8);
      Result := True;
    finally
      TMonitor.Exit(FEnvioLock);
    end;
  except
    on E: Exception do
    begin
      Disconnect('Erro ao enviar comando: ' + E.Message);
    end;
  end;
end;

procedure TClientManager.SendFile(const ALocalFile: string);
begin
  TTask.Run(
    procedure
    begin
      SendFileInternal(ALocalFile);
    end);
end;

procedure TClientManager.SendFileInternal(const ALocalFile: string);
var
  Buffer: TIdBytes;
begin

  try
    TMonitor.Enter(FEnvioLock);

    if not IsConnected then
      Exit;

    var
    FileSize := TFile.GetSize(ALocalFile);

    if not ValidateFileSize(FileSize) then
      Exit;

    // Envia o comando e informações do arquivo
    if not SendWriteLnCommand(CMD_SEND_FILE) then
      Exit;

    if not SendWriteLnCommand(ExtractFileName(ALocalFile)) then
      Exit;

    FClient.IOHandler.Write(FileSize);

    // Dispara evento de início de trabalho
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnFileProgress) then
          FOnFileProgress(FileSize, 0);
      end);

    var
    FileStream := TFileStream.Create(ALocalFile, fmOpenRead or
      fmShareDenyWrite);
    try
      var
        ChunkSize: Integer := 262144; // 256KB

      SetLength(Buffer, ChunkSize);
      var
        TotalSent: Int64 := 0;
      var
        BytesRead: Integer;

      while TotalSent < FileSize do
      begin
        BytesRead := FileStream.Read(Buffer[0], ChunkSize);
        if BytesRead <= 0 then
          Break;

        FClient.IOHandler.Write(Buffer, BytesRead);
        Inc(TotalSent, BytesRead);

        // Atualiza progresso
        TThread.Queue(nil,
          procedure
          begin
            if Assigned(FOnFileProgress) then
              FOnFileProgress(FileSize, TotalSent);
          end);
      end;

      TThread.Queue(nil,
        procedure
        begin
          if Assigned(FOnFileUploadSuccess) then
            FOnFileUploadSuccess();
          if Assigned(FOnLogMessage) then
            FOnLogMessage('✅ Arquivo enviado: ' + ExtractFileName(ALocalFile));
        end);

    except
      on E: Exception do
        TThread.Queue(nil,
          procedure
          begin
            if Assigned(FOnFileUploadError) then
              FOnFileUploadError('❌ Erro ao enviar ' + ALocalFile + ': ' +
                E.Message);
          end);
    end;
  finally
    TMonitor.Exit(FEnvioLock);
  end;

end;

function TClientManager.ValidateFileSize(FileSize: Int64): Boolean;
begin
  Result := FileSize <= FILE_SIZE_LIMIT;
  if not Result then
  begin
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnFileUploadError) then
          FOnFileUploadError('❌ Arquivo muito grande: ' + FileSize.ToString +
            ' bytes');
      end);
  end;
end;

// =============================================================
// Event Handlers para IOHandler (Progresso de Transferência)
// =============================================================

procedure TClientManager.IOHandlerWorkBegin(Sender: TObject;
AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  FCurrentTransferSize := AWorkCountMax;

  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnFileProgress) then
        FOnFileProgress(FCurrentTransferSize, 0);
    end);
end;

procedure TClientManager.IOHandlerWork(Sender: TObject; AWorkMode: TWorkMode;
AWorkCount: Int64);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnFileProgress) then
        FOnFileProgress(FCurrentTransferSize, AWorkCount);
    end);
end;

procedure TClientManager.IOHandlerWorkEnd(Sender: TObject;
AWorkMode: TWorkMode);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnFileProgress) then
        FOnFileProgress(FCurrentTransferSize, FCurrentTransferSize);

      DelayAsync(500,
        procedure
        begin
          if Assigned(FOnFileProgress) then
            FOnFileProgress(0, 0);
        end);
    end);

  FCurrentTransferSize := 0;
end;

// =============================================================
// Event Handlers para Thread de Recepção
// =============================================================

procedure TClientManager.HandleThreadDisconnect(const Reason: string);
begin
  Disconnect(Reason);
end;

procedure TClientManager.HandleThreadLogMessage(const AMsg: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnLogMessage) then
        FOnLogMessage('' + AMsg);
    end);
end;

procedure TClientManager.HandleThreadFilesList(const AJsonFiles: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnFilesListReceived) then
        FOnFilesListReceived(AJsonFiles);
    end);
end;

procedure TClientManager.HandleThreadChatMessage(const AMsg: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnChatMessageReceived) then
        FOnChatMessageReceived(AMsg);
    end);
end;

end.
