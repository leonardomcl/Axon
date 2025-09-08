unit UClientThread;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Threading,
  System.IOUtils,
  System.JSON, IdTCPClient, IdGlobal, IdException, IdExceptionCore, IdStack,
  IdIntercept, IdCompressionIntercept,
  IdSSLOpenSSL, FileStreamHelper, UTypes, UFunctions;

type
  TReceiveThread = class(TThread)
  private
    FClient: TIdTCPClient;
    FOnMessage: TOnMessageProc;
    FOnListFiles: TOnListFilesProc;
    FOnChatMessage: TOnChatMessageProc;
    FOnDisconnect: TDisconnectEvent;

    // Métodos de processamento de comandos
    procedure ProcessServerShutdown;
    procedure ProcessDisconnectedByServer;
    procedure ProcessPing;
    procedure ProcessPong;
    procedure ProcessMessage;
    procedure ProcessDownloadFile;
    procedure ProcessSendFile;
    procedure ProcessFileUploadSuccess;
    procedure ProcessGetListFiles;
    procedure ProcessFiles;

    // Métodos auxiliares
    function ReadCommand: string;
    procedure HandleFileTransfer(const AFileName: string; AFileSize: Int64);
    procedure UpdateProgressBar(ABytesRead: Int64);
    procedure ResetProgressBar;

    // Manipuladores de erros
    procedure HandleReadTimeout;
    procedure HandleConnectionClosed(const AReason: string);
    procedure HandleSocketError(E: Exception);
    procedure HandleSSLError(E: Exception);
    procedure HandleGenericError(E: Exception);
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TIdTCPClient; AOnMessage: TOnMessageProc;
      AOnListFiles: TOnListFilesProc; AOnChatMessages: TOnChatMessageProc);
    destructor Destroy; override;
    property OnDisconnect: TDisconnectEvent read FOnDisconnect
      write FOnDisconnect;
  end;

implementation

uses UPrincipal;

{ TReceiveThread }

constructor TReceiveThread.Create(AClient: TIdTCPClient;
  AOnMessage: TOnMessageProc; AOnListFiles: TOnListFilesProc;
  AOnChatMessages: TOnChatMessageProc);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FClient := AClient;
  FOnMessage := AOnMessage;
  FOnListFiles := AOnListFiles;
  FOnChatMessage := AOnChatMessages;
end;

destructor TReceiveThread.Destroy;
begin
  // Garante que a thread seja terminada corretamente
  if not Terminated then
    Terminate;
  inherited;
end;

procedure TReceiveThread.Execute;
var
  LCmd: string;
begin
  while not Terminated and FClient.Connected do
  begin
    try
      LCmd := ReadCommand;

      // Processa os comandos recebidos
      if LCmd = CMD_SERVER_SHUTDOWN then
        ProcessServerShutdown
      else if LCmd = CMD_BYE then
        ProcessDisconnectedByServer
      else if LCmd = CMD_PING then
        ProcessPing
      else if LCmd = CMD_PONG then
        ProcessPong
      else if LCmd = CMD_MESSAGE then
        ProcessMessage
      else if LCmd = CMD_DOWNLOAD_FILE then
        ProcessDownloadFile
      else if LCmd = CMD_SEND_FILE then
        ProcessSendFile
      else if LCmd = CMD_FILE_UPLOAD_SUCCESS then
        ProcessFileUploadSuccess
      else if LCmd = CMD_GET_LIST_FILES then
        ProcessGetListFiles
      else if LCmd = CMD_FILES then
        ProcessFiles;

    except
      on E: EIdReadTimeout do
      begin
        HandleReadTimeout
      end;

      on E: EIdConnClosedGracefully do
      begin
        HandleConnectionClosed('Conexão fechada pelo servidor.')
      end;

      on E: EIdSocketError do
      begin
        HandleSocketError(E)
      end;

      on E: EIdSSLProtocolReplyError do
      begin
        HandleSSLError(E)
      end;

      on E: Exception do
      begin
        HandleGenericError(E);
      end;
    end;
  end;
end;

function TReceiveThread.ReadCommand: string;
begin
  if not FClient.Connected or Terminated then
    Exit('');

  try
    Result := FClient.IOHandler.ReadLn(IndyTextEncoding_UTF8).ToUpper;
  except
    on E: Exception do
    begin
      if not(E is EIdConnClosedGracefully) then
        raise;
      Result := '';
    end;
  end;
end;

// =============================================================
// Processadores de comandos
// =============================================================

procedure TReceiveThread.ProcessDisconnectedByServer;
begin
  if Assigned(FOnDisconnect) then
    TThread.Queue(nil,
      procedure
      begin
        FOnDisconnect('🔴 Você foi desconectado pelo servidor.');
      end);
  Terminate;
end;

procedure TReceiveThread.ProcessServerShutdown;
begin
  if Assigned(FOnDisconnect) then
    TThread.Queue(nil,
      procedure
      begin
        FOnDisconnect('🔴 Servidor foi desligado');
      end);
  Terminate;
end;

procedure TReceiveThread.ProcessPing;
begin
  if FClient.Connected then
    FClient.IOHandler.WriteLn(CMD_PONG);
end;

procedure TReceiveThread.ProcessPong;
begin
  if Assigned(FOnMessage) then
    TThread.Queue(nil,
      procedure
      begin
        FOnMessage('🔄 Pong recebido');
      end);
end;

procedure TReceiveThread.ProcessMessage;
var
  LMsg: string;
begin
  if not FClient.Connected then
    Exit;

  LMsg := FClient.IOHandler.ReadLn(CMD_END, IndyTextEncoding_UTF8).Trim;

  if Assigned(FOnChatMessage) then
  begin
    if Assigned(FOnMessage) then
      TThread.Queue(nil,
        procedure
        begin
          FOnMessage('💬 Mensagem recebida');
        end);

    TThread.Queue(nil,
      procedure
      begin
        FOnChatMessage(FormatDateTime('[dd/mm/yy hh:nn] ', Now) +
          Format('[%s] ', ['SERVER']) + LMsg);
      end);
  end;
end;

procedure TReceiveThread.ProcessDownloadFile;
var
  LFileName: string;
begin
  if not FClient.Connected then
    Exit;

  LFileName := FClient.IOHandler.ReadLn(IndyTextEncoding_UTF8);

  if Assigned(FOnMessage) then
    TThread.Queue(nil,
      procedure
      begin
        FOnMessage('🔔 Servidor solicitou download do arquivo ' + LFileName);
      end);

  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FPrincipal) and Assigned(FPrincipal.FClient) then
        FPrincipal.FClient.SendFile(TPath.Combine(DiretorioSelecionado,
          ExtractFileName(LFileName)));
    end);
end;

procedure TReceiveThread.ProcessSendFile;
var
  LFileName: string;
  LFileSize: Int64;
begin
  if not FClient.Connected then
    Exit;

  LFileName := FClient.IOHandler.ReadLn(IndyTextEncoding_UTF8);
  LFileSize := FClient.IOHandler.ReadInt64;

  if LFileSize > FILE_SIZE_LIMIT then
  begin
    FClient.IOHandler.WriteLn(CMD_ERROR_FILE_SIZE);
    Exit;
  end;

  HandleFileTransfer(LFileName, LFileSize);
end;

procedure TReceiveThread.ProcessFileUploadSuccess;
begin
  if FClient.Connected then
    FClient.IOHandler.WriteLn(CMD_GET_LIST_FILES);
end;

procedure TReceiveThread.ProcessGetListFiles;
var
  LJsonString: string;
begin
  if not FClient.Connected then
    Exit;

  try
    LJsonString := ListarNomesDeArquivosParaJson(DiretorioSelecionado);
    FClient.IOHandler.WriteLn(CMD_FILES, IndyTextEncoding_UTF8);
    FClient.IOHandler.WriteLn(LJsonString, IndyTextEncoding_UTF8);
  except
    on E: Exception do
    begin
      if FClient.Connected then
        FClient.IOHandler.WriteLn('[]', IndyTextEncoding_UTF8);
    end;
  end;
end;

procedure TReceiveThread.ProcessFiles;
var
  LFiles: string;
begin
  if not FClient.Connected then
    Exit;

  LFiles := FClient.IOHandler.ReadLn(IndyTextEncoding_UTF8);

  if Assigned(FOnListFiles) then
    TThread.Queue(nil,
      procedure
      begin
        FOnListFiles(LFiles);
      end);
end;

// =============================================================
// Manipulação de transferência de arquivos
// =============================================================

procedure TReceiveThread.HandleFileTransfer(const AFileName: string;
AFileSize: Int64);
var
  LFileStream: TFileStream;
  LProgressStream: TProgressStream;
  LFullPath: string;
begin
  try
    ForceDirectories(DiretorioSelecionado);
    LFullPath := TPath.Combine(DiretorioSelecionado,
      ExtractFileName(AFileName));

    // Verifica se o diretório de destino existe
    if not TDirectory.Exists(ExtractFilePath(LFullPath)) then
      ForceDirectories(ExtractFilePath(LFullPath));

    LFileStream := TFileStream.Create(LFullPath, fmCreate or fmShareExclusive);

    try
      // Configura a barra de progresso
      TThread.Synchronize(nil,
        procedure
        begin
          if Assigned(FPrincipal) and Assigned(FPrincipal.ProgressBar1) then
          begin
            FPrincipal.ProgressBar1.Max := AFileSize;
            FPrincipal.ProgressBar1.Value := 0;
          end;
        end);

      // Cria stream com monitoramento de progresso
      LProgressStream := TProgressStream.Create(LFileStream, AFileSize,
        UpdateProgressBar);

      try
        // Lê o stream com monitoramento
        FClient.IOHandler.ReadStream(LProgressStream, AFileSize, False);

        if FClient.Connected then
          FClient.IOHandler.WriteLn(CMD_FILE_UPLOAD_SUCCESS);

        if Assigned(FOnMessage) then
          TThread.Queue(nil,
            procedure
            begin
              FOnMessage(Format('📁 Arquivo recebido de %s: %s (%d bytes)',
                ['SERVIDOR', AFileName, AFileSize]));
            end);

      finally
        LProgressStream.Free;
      end;

      // Reseta a barra de progresso após conclusão
      ResetProgressBar;

    finally
      LFileStream.Free;
    end;

  except
    on E: Exception do
    begin
      if Assigned(FOnMessage) then
        TThread.Queue(nil,
          procedure
          begin
            FOnMessage('❗ Erro ao receber arquivo: ' + E.Message);
          end);
    end;
  end;
end;

procedure TReceiveThread.UpdateProgressBar(ABytesRead: Int64);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FPrincipal) and Assigned(FPrincipal.ProgressBar1) then
        FPrincipal.ProgressBar1.Value := ABytesRead;
    end);
end;

procedure TReceiveThread.ResetProgressBar;
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FPrincipal) and Assigned(FPrincipal.ProgressBar1) then
      begin
        FPrincipal.ProgressBar1.Max := 100;
        FPrincipal.ProgressBar1.Value := 0;
      end;
    end);
end;

// =============================================================
// Manipuladores de erros
// =============================================================

procedure TReceiveThread.HandleReadTimeout;
begin
  try
    // Tenta enviar um ping para verificar se a conexão ainda está ativa
    if FClient.Connected then
      FClient.IOHandler.WriteLn(CMD_PING);
  except
    // Se falhar ao enviar o PING, a conexão caiu
    if Assigned(FOnDisconnect) then
      TThread.Queue(nil,
        procedure
        begin
          FOnDisconnect('Timeout: Servidor não responde.');
        end);
    Terminate;
  end;
end;

procedure TReceiveThread.HandleConnectionClosed(const AReason: string);
begin
  if Assigned(FOnDisconnect) then
    TThread.Queue(nil,
      procedure
      begin
        FOnDisconnect(AReason);
      end);
  Terminate;
end;

procedure TReceiveThread.HandleSocketError(E: Exception);
begin
  if Assigned(FOnDisconnect) then
    TThread.Queue(nil,
      procedure
      begin
        FOnDisconnect('Erro de socket: ' + E.Message);
      end);
  Terminate;
end;

procedure TReceiveThread.HandleSSLError(E: Exception);
begin
  if Assigned(FOnDisconnect) then
    TThread.Queue(nil,
      procedure
      begin
        FOnDisconnect('Erro SSL: ' + E.Message);
      end);
  Terminate;
end;

procedure TReceiveThread.HandleGenericError(E: Exception);
var
  LErrorMessage: string;
begin
  if (Pos('SSL_read_ex failed', E.Message) > 0) and
    (Pos('EOF was observed', E.Message) > 0) then
  begin
    LErrorMessage := '⚠️ Servidor caiu/encerrou sem close_notify. ' + E.Message;
  end
  else
  begin
    LErrorMessage := 'Erro Crítico: ' + E.Message;
  end;

  if Assigned(FOnDisconnect) then
    TThread.Queue(nil,
      procedure
      begin
        FOnDisconnect(LErrorMessage);
      end);
  Terminate;
end;

end.
