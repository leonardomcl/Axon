unit Server.ProtocolHandler;

interface

uses
  System.SysUtils, System.Classes, IdGlobal, IdContext, Server.ClientManager,
  System.IOUtils, Functions, Server.Types, Server.MessageManager,
  System.Generics.Collections;

type
  // Evento para notificar o core sobre o log
  TLogEvent = procedure(const AMessage: string) of object;
  TFileListEvent = procedure(const AJsonFiles: string) of object;
  TMessageReceivedEvent = procedure(AContext: TIdContext;

    const AComputerName, AMessage: string) of object;

  TProtocolHandler = class
  private
    FClientManager: TClientManager;
    FMessageManager: TMessageManager;
    FCommandHandlers: TDictionary<string, TCommandProc>;
    FOnMessageReceived: TMessageReceivedEvent;
    FOnLog: TLogEvent;
    FOnListFiles: TFileListEvent;

    // Handlers específicos para cada comando
    procedure HandleSendFile(AContext: TIdContext; const ASavePath: string);
    procedure HandleDownloadFile(AContext: TIdContext; const ASavePath: string);
    procedure HandlePing(AContext: TIdContext; const ASavePath: string);
    procedure HandlePong(AContext: TIdContext; const ASavePath: string);
    procedure HandleBye(AContext: TIdContext; const ASavePath: string);
    procedure HandleFileUploadSuccess(AContext: TIdContext;
      const ASavePath: string);
    procedure HandleGetListFiles(AContext: TIdContext; const ASavePath: string);
    procedure HandleFiles(AContext: TIdContext; const ASavePath: string);
    procedure HandleMessage(AContext: TIdContext; const ASavePath: string);

    procedure Log(const AMessage: string);
    procedure ListFilesReceived(const AJsonFiles: string);
  public
    procedure Handle(AContext: TIdContext; const ACommand: string;
      const ASavePath: string);
    constructor Create(AClientManager: TClientManager;
      AMessageManager: TMessageManager);
    destructor Destroy; override;
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnFileList: TFileListEvent read FOnListFiles write FOnListFiles;
    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived
      write FOnMessageReceived;
  end;

implementation

uses System.JSON, FileStreamHelper, USendFile;

{ TProtocolHandler }

constructor TProtocolHandler.Create(AClientManager: TClientManager;
  AMessageManager: TMessageManager);
begin
  inherited Create;
  FClientManager := AClientManager;
  FMessageManager := AMessageManager;

  FCommandHandlers := TDictionary<string, TCommandProc>.Create;

  // Mapeamento de comandos para handlers
  FCommandHandlers.Add(CMD_SEND_FILE, HandleSendFile);
  FCommandHandlers.Add(CMD_DOWNLOAD_FILE, HandleDownloadFile);
  FCommandHandlers.Add(CMD_PING, HandlePing);
  FCommandHandlers.Add(CMD_PONG, HandlePong);
  FCommandHandlers.Add(CMD_FILE_UPLOAD_SUCCESS, HandleFileUploadSuccess);
  FCommandHandlers.Add(CMD_GET_LIST_FILES, HandleGetListFiles);
  FCommandHandlers.Add(CMD_FILES, HandleFiles);
  FCommandHandlers.Add(CMD_BYE, HandleBye);
  FCommandHandlers.Add(CMD_MESSAGE, HandleMessage);
end;

destructor TProtocolHandler.Destroy;
begin
  FCommandHandlers.Free;
  inherited Destroy;
end;

procedure TProtocolHandler.Log(const AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AMessage);
end;

procedure TProtocolHandler.ListFilesReceived(const AJsonFiles: string);
begin
  if Assigned(FOnListFiles) then
    FOnListFiles(AJsonFiles);
end;

procedure TProtocolHandler.Handle(AContext: TIdContext; const ACommand: string;
  const ASavePath: string);
var
  Handler: TCommandProc;
  ClientInfo: TAuthenticatedClient;
begin
  if not AContext.Connection.Connected then
    Exit;

  // Verifica se o cliente está autenticado
  if not FClientManager.GetClientInfo(AContext, ClientInfo) then
  begin
    Log('🚫 Tentativa de comando de cliente não autenticado: ' +
      AContext.Binding.PeerIP);
    Exit;
  end;

  // Busca o handler para o comando
  if FCommandHandlers.TryGetValue(ACommand, Handler) then
  begin
    try
      Handler(AContext, ASavePath);
    except
      on E: Exception do
      begin
        Log('Erro ao executar comando ' + ACommand + ': ' + E.Message);
        // Opcional: enviar mensagem de erro para o cliente
        // AContext.Connection.IOHandler.WriteLn('#ERROR: ' + E.Message);
      end;
    end;
  end
  else
  begin
    Log('Comando desconhecido recebido: ' + ACommand);
    // AContext.Connection.IOHandler.WriteLn('#UNKNOWN_COMMAND');
  end;
end;

procedure TProtocolHandler.HandleFileUploadSuccess(AContext: TIdContext;
  const ASavePath: string);
begin
  //
end;

procedure TProtocolHandler.HandleSendFile(AContext: TIdContext;
  const ASavePath: string);
var
  FileName: string;
  FileSize: Int64;
  FileStream: TFileStream;
  ProgressStream: TProgressStream;
  ClientInfo: TAuthenticatedClient;
begin
  if not FClientManager.GetClientInfo(AContext, ClientInfo) then
    Exit;

  try
    FileName := AContext.Connection.IOHandler.ReadLn(IndyTextEncoding_UTF8);
    FileSize := AContext.Connection.IOHandler.ReadInt64;

    if FileSize > FILE_SIZE_LIMIT then
    begin
      AContext.Connection.IOHandler.WriteLn(CMD_ERROR_FILE_SIZE);
      Exit;
    end;

    ForceDirectories(ASavePath);
    FileStream := TFileStream.Create(TPath.Combine(ASavePath,
      ExtractFileName(FileName)), fmCreate);

    try
      // Configuração da progress bar (se aplicável)
      TThread.Synchronize(nil,
        procedure
        begin
          if Assigned(FSendFile) then
          begin
            FSendFile.ProgressFile.Max := FileSize;
            FSendFile.ProgressFile.Value := 0;
          end;
        end);

      ProgressStream := TProgressStream.Create(FileStream, FileSize,
        procedure(ABytesRead: Int64)
        begin
          TThread.Queue(nil,
            procedure
            begin
              if Assigned(FSendFile) then
                FSendFile.ProgressFile.Value := ABytesRead;
            end);
        end);

      try
        AContext.Connection.IOHandler.ReadStream(ProgressStream,
          FileSize, False);
        AContext.Connection.IOHandler.WriteLn(CMD_FILE_UPLOAD_SUCCESS);

        Log(Format('📁 Arquivo recebido de %s: %s (%d bytes)',
          [ClientInfo.ComputerName, FileName, FileSize]));

      finally
        ProgressStream.Free;
      end;

    finally
      FileStream.Free;
    end;

  except
    on E: Exception do
      Log('❗ Erro ao receber arquivo: ' + E.Message);
  end;
end;

procedure TProtocolHandler.HandleDownloadFile(AContext: TIdContext;
const ASavePath: string);
var
  FileName: string;
  FileSize: Int64;
  FileStream: TFileStream;
  Buffer: TIdBytes;
  BytesRead: Integer;
  TotalSent: Int64;
  ChunkSize: Integer;
  ClientInfo: TAuthenticatedClient;
begin
  if not FClientManager.GetClientInfo(AContext, ClientInfo) then
    Exit;

  FileName := AContext.Connection.IOHandler.ReadLn(IndyTextEncoding_UTF8);
  FileSize := TFile.GetSize(ASavePath + '\' + FileName);

  if FileSize > FILE_SIZE_LIMIT then
    raise Exception.Create('Arquivo muito grande');

  Log(Format('🔔 [%s] Solicitou download do arquivo %s (%d bytes)',
    [ClientInfo.ComputerName, FileName, FileSize]));

  FileStream := TFileStream.Create(ASavePath + '\' + FileName,
    fmOpenRead or fmShareDenyWrite);
  try
    AContext.Connection.IOHandler.WriteLn(CMD_SEND_FILE, IndyTextEncoding_UTF8);
    AContext.Connection.IOHandler.WriteLn(FileName, IndyTextEncoding_UTF8);
    AContext.Connection.IOHandler.Write(FileStream.Size);

    // Envia em chunks de 256KB
    ChunkSize := 262144;
    SetLength(Buffer, ChunkSize);
    TotalSent := 0;

    while TotalSent < FileStream.Size do
    begin
      BytesRead := FileStream.Read(Buffer[0], ChunkSize);
      if BytesRead <= 0 then
        Break;

      AContext.Connection.IOHandler.Write(Buffer, BytesRead);
      Inc(TotalSent, BytesRead);
    end;

    Log(Format('✔ [%s] Upload completo - %s (%d bytes)',
      [ClientInfo.ComputerName, FileName, FileSize]));

  finally
    FileStream.Free;
    SetLength(Buffer, 0);
  end;
end;

procedure TProtocolHandler.HandleMessage(AContext: TIdContext;
const ASavePath: string);
var
  MessageText, FormattedMsg: string;
  ClientInfo: TAuthenticatedClient;
begin
  if not FClientManager.GetClientInfo(AContext, ClientInfo) then
    Exit;

  try
    MessageText := AContext.Connection.IOHandler.ReadLn(CMD_END,
      IndyTextEncoding_UTF8).Trim;

    FormattedMsg := FormatDateTime('[dd/mm/yy hh:nn] ', Now) +
      Format('[%s] ', [ClientInfo.ComputerName]) + MessageText;

    FMessageManager.AddMessage(AContext, FormattedMsg);

    if Assigned(FOnMessageReceived) then
      FOnMessageReceived(AContext, ClientInfo.ComputerName, FormattedMsg);

    Log('[' + ClientInfo.ComputerName + ']' + ' 💬 Mensagem recebida ');
    AContext.Connection.IOHandler.WriteLn(CMD_CONFIRM_MESSAGE);


  except
    on E: Exception do
      Log('❗ Erro ao processar mensagem de ' + ClientInfo.ComputerName + ': ' +
        E.Message);
  end;
end;

procedure TProtocolHandler.HandleGetListFiles(AContext: TIdContext;
const ASavePath: string);
var
  JsonString: string;
begin
  try
    JsonString := ListarNomesDeArquivosParaJson(ASavePath);
    AContext.Connection.IOHandler.WriteLn(CMD_FILES, IndyTextEncoding_UTF8);
    AContext.Connection.IOHandler.WriteLn(JsonString, IndyTextEncoding_UTF8);
  except
    on E: Exception do
      AContext.Connection.IOHandler.WriteLn('[]', IndyTextEncoding_UTF8);
  end;
end;

procedure TProtocolHandler.HandleFiles(AContext: TIdContext;
const ASavePath: string);
var
  JsonString: string;
begin
  JsonString := AContext.Connection.IOHandler.ReadLn(IndyTextEncoding_UTF8);
  if Assigned(FOnListFiles) then
    ListFilesReceived(JsonString);
end;

procedure TProtocolHandler.HandlePing(AContext: TIdContext;
const ASavePath: string);
var
  ClientInfo: TAuthenticatedClient;
begin
  if FClientManager.GetClientInfo(AContext, ClientInfo) then
  begin
    Log(Format('[%s] ➡ Ping Recebido', [ClientInfo.ComputerName]));
    AContext.Connection.IOHandler.WriteLn('#PONG');
  end;
end;

procedure TProtocolHandler.HandlePong(AContext: TIdContext;
const ASavePath: string);
var
  ClientInfo: TAuthenticatedClient;
begin
  if FClientManager.GetClientInfo(AContext, ClientInfo) then
    Log(Format('[%s] 🔄 Pong recebido', [ClientInfo.ComputerName]));
end;

procedure TProtocolHandler.HandleBye(AContext: TIdContext;
const ASavePath: string);
begin
  AContext.Connection.Disconnect;
end;

end.
