unit Server.Types;

interface

uses
  System.SysUtils, IdContext;

type
  TCommandProc = reference to procedure(AContext: TIdContext;
    const ASavePath: string);

  TFailedAttempt = record
    IP: string;
    Attempts: Integer;
    LastAttempt: TDateTime;
  end;

  TAuthenticatedClient = record
    Context: TIdContext;
    ComputerName: string;
    IP: string;
    ConnectedTime: TDateTime;
  end;

const

  // Config
  MAX_LOGIN_ATTEMPS = 2;
  BLOCK_DURATION_MINUTES = 30;
  FILE_SIZE_LIMIT = 1000 * 1024 * 1024; // 1GIGA

  // Comandos do protocolo
  CMD_LOGIN_ERROR = '#LOGIN_ERROR';
  CMD_LOGIN_SUCCESS = '#AUTH_SUCCESS';
  CMD_USER_BANNED = '#USER_BANNED';
  CMD_SSL_ERROR = '#SSL_ERROR';
  CMD_MESSAGE = '#MESSAGE';
  CMD_CONFIRM_MESSAGE = '#MESSAGE_RECEIVED';
  CMD_PING = '#PING';
  CMD_PONG = '#PONG';
  CMD_GET_LIST_FILES = '#GET_LIST_FILES';
  CMD_DOWNLOAD_FILE = '#DOWNLOAD';
  CMD_FILES = '#FILES';
  CMD_FILE_UPLOAD_SUCCESS = '#FILE_UPLOAD_SUCCESS';
  CMD_SEND_FILE = '#SEND_FILE';
  CMD_ERROR_FILE_SIZE = '#ERROR_FILE_SIZE';
  CMD_BYE = '#BYE';
  CMD_END = '|END|';
  CMD_SERVER_SHUTDOWN = '#SERVER_SHUTDOWN';

implementation

end.
