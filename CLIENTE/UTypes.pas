unit UTypes;

interface

uses
  System.SysUtils, System.Classes, IdGlobal;

type
  // Eventos para notificar a UI
  TLogEvent = procedure(const AMessage: string) of object;
  TConnectionStateChangeEvent = procedure(AConnected: Boolean;
    const AReason: string) of object;
  TFilesListEvent = procedure(const AJsonFiles: string) of object;
  TChatMessageEvent = procedure(const AChatMessage: string) of object;
  TFileProgressEvent = procedure(const ATotal, ATransferred: Int64) of object;
  TGenericEvent = procedure of object;

  // Callbacks para threads
  TOnMessageProc = reference to procedure(const AMsg: string);
  TOnListFilesProc = reference to procedure(const AJsonFiles: string);
  TOnChatMessageProc = reference to procedure(const AMsg: string);
  TDisconnectEvent = reference to procedure(const Reason: string);

const

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
