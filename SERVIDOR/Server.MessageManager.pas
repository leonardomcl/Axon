unit Server.MessageManager;

interface

uses
  System.SysUtils, System.Generics.Collections, IdContext, SyncObjs,
  System.Classes;

type
  TMessageManager = class
  private
    FMessages: TDictionary<TIdContext, TStringList>;
    FCriticalSection: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterClient(AContext: TIdContext);
    procedure UnregisterClient(AContext: TIdContext);
    procedure AddMessage(AContext: TIdContext; const AMessage: string);
    function GetMessages(AContext: TIdContext): TStringList;
  end;

implementation

{ TMessageManager }

constructor TMessageManager.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FMessages := TDictionary<TIdContext, TStringList>.Create;
end;

destructor TMessageManager.Destroy;
begin
  FCriticalSection.Enter;
  try
    for var List in FMessages.Values do
      List.Free;
    FMessages.Free;
  finally
    FCriticalSection.Leave;
    FCriticalSection.Free;
  end;

  inherited Destroy;
end;

procedure TMessageManager.RegisterClient(AContext: TIdContext);
begin
  FCriticalSection.Enter;
  try
    if not FMessages.ContainsKey(AContext) then
      FMessages.Add(AContext, TStringList.Create);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TMessageManager.UnregisterClient(AContext: TIdContext);
var
  MessageList: TStringList;
begin
  FCriticalSection.Enter;
  try
    if FMessages.TryGetValue(AContext, MessageList) then
    begin
      FMessages.Remove(AContext);
      MessageList.Free;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TMessageManager.AddMessage(AContext: TIdContext;
  const AMessage: string);
var
  MessageList: TStringList;
begin
  FCriticalSection.Enter;
  try
    if FMessages.TryGetValue(AContext, MessageList) then
    begin
      MessageList.Add(AMessage);
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

function TMessageManager.GetMessages(AContext: TIdContext): TStringList;
begin
  Result := TStringList.Create;
  FCriticalSection.Enter;
  try
    if FMessages.ContainsKey(AContext) then
    begin
      Result.AddStrings(FMessages[AContext]);
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

end.
