unit Server.ClientManager;

interface

uses
  System.SysUtils, System.Generics.Collections, IdContext, SyncObjs, Server.Types;

type
  TClientManager = class
  private
    FAuthenticatedClients: TDictionary<TIdContext, TAuthenticatedClient>;
    FCriticalSection: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AContext: TIdContext; const AComputerName: string);
    procedure Remove(AContext: TIdContext);
    function IsAuthenticated(AContext: TIdContext): Boolean;
    function GetClientInfo(AContext: TIdContext; out AClient: TAuthenticatedClient): Boolean;
    function GetCount: Integer;
    function GetAuthenticatedList: TArray<TAuthenticatedClient>;
  end;

implementation

{ TClientManager }

constructor TClientManager.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FAuthenticatedClients := TDictionary<TIdContext, TAuthenticatedClient>.Create;
end;

destructor TClientManager.Destroy;
begin
  FAuthenticatedClients.Free;
  FCriticalSection.Free;
  inherited Destroy;
end;

procedure TClientManager.Add(AContext: TIdContext; const AComputerName: string);
var
  Client: TAuthenticatedClient;
begin
  FCriticalSection.Enter;
  try
    Client.Context := AContext;
    Client.ComputerName := AComputerName;
    Client.IP := AContext.Binding.PeerIP;
    Client.ConnectedTime := Now;
    FAuthenticatedClients.AddOrSetValue(AContext, Client);
  finally
    FCriticalSection.Leave;
  end;
end;

function TClientManager.GetAuthenticatedList: TArray<TAuthenticatedClient>;
begin
  FCriticalSection.Enter;
  try
    Result := FAuthenticatedClients.Values.ToArray;
  finally
    FCriticalSection.Leave;
  end;
end;


function TClientManager.GetClientInfo(AContext: TIdContext; out AClient: TAuthenticatedClient): Boolean;
begin
  FCriticalSection.Enter;
  try
    Result := FAuthenticatedClients.TryGetValue(AContext, AClient);
  finally
    FCriticalSection.Leave;
  end;
end;

function TClientManager.GetCount: Integer;
begin
  FCriticalSection.Enter;
  try
    Result := FAuthenticatedClients.Count;
  finally
    FCriticalSection.Leave;
  end;
end;

function TClientManager.IsAuthenticated(AContext: TIdContext): Boolean;
begin
  FCriticalSection.Enter;
  try
    Result := FAuthenticatedClients.ContainsKey(AContext);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TClientManager.Remove(AContext: TIdContext);
begin
  FCriticalSection.Enter;
  try
    if FAuthenticatedClients.ContainsKey(AContext) then
      FAuthenticatedClients.Remove(AContext);
  finally
    FCriticalSection.Leave;
  end;
end;

end.
