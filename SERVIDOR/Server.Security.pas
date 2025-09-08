unit Server.Security;

interface

uses
  System.SysUtils, System.Generics.Collections, System.DateUtils, SyncObjs, Server.Types;

type
  TSecurityManager = class
  private
    FFailedAttempts: TDictionary<string, TFailedAttempt>;
    FCriticalSection: TCriticalSection;
    FBlockDurationMinutes: Integer; // Duração do bloqueio em minutos
    FMaxAttempts: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterFailedAttempt(const AIP: string);
    function IsIPBlocked(const AIP: string): Boolean;
    procedure ClearExpiredAttempts;
    procedure ClearAllAttempts;

    property BlockDurationMinutes: Integer read FBlockDurationMinutes write FBlockDurationMinutes;
    property MaxAttempts: Integer read FMaxAttempts write FMaxAttempts;
  end;

implementation

{ TSecurityManager }

constructor TSecurityManager.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FFailedAttempts := TDictionary<string, TFailedAttempt>.Create;
  FBlockDurationMinutes := BLOCK_DURATION_MINUTES; // Padrão
  FMaxAttempts := MAX_LOGIN_ATTEMPS; // Padrão
end;

destructor TSecurityManager.Destroy;
begin
  FFailedAttempts.Free;
  FCriticalSection.Free;
  inherited Destroy;
end;

procedure TSecurityManager.ClearAllAttempts;
begin
  FCriticalSection.Enter;
  try
    FFailedAttempts.Clear;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSecurityManager.ClearExpiredAttempts;
var
  Pair: TPair<string, TFailedAttempt>;
  ExpiredIPs: TList<string>;
begin
  ExpiredIPs := TList<string>.Create;
  try
    FCriticalSection.Enter;
    try
      for Pair in FFailedAttempts do
      begin
        if MinutesBetween(Now, Pair.Value.LastAttempt) >= FBlockDurationMinutes then
          ExpiredIPs.Add(Pair.Key);
      end;

      for var IP in ExpiredIPs do
        FFailedAttempts.Remove(IP);
    finally
      FCriticalSection.Leave;
    end;
  finally
    ExpiredIPs.Free;
  end;
end;

function TSecurityManager.IsIPBlocked(const AIP: string): Boolean;
var
  Attempt: TFailedAttempt;
begin
  Result := False;
  FCriticalSection.Enter;
  try
    if FFailedAttempts.TryGetValue(AIP, Attempt) then
    begin
      if (Attempt.Attempts >= FMaxAttempts) and
         (MinutesBetween(Now, Attempt.LastAttempt) < FBlockDurationMinutes) then
      begin
        Result := True;
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSecurityManager.RegisterFailedAttempt(const AIP: string);
var
  Attempt: TFailedAttempt;
begin
  FCriticalSection.Enter;
  try
    if FFailedAttempts.TryGetValue(AIP, Attempt) then
    begin
      Inc(Attempt.Attempts);
      Attempt.LastAttempt := Now;
      FFailedAttempts.Items[AIP] := Attempt;
    end
    else
    begin
      Attempt.IP := AIP;
      Attempt.Attempts := 1;
      Attempt.LastAttempt := Now;
      FFailedAttempts.Add(AIP, Attempt);
    end;
    // O log será feito pela classe Server.Core que usa esta
  finally
    FCriticalSection.Leave;
  end;
end;

end.
