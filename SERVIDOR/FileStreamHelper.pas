unit FileStreamHelper;

interface

uses
  System.SysUtils, System.Classes;

type
  TProgressStream = class(TStream)
  private
    FStream: TStream;
    FTotalSize: Int64;
    FOnProgress: TProc<Int64>;
  public
    constructor Create(AStream: TStream; ATotalSize: Int64; AOnProgress: TProc<Int64>);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;


implementation

constructor TProgressStream.Create(AStream: TStream; ATotalSize: Int64; AOnProgress: TProc<Int64>);
begin
  inherited Create;
  FStream := AStream;
  FTotalSize := ATotalSize;
  FOnProgress := AOnProgress;
end;

function TProgressStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FStream.Read(Buffer, Count);
  if Assigned(FOnProgress) then
    FOnProgress(FStream.Position);
end;

function TProgressStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FStream.Write(Buffer, Count);
  if Assigned(FOnProgress) then
    FOnProgress(FStream.Position);
end;

function TProgressStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset, Origin);
end;

end.

