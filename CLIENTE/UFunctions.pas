unit UFunctions;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, Winapi.IpHlpApi, Winapi.IpTypes, System.Generics.Collections,
  Winapi.Windows, IdStack, System.Threading, System.IOUtils, System.jSON;

function GetComputerName: string;
function GetWirelessIP: string;
function DirectoryIsWritable(const ADir: string): Boolean;
function ListarNomesDeArquivosParaJson(const ACaminhoDiretorio: string): string;
procedure DelayAsync(Milliseconds: Integer; ACallback: TProc);

implementation

// ===================
// Funções auxiliares
// ===================


function ListarNomesDeArquivosParaJson(const ACaminhoDiretorio: string): string;
var
  LJsonArray: TJSONArray;
  LArquivo: string;
begin
  LJsonArray := TJSONArray.Create;
  try
    // Retorna um array vazio '[]' se o diretório não existir
    if not TDirectory.Exists(ACaminhoDiretorio) then
    begin
      Result := LJsonArray.ToString;
      Exit;
    end;

    // TDirectory.GetFiles retorna uma lista com o caminho completo dos arquivos
    // no diretório especificado, sem incluir subpastas.
    for LArquivo in TDirectory.GetFiles(ACaminhoDiretorio) do
    begin
      // TPath.GetFileName extrai apenas o nome e a extensão do arquivo
      // e adicionamos diretamente ao array JSON.
      LJsonArray.Add(TPath.GetFileName(LArquivo));
    end;

    // Formata o JSON com indentação de 2 espaços para melhor leitura
    Result := LJsonArray.ToString;
  finally
    LJsonArray.Free;
  end;
end;

procedure DelayAsync(Milliseconds: Integer; ACallback: TProc);
begin
  TTask.Run(
    procedure
    begin
      Sleep(Milliseconds); // roda em thread separada
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(ACallback) then
            ACallback();
        end);
    end);
end;

function DirectoryIsWritable(const ADir: string): Boolean;
var
  TempFile: string;
begin
  TempFile := TPath.Combine(ADir, 'temp_test.tmp');
  try
    TFile.WriteAllText(TempFile, 'test');
    Result := True;
    TFile.Delete(TempFile);
  except
    Result := False;
  end;
end;

function GetComputerName: string;
begin
  Result := GetEnvironmentVariable('COMPUTERNAME');
end;

function StrPasA(const P: PAnsiChar): string;
begin
  if P = nil then
    Result := ''
  else
    Result := string(AnsiString(P));
end;

function GetWirelessIP: string;
var
  pAdapterInfo, pAdapter: PIP_ADAPTER_INFO;
  BufLen: ULONG;
  RetVal: DWORD;
  Desc: string;
  pIpAddr: PIP_ADDR_STRING;
begin
  Result := '';
  BufLen := 0;
  RetVal := GetAdaptersInfo(nil, BufLen);
  if RetVal <> ERROR_BUFFER_OVERFLOW then
  begin
    Result := GStack.LocalAddress;
    Exit;
  end;

  GetMem(pAdapterInfo, BufLen);
  try
    RetVal := GetAdaptersInfo(pAdapterInfo, BufLen);
    if RetVal <> ERROR_SUCCESS then
    begin
      Result := GStack.LocalAddress;
      Exit;
    end;

    pAdapter := pAdapterInfo;
    while pAdapter <> nil do
    begin
      Desc := StrPasA(PAnsiChar(@pAdapter.Description[0]));
      if (Pos('Wireless', Desc) > 0) or (Pos('Wi-Fi', Desc) > 0) or
        (Pos('WLAN', Desc) > 0) then
      begin
        pIpAddr := @pAdapter.IpAddressList;
        while pIpAddr <> nil do
        begin
          if (pIpAddr.IpAddress.S[0] <> #0) and
            (StrPasA(PAnsiChar(@pIpAddr.IpAddress.S[0])) <> '0.0.0.0') then
          begin
            Result := StrPasA(PAnsiChar(@pIpAddr.IpAddress.S[0]));
            Exit;
          end;
          pIpAddr := pIpAddr.Next;
        end;
      end;
      pAdapter := pAdapter.Next;
    end;
  finally
    FreeMem(pAdapterInfo);
  end;

  if Result = '' then
    Result := GStack.LocalAddress;
end;

end.
