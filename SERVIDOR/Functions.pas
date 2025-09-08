unit Functions;

interface

uses
  System.SysUtils, System.Types, System.Classes,
  System.Variants, System.JSON, System.IOUtils, System.Threading;

function DirectoryIsWritable(const ADir: string): Boolean;
function GerarStringAleatoria(Tamanho: Integer;
  UsarNumeros, UsarLetrasMaiusculas: Boolean): string;
function ListarNomesDeArquivosParaJson(const ACaminhoDiretorio: string): string;
procedure DelayAsync(Milliseconds: Integer; ACallback: TProc);

implementation

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

function GerarStringAleatoria(Tamanho: Integer;
  UsarNumeros, UsarLetrasMaiusculas: Boolean): string;
const
  Numeros = '0123456789';
  LetrasMaiusculas = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  CaracteresPermitidos: string;
  I: Integer;
begin
  // Configura os caracteres permitidos baseado nos parâmetros
  CaracteresPermitidos := '';
  if UsarNumeros then
    CaracteresPermitidos := CaracteresPermitidos + Numeros;
  if UsarLetrasMaiusculas then
    CaracteresPermitidos := CaracteresPermitidos + LetrasMaiusculas;

  // Verifica se há caracteres permitidos
  if CaracteresPermitidos = '' then
    raise Exception.Create('Nenhum conjunto de caracteres foi selecionado');

  Randomize;
  Result := '';
  SetLength(Result, Tamanho);

  for I := 1 to Tamanho do
    Result[I] := CaracteresPermitidos[Random(Length(CaracteresPermitidos)) + 1];
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

end.
