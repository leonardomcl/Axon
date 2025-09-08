unit UConfig;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, System.IOUtils;

type
  TFConfig = class(TForm)
    GroupBoxServidor: TGroupBox;
    LbDir: TLabel;
    EdtPath: TEdit;
    StyleBook1: TStyleBook;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FConfig: TFConfig;

implementation

Uses UPrincipal, UFunctions;

{$R *.fmx}



procedure TFConfig.Button1Click(Sender: TObject);
var
  DefaultDir: string;
begin
  DefaultDir := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'Recebidos');

  if SelectDirectory('Selecione um diretório', DefaultDir, DiretorioSelecionado)
  then
  begin
    DiretorioSelecionado := IncludeTrailingPathDelimiter(DiretorioSelecionado);
    EdtPath.Text := DiretorioSelecionado;

    if not DirectoryIsWritable(DiretorioSelecionado) then
      raise Exception.Create
        ('Sem permissão de escrita no diretório selecionado');
  end
  else
  begin
    EdtPath.Text := '';
  end;

end;

end.
