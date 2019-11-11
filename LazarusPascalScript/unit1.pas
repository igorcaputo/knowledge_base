unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, uPSComponent, SynEdit, SynCompletion, SynHighlighterMulti,
  SynExportHTML, SynHighlighterPas, uPSR_std,
  uPSC_std,
  uPSR_stdctrls,
  uPSC_stdctrls,
  uPSR_forms,
  uPSC_forms,
  uPSC_graphics,
  uPSC_controls,
  uPSC_classes,
  uPSR_graphics,
  uPSR_controls,
  uPSR_classes, uPSCompiler, uPSRuntime;

type

  { TfrmEditor }

  TfrmEditor = class(TForm)
    btExecutar: TButton;
    mSaida: TMemo;
    mMensagens: TMemo;
    psExecutar: TPSScript;
    SynAutoComplete1: TSynAutoComplete;
    mFonte: TSynEdit;
    SynExporterHTML1: TSynExporterHTML;
    SynPasSyn1: TSynPasSyn;
    procedure btExecutarClick(Sender: TObject);
    procedure psExecutarCompile(Sender: TPSScript);
    procedure psExecutarCompImport(Sender: TObject; x: TPSPascalCompiler);
    procedure psExecutarExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
    procedure psExecutarExecute(Sender: TPSScript);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmEditor: TfrmEditor;

procedure NossoWriteLn(const s: string);
procedure NossoReadLn(var s: string);

function ConstrutorDeForms(NomeClasse: string): TForm;

implementation

procedure NossoWriteLn(const s: string);
begin
  frmEditor.mSaida.Lines.Add(s);
end;

procedure NossoReadLn(var s: string);
begin
  s := InputBox('Digite um valor:', 'Digite um valor:', '');
end;

function ConstrutorDeForms(NomeClasse: string): TForm;
var
  ClasseDaForm: TFormClass;
begin
  ClasseDaForm := TFormClass(FindClass(NomeClasse));
  Result := ClasseDaForm.Create(Application);
end;




{ TfrmEditor }

procedure TfrmEditor.btExecutarClick(Sender: TObject);
var
  Compilou, Executou: boolean;
  i: integer;
begin
  mMensagens.Clear;
  mSaida.Clear;


  psExecutar.Script.Text := mFonte.Text;
  Compilou := psExecutar.Compile;

  if Compilou then
  begin
    mMensagens.Lines.Add('Programa compilado com sucesso!');
    Executou := psExecutar.Execute;

    if Executou then
    begin
      mMensagens.Lines.Add('Programa executado com sucesso!');
    end
    else
    begin
      mMensagens.Lines.Add('Ocorreu o erro de execução: ' + psExecutar.ExecErrorToString +
        ' onde? ' + IntToStr(psExecutar.ExecErrorProcNo) + '.' + IntToStr(
        psExecutar.ExecErrorByteCodePosition));
    end;

  end
  else
    mMensagens.Lines.Add('Erro de compilação:');

  for i := 0 to psExecutar.CompilerMessageCount - 1 do
  begin
    mMensagens.Lines.Add('Compilador: ' + psExecutar.CompilerErrorToStr(i));
  end;
end;

procedure TfrmEditor.psExecutarCompile(Sender: TPSScript);
begin
  psExecutar.AddFunction(@NossoWriteLn, 'procedure Writeln(s: string);');
  psExecutar.AddFunction(@NossoReadLn, 'procedure ReadLn(var s: string);');
  psExecutar.AddFunction(@ShowMessage, 'procedure ShowMessage(s: string);');

  psExecutar.AddFunction(@ConstrutorDeForms,
    'function ConstrutorDeForms(NomeClasse: string):TForm;');

  //adicionamos isso para existir a variável
  psExecutar.AddRegisteredVariable('Application', 'TApplication');
  psExecutar.AddRegisteredVariable('Self', 'TForm');
end;

procedure TfrmEditor.psExecutarCompImport(Sender: TObject; x: TPSPascalCompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, True);
  SIRegister_Graphics(x, True);
  SIRegister_Controls(x);
  SIRegister_stdctrls(x);
  SIRegister_Forms(x);
end;

procedure TfrmEditor.psExecutarExecImport(Sender: TObject; se: TPSExec;
  x: TPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Graphics(x, True);
  RIRegister_Controls(x);
  RIRegister_stdctrls(x);
  RIRegister_Forms(x);
end;

procedure TfrmEditor.psExecutarExecute(Sender: TPSScript);
begin
  psExecutar.SetVarToInstance('Application', Application);
  psExecutar.SetVarToInstance('Self', Self);
end;

initialization
  {$I unit1.lrs}



end.

