program test_all;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  Interfaces, //wtf?
  CustApp,
  //zexmlss,
  test_main,
  test_cf in 'test_cf.pas', test_numberformats

  { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun(); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun();
var
  ErrorMsg: String;
  _path: string;

begin
  // parse parameters
  if (HasOption('h', 'help')) then
  begin
    WriteHelp();
    Terminate();
    Exit;
  end;

  _path := ExtractFilePath(Paramstr(0)) + '1\';

  TestSimpleWrite(_path);

  // stop program loop
  Terminate();
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

procedure TMyApplication.WriteHelp();
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TMyApplication;

begin
  Application := TMyApplication.Create(nil);
  Application.Title := 'My Application';
  Application.Run();
  Application.Free();
end.
