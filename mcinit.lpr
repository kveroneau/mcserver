program mcinit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, mcconfig
  { you can add units after this };

type

  { TMCInit }

  TMCInit = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMCInit }

procedure TMCInit.DoRun;
var
  ErrorMsg, root_token: String;
  port, cards: integer;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hp:r:c:', ['help','port:','root-token:','cards:']);
  if ErrorMsg<>'' then
  begin
    WriteLn(ErrorMsg);
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('p', 'port') then
    port:=StrToInt(GetOptionValue('p', 'port'))
  else
    port:=SERVER_PORT^;

  if HasOption('r', 'root-token') then
    root_token:=GetOptionValue('r', 'root-token')
  else
    root_token:=ROOT_KEY^;

  UpdateConfig(port, root_token);

  if HasOption('c', 'cards') then
  begin
    cards:=StrToInt(GetOptionValue('c', 'cards'));
    AddCard(cards);
    WriteLn('New Card BlockSize: ',cards);
  end;

  WriteLn('Port: ',port);
  WriteLn('Root Token: ',root_token);

  // stop program loop
  Terminate;
end;

constructor TMCInit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMCInit.Destroy;
begin
  inherited Destroy;
end;

procedure TMCInit.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMCInit;
begin
  Application:=TMCInit.Create(nil);
  Application.Title:='MCServer Init';
  Application.Run;
  Application.Free;
end.

