program mcserver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, BaseUnix,
  {$ENDIF}
  Classes, SysUtils, CustApp, tcpserver, mcprotocol, mcpool, mcconfig,
  ssockets, logger;

type

  { TMCServer }

  TMCServer = class(TCustomApplication)
  Private
    FServer: TMCTCPServer;
    Procedure CheckStatus(Sender: TObject);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure StopServer;
  end;

{ TMCServer }

procedure TMCServer.CheckStatus(Sender: TObject);
begin
  CheckSynchronize();
  Sleep(1000);
end;

procedure TMCServer.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hp:', ['help', 'prefix:']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  FServer:=TMCTCPServer.Create(SERVER_PORT^);
  FServer.OnIdle:=@CheckStatus;
  FServer.SetNonBlocking;
  if HasOption('p', 'prefix') then
    Memcards:=TMCPool.Create(GetOptionValue('p','prefix')+DirectorySeparator)
  else
    Memcards:=TMCPool.Create('');
  LogInfo('Server Listening on port '+IntToStr(SERVER_PORT^));
  try
    FServer.StartAccepting;
  except
    On ESocketError do LogError('Error starting server!');
  end;
  FServer.Free;
  LogInfo('Server terminated.');
  Sleep(1000);
  Memcards.Free;
  LogInfo('Memory Card Pool freed.');
  // stop program loop
  Terminate;
end;

constructor TMCServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMCServer.Destroy;
begin
  inherited Destroy;
end;

procedure TMCServer.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

procedure TMCServer.StopServer;
begin
  LogWarn('Server Stop has been requested.');
  FServer.StopAccepting(False);
end;

var
  Application: TMCServer;
{$IFDEF UNIX}
  oa, na: psigactionrec;

procedure HandleSignal(sig: cint); cdecl;
begin
  if (sig = SIGTERM) or (sig = SIGINT) then
    Application.StopServer;
end;

procedure HookSignals;
begin
  New(oa);
  New(na);
  na^.sa_handler:=sigactionhandler(@HandleSignal);
  FillChar(na^.sa_mask, SizeOf(na^.sa_mask), #0);
  na^.sa_flags:=0;
  {$IFDEF Linux}
  na^.sa_restorer:=Nil;
  {$ENDIF}
  if FPSigaction(SIGINT, na, oa) <> 0 then
  begin
    WriteLn('Unable to trap signal: SIGINT!');
    Halt(1);
  end;
  if FPSigaction(SIGTERM, na, oa) <> 0 then
  begin
    WriteLn('Unable to trap signal: SIGTERM!');
    Halt(1);
  end;
end;

{$ENDIF}
begin
  // SetHeapTraceOutput('heap.trc');
  {$IFDEF UNIX}HookSignals;{$ENDIF}
  Application:=TMCServer.Create(nil);
  Application.Title:='Memory Card Server';
  Application.Run;
  Application.Free;
  {$IFDEF UNIX}
  Dispose(na);
  Dispose(oa);
  {$ENDIF}
end.

