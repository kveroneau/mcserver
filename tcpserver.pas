unit tcpserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ssockets, mcprotocol, logger;

type

  { TMCTCPServer }

  TMCTCPServer = class(TInetServer)
  Private
    Procedure HandleExit;
  Protected
    Procedure DoConnect(ASocket: TSocketStream); override;
  end;

implementation

{ TMCTCPServer }

procedure TMCTCPServer.HandleExit;
begin
  LogWarn('Server Stop Requested.');
  StopAccepting(False);
end;

procedure TMCTCPServer.DoConnect(ASocket: TSocketStream);
var
  channel: TMCHandler;
begin
  {$IFDEF VERBOSE}LogInfo('Got connection!');{$ENDIF}
  channel:=TMCHandler.Create(ASocket);
  channel.OnExit:=@HandleExit;
  channel.Start;
  {$IFDEF VERBOSE}LogInfo('Thread created.');{$ENDIF}
  inherited DoConnect(ASocket);
end;

end.

