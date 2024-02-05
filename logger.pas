unit logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, eventlog;

procedure LogInfo(const msg: string);
procedure LogWarn(const msg: string);
procedure LogError(const msg: string);

implementation

var
  log: TEventLog;

procedure SetupLog;
begin
  log:=TEventLog.Create(Nil);
  log.LogType:=ltFile;
  log.FileName:='mcserver.log';
  log.AppendContent:=True;
  log.Active:=True;
  log.Info('Memory Card Server starting...');
end;

procedure LogInfo(const msg: string);
begin
  log.Info(msg);
  {$IFDEF DEBUG}WriteLn('INFO: ', msg);{$ENDIF}
end;

procedure LogWarn(const msg: string);
begin
  log.Warning(msg);
  {$IFDEF DEBUG}WriteLn('WARN: ', msg);{$ENDIF}
end;

procedure LogError(const msg: string);
begin
  log.Error(msg);
  {$IFDEF DEBUG}WriteLn('ERROR: ', msg);{$ENDIF}
end;

initialization
  SetupLog;
finalization
  log.Free;
end.

