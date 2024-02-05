program mctester;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, ssockets, netcard, memcard
  { you can add units after this };

const
  SERVER_HOST = 'dev-02.home.lan';
  SERVER_PORT = 3846;

type

  { TMCTest }

  TMCTest = class(TCustomApplication)
  private
    procedure ClassTester;
    procedure ClearTestItem(mc: TNetCard);
    procedure TestAllCalls(mc: TNetCard);
    procedure AuthTester;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMCTest }

procedure TMCTest.ClassTester;
var
  mc: TNetCard;
  blk: TMemoryStream;
  info: PBlockInfo;
  b: integer;
  blklst: TStringList;
begin
  mc:=TNetCard.Create(SERVER_HOST, SERVER_PORT);
  try
    WriteLn('Connection made.');
    mc.Authenticate('TestKey123');
    mc.SelectCard(0);
    mc.AddAuth('TestRW1', True, True);
    ClearTestItem(mc);
    New(info);
    b:=mc.FindType(65, info);
    WriteLn('FindType(65) = ',b);
    b:=mc.FindApp(65, info);
    WriteLn('FindApp(65) = ',b);
    b:=mc.FindFree;
    WriteLn('Free Block: ',b);
    blk:=mc.ReadBlock(b);
    WriteLn(blk.ReadByte);
    info^.title:='TestItem';
    info^.appno:=48;
    info^.typno:=65;
    mc.WriteBlock(b, blk, info);
    info^.title:='BLank';
    mc.GetInfo(b, info);
    WriteLn('Title: ', info^.title);
    b:=mc.FindType(65, info);
    WriteLn('FindType(65) = ',b);
    b:=mc.FindApp(65, info);
    WriteLn('FindApp(65) = ',b);
    b:=mc.FindFree;
    WriteLn('Free Block: ',b);
    Dispose(info);
    blk.Free;
    blklst:=mc.BlockList;
    WriteLn(blklst.Strings[0]);
    blklst.Free;
    mc.DeleteBlock(b);
    mc.StopServer;
  finally
    mc.Free;
  end;
end;

procedure TMCTest.ClearTestItem(mc: TNetCard);
var
  b: integer;
  info: TBlockInfo;
begin
  repeat
    b:=mc.FindApp(48, @info);
    if b > 0 then
      mc.DeleteBlock(b);
  until b = 0;
end;

procedure TMCTest.TestAllCalls(mc: TNetCard);
var
  blk: TMemoryStream;
  b: integer;
  info: PBlockInfo;
  lst: TStringList;
begin
  WriteLn('Running tests with current auth...');
  try
    mc.SelectCard(0);
    blk:=mc.ReadBlock(1);
    try
      b:=mc.FindFree;
      mc.WriteBlock(b, blk);
      mc.DeleteBlock(b);
      New(info);
      mc.GetInfo(1, info);
      b:=mc.FindType($00, info);
      b:=mc.FindApp($00, info);
      Dispose(info);
      lst:=mc.BlockList;
      lst.Free;
      mc.Subscribe(1);
      mc.AddAuth('testadd', False, False);
    finally
      blk.Free;
    end;
  except
    On EInvalidCard do WriteLn('Invalid memory card selected.');
    On EAuthError do WriteLn('Action invalid.');
  end;
end;

procedure TMCTest.AuthTester;
var
  mc1, mc2: TNetCard;
begin
  WriteLn('Connecting to server...');
  mc1:=TNetCard.Create(SERVER_HOST, SERVER_PORT);
  try
    WriteLn('Auth as root key...');
    try
      mc1.Authenticate('FakeKey101');
    except
      on EAuthenticationFailed do TestAllCalls(mc1);
    end;
    mc1.Authenticate('TestKey123');
    WriteLn('Adding read-only user...');
    mc1.SelectCard(0);
    mc1.AddAuth('TestRO1', True, False);
    WriteLn('Connecting as read-only user...');
    mc2:=TNetCard.Create(SERVER_HOST, SERVER_PORT);
    try
      mc2.Authenticate('TestRO1');
      TestAllCalls(mc2);
    finally
      mc2.Free;
    end;
    WriteLn('Adding write-only user...');
    mc1.AddAuth('TestWO1', False, True);
    WriteLn('Connecting as write-only user...');
    mc2:=TNetCard.Create(SERVER_HOST, SERVER_PORT);
    try
      mc2.Authenticate('TestWO1');
      TestAllCalls(mc2);
    finally
      mc2.Free;
    end;
    WriteLn('Adding read-write user...');
    mc1.AddAuth('TestRW1', True, True);
    WriteLn('Connecting as read-write user...');
    mc2:=TNetCard.Create(SERVER_HOST, SERVER_PORT);
    try
      mc2.Authenticate('TestRW1');
      TestAllCalls(mc2);
    finally
      mc2.Free;
    end;
    mc1.StopServer;
  finally
    mc1.Free;
  end;
end;

procedure TMCTest.DoRun;
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

  if HasOption('p', 'prefix') then
  begin
    WriteLn('A Prefix has been set: ',GetOptionValue('p','prefix'));
  end;

  // NetworkTester;
  ClassTester;
  // AuthTester;

  // stop program loop
  Terminate;
end;

constructor TMCTest.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMCTest.Destroy;
begin
  inherited Destroy;
end;

procedure TMCTest.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMCTest;
begin
  Application:=TMCTest.Create(nil);
  Application.Title:='Memory Card Test';
  Application.Run;
  Application.Free;
end.

