unit mcprotocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, netcard, ssockets, mcpool, memcard, mcconfig, logger;

type

  TExitEvent = Procedure of Object;

  { TMCHandler }

  TMCHandler = class(TThread)
  private
    FSocket: TSocketStream;
    FOnExit: TExitEvent;
    FAuthenticated: Boolean;
    FAuthData: PAuthData;
    FCard: integer;
    FSubs: Array[0..15] of integer;
    FSubCount: integer;
    Procedure QuickSend(const op: byte; const data: string);
    Procedure ProcessCommand(buf: PMCProtocol);
    Procedure ProcessExit;
    Procedure VerifyKey(key: TMCData);
    Procedure SelectCard(data: TMCData);
    Procedure ReadBlock(data: TMCData);
    Procedure WriteBlock(data: TMCData);
    Procedure DeleteBlock(data: TMCData);
    Procedure GetInfo(data: TMCData);
    Procedure FindFree(data: TMCData);
    Procedure FindType(data: TMCData);
    Procedure FindApp(data: TMCData);
    Procedure BlockList(data: TMCData);
    Procedure Subscribe(data: TMCData);
    Procedure Unsubscribe(data: TMCData);
    Procedure AddAuth(data: TMCData);
    Procedure SysStat(data: TMCData);
  protected
    Procedure Execute; override;
  public
    Constructor Create(ASocket: TSocketStream);
    Property OnExit: TExitEvent read FOnExit write FOnExit;
  end;

implementation

type
  EMCAccessDenied = class(Exception);

{ TMCHandler }

procedure TMCHandler.QuickSend(const op: byte; const data: string);
var
  buf: TMCProtocol;
  s: string[20];
begin
  buf.op:=op;
  s:=data;
  Move(s[0], buf.data, Length(s)+1);
  FSocket.Write(buf, SizeOf(buf));
end;

procedure TMCHandler.ProcessCommand(buf: PMCProtocol);
begin
  try
    case buf^.op of
      10: SelectCard(buf^.data);
      20: VerifyKey(buf^.data);
      30: Synchronize(@ProcessExit);
      40: ReadBlock(buf^.data);
      50: WriteBlock(buf^.data);
      60: DeleteBlock(buf^.data);
      70: GetInfo(buf^.data);
      80: FindFree(buf^.data);
      90: FindType(buf^.data);
      100: FindApp(buf^.data);
      110: BlockList(buf^.data);
      120: Subscribe(buf^.data);
      130: Unsubscribe(buf^.data);
      140: AddAuth(buf^.data);
      150: SysStat(buf^.data);
    end;
  except
    On EMCAccessDenied do QuickSend(254, 'Access Denied.');
  end;
end;

procedure TMCHandler.ProcessExit;
begin
  if not FAuthenticated or Assigned(FAuthData) then
    Exit;
  if Assigned(FOnExit) then
    FOnExit();
end;

procedure TMCHandler.VerifyKey(key: TMCData);
var
  s: string[20];
begin
  Move(key, s[0], 20);
  New(FAuthData);
  if s = ROOT_KEY^ then
  begin
    FAuthenticated:=True;
    Dispose(FAuthData);
    FAuthData:=Nil;
    QuickSend(200, 'AUTH OK');
  end
  else if GetAuthData(s, FAuthData) > -1 then
  begin
    FAuthenticated:=True;
    LogInfo('Authenticated via auth data.');
    QuickSend(200, 'AUTH OK');
  end
  else
  begin
    Dispose(FAuthData);
    FAuthData:=Nil;
    QuickSend(254, 'Access Denied.');
  end;
end;

procedure TMCHandler.SelectCard(data: TMCData);
var
  card: Word;
begin
  if not FAuthenticated then
  begin
    FSocket.WriteWord(0);
    Exit;
  end;
  Move(data, card, 2);
  LogInfo('Card selected: '+IntToStr(card));
  if Assigned(FAuthData) then
    if not (FAuthData^.read[card] or FAuthData^.write[card]) then
    begin
      FSocket.WriteWord(0);
      Exit;
    end;
  FCard:=card;
  FSocket.WriteWord(Memcards.GetBlockSize(card));
end;

procedure TMCHandler.ReadBlock(data: TMCData);
var
  blkid: Word;
  strm: TMemoryStream;
begin
  if not FAuthenticated or (FCard = -1) then
    Raise EMCAccessDenied.Create('Denied.');
  if Assigned(FAuthData) then
    if not FAuthData^.read[FCard] then
      Raise EMCAccessDenied.Create('Denied.');
  Move(data, blkid, 2);
  LogInfo('ReadBlock: '+IntToStr(blkid));
  QuickSend(200, 'READ OK');
  strm:=Memcards.ReadBlock(FCard, blkid);
  FSocket.CopyFrom(strm, strm.size);
  strm.Free;
end;

procedure TMCHandler.WriteBlock(data: TMCData);
var
  blkid: ^Word;
  strm: TMemoryStream;
  info: PBlockInfo;
  flag: ^Byte;
  bs: integer;
begin
  if not FAuthenticated or (FCard = -1) then
    Raise EMCAccessDenied.Create('Denied, not authenticated.');
  if Assigned(FAuthData) then
    if not FAuthData^.write[FCard] then
      Raise EMCAccessDenied.Create('Denied, not authorized.');
  blkid:=@data[0];
  flag:=@data[2];
  info:=@data[3];
  {$IFDEF VERBOSE}LogInfo('WRITE OK');{$ENDIF}
  QuickSend(200, 'WRITE OK');
  bs:=Memcards.GetBlockSize(FCard);
  strm:=TMemoryStream.Create;
  strm.SetSize(bs);
  strm.CopyFrom(FSocket, bs);
  if flag^ = 128 then
    Memcards.WriteBlock(FCard, blkid^, strm, info)
  else
    Memcards.WriteBlock(FCard, blkid^, strm);
  strm.Free;
end;

procedure TMCHandler.DeleteBlock(data: TMCData);
var
  blkid: ^Word;
begin
  if not FAuthenticated or (FCard = -1) then
    Raise EMCAccessDenied.Create('Denied.');
  if Assigned(FAuthData) then
    if not FAuthData^.write[FCard] then
      Raise EMCAccessDenied.Create('Denied.');
  blkid:=@data[0];
  Memcards.DeleteBlock(FCard, blkid^);
  QuickSend(200, 'DELETE OK');
end;

procedure TMCHandler.GetInfo(data: TMCData);
var
  blkid: ^Word;
  info: PBlockInfo;
begin
  if not FAuthenticated or (FCard = -1) then
    Raise EMCAccessDenied.Create('Denied.');
  blkid:=@data[0];
  New(info);
  info^.title:='ERROR';
  Memcards.GetInfo(FCard, blkid^, info);
  QuickSend(200, 'INFO FOLLOWS');
  FSocket.Write(info^, SizeOf(info^));
  Dispose(info);
end;

procedure TMCHandler.FindFree(data: TMCData);
begin
  if not FAuthenticated or (FCard = -1) then
    FSocket.WriteWord(0)
  else
    FSocket.WriteWord(Memcards.FindFree(FCard));
end;

procedure TMCHandler.FindType(data: TMCData);
var
  typno: ^Word;
  blkid: ^Word;
  info: PBlockInfo;
  buf: TMCProtocol;
begin
  if not FAuthenticated or (FCard = -1) then
    Raise EMCAccessDenied.Create('Denied.');
  typno:=@data[0];
  blkid:=@buf.data[0];
  info:=@buf.data[2];
  blkid^:=Memcards.FindType(FCard, typno^, info);
  buf.op:=90;
  FSocket.Write(buf, SizeOf(buf));
end;

procedure TMCHandler.FindApp(data: TMCData);
var
  appno: ^Word;
  blkid: ^Word;
  info: PBlockInfo;
  buf: TMCProtocol;
begin
  if not FAuthenticated or (FCard = -1) then
    Raise EMCAccessDenied.Create('Denied.');
  appno:=@data[0];
  blkid:=@buf.data[0];
  info:=@buf.data[2];
  buf.op:=100;
  blkid^:=Memcards.FindApp(FCard, appno^, info);
  FSocket.Write(buf, SizeOf(buf));
end;

procedure TMCHandler.BlockList(data: TMCData);
var
  strm: TStringList;
  list: PBlockList;
  i: integer;
begin
  if not FAuthenticated or (FCard = -1) then
    Raise EMCAccessDenied.Create('Denied.');
  strm:=Memcards.BlockList(Fcard);
  try
    New(list);
    for i:=1 to 15 do
      list^[i]:=strm.Strings[i-1];
    QuickSend(200, 'LIST FOLLOWS');
    FSocket.Write(list^, SizeOf(list^));
  finally
    Dispose(list);
    strm.Free;
  end;
end;

procedure TMCHandler.Subscribe(data: TMCData);
var
  blkid: ^Word;
  i: integer;
begin
  if not FAuthenticated or (FCard = -1) then
    Raise EMCAccessDenied.Create('Denied.');
  blkid:=@data[0];
  i:=Memcards.Subscribe(FCard, blkid^, FSocket);
  if i > -1 then
  begin
    FSubs[FSubCount]:=i;
    Inc(FSubCount);
  end;
  QuickSend(200, 'SUB OK');
end;

procedure TMCHandler.Unsubscribe(data: TMCData);
var
  i: integer;
begin
  if not FAuthenticated then
    Raise EMCAccessDenied.Create('Denied.');
  for i := 0 to High(FSubs) do
    if FSubs[i] > -1 then
      Memcards.Unsubscribe(FSubs[i]);
end;

procedure TMCHandler.AddAuth(data: TMCData);
var
  a: TCardAuth;
begin
  LogWarn('Add auth request!');
  if not FAuthenticated or (FCard = -1) then
    Raise EMCAccessDenied.Create('Denied.');
  if Assigned(FAuthData) then
    Raise EMCAccessDenied.Create('Denied.');
  Move(data, a, SizeOf(a));
  LogWarn('Add auth key: '+a.key);
  AddAuthData(a.key, FCard, a.read, a.write);
  QuickSend(200, 'AUTH ADDED');
end;

procedure TMCHandler.SysStat(data: TMCData);
var
  users: TList;
  i: integer;
  auth: PAuthData;
begin
  if not FAuthenticated or Assigned(FAuthData) then
  begin
    if not FAuthenticated then
      WriteLn('1 OK');
    if Assigned(FAuthData) then
      WriteLn('2 OK');
  end;
  users:=GetUserList;
  for i:=0 to users.Count-1 do
  begin
    auth:=users.Items[i];

  end;
  QuickSend(200, 'OK');
  users.Free;
end;

procedure TMCHandler.Execute;
var
  buf: TMCProtocol;
  size: Integer;
  running: Boolean;
begin
  running:=True;
  try
    repeat
      size:=FSocket.Read(buf, SizeOf(buf));
      if size > 0 then
      begin
        if buf.op = 255 then
          running:=False
        else
          ProcessCommand(@buf);
      end
      else
        running:=False;
    until not running;
    LogInfo('Disconnected.');
    Unsubscribe(buf.data);
    if Assigned(FAuthData) then
      Dispose(FAuthData);
  finally
    FSocket.Free;
  end;
end;

constructor TMCHandler.Create(ASocket: TSocketStream);
var
  i: integer;
begin
  inherited Create(True);
  FreeOnTerminate:=True;
  FSocket:=ASocket;
  FCard:=-1;
  FAuthenticated:=False;
  FAuthData:=Nil;
  for i:=0 to High(FSubs) do
    FSubs[i]:=-1;
  FSubCount:=0;
end;

end.

