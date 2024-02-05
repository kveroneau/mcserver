unit mcconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memcard;

type
  TAuthKey = string[20];
  PAuthKey = ^TAuthKey;
  PAuthData = ^TAuthData;
  TAuthData = record
    key: TAuthKey;
    read: bitpacked array[0..255] of Boolean;
    write: bitpacked array[0..255] of Boolean;
  end;
  PCardArray = ^TCardArray;
  TCardArray = array[0..32] of Byte;

var
  SERVER_PORT: ^word;
  ROOT_KEY: PAuthKey;

procedure AddAuthData(const key: TAuthKey; const card: integer; read, write: boolean);
function GetAuthData(const key: TAuthKey; auth: PAuthData): integer;
function GetCardData: PCardArray;
procedure UpdateConfig(const port: word; const root_key: TAuthKey);
procedure AddCard(const block_size: integer);
function GetUserList: TList;

implementation

const
  CARD_FILENAME = 'root.dat';
  CARD_BLOCKSIZE = 1024;
  TYPE_SERVER = $80;
  TYPE_AUTH = $90;

type
  PConfig = ^TConfig;
  TConfig = record
    port: word;
    root_key: TAuthKey;
    cards: TCardArray;
  end;

var
  rootcard: TMemCard;
  cfg: PConfig;

procedure CreateNewConfig;
var
  b: integer;
  blk: TMemoryStream;
  info: PBlockInfo;
begin
  New(info);
  info^.title:='Server Configuration';
  info^.appno:=$80;
  info^.typno:=TYPE_SERVER;
  b:=rootcard.FindFree;
  blk:=rootcard.ReadBlock(b);
  cfg^.port:=3845;
  cfg^.root_key:='TestKey123';
  FillChar(cfg^.cards, SizeOf(cfg^.cards), 0);
  cfg^.cards[0]:=2;
  blk.Write(cfg^, SizeOf(cfg^));
  rootcard.WriteBlock(b, blk, info);
  rootcard.Flush;
  blk.Free;
  Dispose(info);
end;

procedure InitializeConfig;
var
  b: Integer;
  info: PBlockInfo;
  blk: TMemoryStream;
begin
  new(cfg);
  new(info);
  b:=rootcard.FindType(TYPE_SERVER, info);
  if b > 0 then
  begin
    blk:=rootcard.ReadBlock(b);
    blk.Read(cfg^, Sizeof(cfg^));
    blk.Free;
  end
  else
    CreateNewConfig;
  Dispose(info);
  SERVER_PORT:=@cfg^.port;
  ROOT_KEY:=@cfg^.root_key;
end;

procedure AddAuthData(const key: TAuthKey; const card: integer; read, write: boolean);
var
  b, i, idx: integer;
  info: PBlockInfo;
  blk: TMemoryStream;
  auth: PAuthData;
  update: boolean;
begin
  New(auth);
  idx := GetAuthData(key, auth);
  if idx > -1 then
  begin
    auth^.read[card]:=read;
    auth^.write[card]:=write;
    update:=True;
  end
  else
  begin
    auth^.key:=key;
    for i:=0 to 255 do
    begin
      auth^.read[i]:=False;
      auth^.write[i]:=False;
    end;
    auth^.read[card]:=read;
    auth^.write[card]:=write;
    update:=False;
  end;
  new(info);
  b:=rootcard.FindType(TYPE_AUTH, info);
  if b > 0 then
  begin
    blk:=rootcard.ReadBlock(b);
    if not update then
    begin
      blk.Seek(info^.total*SizeOf(auth^), soBeginning);
      Inc(info^.total);
    end
    else
      blk.Seek(idx*SizeOf(auth^), soBeginning);
    blk.Write(auth^, SizeOf(auth^));
    rootcard.WriteBlock(b, blk, info);
    rootcard.Flush;
    blk.Free;
  end
  else
  begin
    info^.title:='Auth Data';
    info^.appno:=$80;
    info^.typno:=TYPE_AUTH;
    info^.total:=1;
    b:=rootcard.FindFree;
    blk:=rootcard.ReadBlock(b);
    blk.Write(auth^, SizeOf(auth^));
    rootcard.WriteBlock(b, blk, info);
    rootcard.Flush;
    blk.Free;
  end;
  Dispose(info);
  Dispose(auth);
end;

function GetAuthData(const key: TAuthKey; auth: PAuthData): integer;
var
  b, i: integer;
  info: PBlockInfo;
  blk: TMemoryStream;
begin
  Result:=-1;
  New(info);
  b:=rootcard.FindType(TYPE_AUTH, info);
  if b > 0 then
  begin
    blk:=rootcard.ReadBlock(b);
    for i:=1 to info^.total do
    begin
      blk.Read(auth^, SizeOf(auth^));
      if auth^.key = key then
      begin
        Result:=i-1;
        Break;
      end;
    end;
    blk.Free;
  end;
  Dispose(info);
end;

function GetCardData: PCardArray;
begin
  New(Result);
  Move(cfg^.cards, Result^, SizeOf(cfg^.cards));
end;

procedure SaveConfig;
var
  b: integer;
  blk: TMemoryStream;
  info: PBlockInfo;
begin
  New(info);
  b:=rootcard.FindType(TYPE_SERVER, info);
  Dispose(info);
  blk:=rootcard.ReadBlock(b);
  blk.Write(cfg^, SizeOf(cfg^));
  rootcard.WriteBlock(b, blk);
  rootcard.Flush;
  blk.Free;
end;

procedure UpdateConfig(const port: word; const root_key: TAuthKey);
begin
  cfg^.port:=port;
  cfg^.root_key:=root_key;
  SaveConfig;
end;

procedure AddCard(const block_size: integer);
var
  i: integer;
begin
  for i:=0 to 32 do
    if cfg^.cards[i] = 0 then
    begin
      cfg^.cards[i] := block_size div 512;
      SaveConfig;
      Exit;
    end;
end;

function GetUserList: TList;
var
  b, i: integer;
  info: PBlockInfo;
  blk: TMemoryStream;
  auth: PAuthData;
begin
  Result:=TList.Create;
  New(info);
  b:=rootcard.FindType(TYPE_AUTH, info);
  if b > 0 then
  begin
    blk:=rootcard.ReadBlock(b);
    for i:=1 to info^.total do
    begin
      New(auth);
      blk.Read(auth^, SizeOf(auth^));
      Result.Add(auth);
    end;
    blk.Free;
  end;
  Dispose(info);
end;

initialization
  rootcard:=TMemCard.Create(CARD_FILENAME, CARD_BLOCKSIZE);
  InitializeConfig;
finalization
  rootcard.Free;
  Dispose(cfg);
end.

