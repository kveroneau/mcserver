unit mcpool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memcard, netcard, ssockets, mcconfig, logger;

type

  PSubscription = ^TSubscription;
  TSubscription = record
    socket: TSocketStream;
    card: integer;
    blkid: integer;
  end;

  { TMCPool }

  TMCPool = class(TObject)
  public
    constructor Create(const prefix: string);
    destructor Destroy; override;
    function GetBlockSize(const card: integer): Word;
    function ReadBlock(const card, blkid: integer): TMemoryStream;
    procedure WriteBlock(const card, blkid: integer; blk: TStream);
    procedure WriteBlock(const card, blkid: integer; blk: TStream; info: PBlockInfo);
    procedure DeleteBlock(const card, blkid: integer);
    procedure GetInfo(const card, blkid: integer; info: PBlockInfo);
    function FindFree(const card: integer): integer;
    function FindType(const card, typno: integer; info: PBlockInfo): integer;
    function FindApp(const card, appno: integer; info: PBlockInfo): integer;
    function BlockList(const card: integer): TStringList;
    function Subscribe(const card, blkid: integer; ASocket: TSocketStream): integer;
    procedure Unsubscribe(const idx: integer);
  private
    FLock: TRTLCriticalSection;
    FCards: Array[0..32] of TMemCard;
    FSubscriptions: Array[0..32] of PSubscription;
    FPrefix: string;
    procedure AddCard(const FileName: string; const BlockSize: integer);
    function FindFreeSub: integer;
    procedure FireWriteEvent(const card, blkid: integer);
    procedure SendEventPacket(sub: PSubscription);
  public
    property Prefix: string read FPrefix write FPrefix;
  end;

var
  Memcards: TMCPool;

implementation

{ TMCPool }

constructor TMCPool.Create(const prefix: string);
var
  i: integer;
  cards: PCardArray;
begin
  InitCriticalSection(FLock);
  for i:=0 to High(FCards) do
    FCards[i]:=Nil;
  for i:=0 to High(FSubscriptions) do
    FSubscriptions[i]:=Nil;
  FPrefix:=prefix;
  cards:=GetCardData;
  for i:=0 to High(cards^) do
    if cards^[i] > 0 then
      AddCard('card'+inttostr(i)+'.kmc', cards^[i]*512);
  Dispose(cards);
end;

destructor TMCPool.Destroy;
var
  i: integer;
begin
  DoneCriticalSection(FLock);
  for i:=0 to High(FCards) do
    if FCards[i] <> Nil then
      FCards[i].Free;
  for i:=0 to High(FSubscriptions) do
    if FSubscriptions[i] <> Nil then
      Dispose(FSubscriptions[i]);
  inherited Destroy;
end;

procedure TMCPool.AddCard(const FileName: string; const BlockSize: integer);
var
  i: Integer;
begin
  EnterCriticalSection(FLock);
  try
    for i:=0 to High(FCards) do
      if FCards[i] = Nil then
        Break;
    FCards[i]:=TMemCard.Create(FPrefix+FileName, BlockSize);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TMCPool.GetBlockSize(const card: integer): Word;
begin
  EnterCriticalSection(FLock);
  try
    if FCards[card] = Nil then
      Result:=0
    else
      Result:=FCards[card].BlockSize;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TMCPool.ReadBlock(const card, blkid: integer): TMemoryStream;
begin
  EnterCriticalSection(FLock);
  try
    if FCards[card] <> Nil then
      Result:=FCards[card].ReadBlock(blkid)
    else
      Result:=Nil;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TMCPool.WriteBlock(const card, blkid: integer; blk: TStream);
begin
  EnterCriticalSection(FLock);
  try
    if FCards[card] <> Nil then
    begin
      FCards[card].WriteBlock(blkid, blk);
      FCards[card].Flush;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
  FireWriteEvent(card, blkid);
end;

procedure TMCPool.WriteBlock(const card, blkid: integer; blk: TStream;
  info: PBlockInfo);
begin
  EnterCriticalSection(FLock);
  try
    if FCards[card] <> Nil then
    begin
      FCards[card].WriteBlock(blkid, blk, info);
      FCards[card].Flush;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
  FireWriteEvent(card, blkid);
end;

procedure TMCPool.DeleteBlock(const card, blkid: integer);
begin
  EnterCriticalSection(FLock);
  try
    if FCards[card] <> Nil then
      FCards[card].DeleteBlock(blkid);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TMCPool.GetInfo(const card, blkid: integer; info: PBlockInfo);
begin
  EnterCriticalSection(FLock);
  try
    if FCards[card] <> Nil then
      FCards[card].GetInfo(blkid, info);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TMCPool.FindFree(const card: integer): integer;
begin
  Result:=0;
  EnterCriticalSection(FLock);
  try
    if FCards[card] <> Nil then
      Result:=FCards[card].FindFree;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TMCPool.FindType(const card, typno: integer; info: PBlockInfo): integer;
begin
  Result:=0;
  EnterCriticalSection(FLock);
  try
    if FCards[card] <> Nil then
      Result:=FCards[card].FindType(typno, info);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TMCPool.FindApp(const card, appno: integer; info: PBlockInfo): integer;
begin
  Result:=0;
  EnterCriticalSection(FLock);
  try
    if FCards[card] <> Nil then
      Result:=FCards[card].FindApp(appno, info);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TMCPool.BlockList(const card: integer): TStringList;
begin
  Result:=Nil;
  EnterCriticalSection(FLock);
  try
    if FCards[card] <> Nil then
      Result:=FCards[card].BlockList;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TMCPool.Subscribe(const card, blkid: integer; ASocket: TSocketStream): integer;
begin
  Result:=FindFreeSub;
  if Result = -1 then
    Exit;
  EnterCriticalSection(FLock);
  try
    New(FSubscriptions[Result]);
    FSubscriptions[Result]^.socket:=ASocket;
    FSubscriptions[Result]^.card:=card;
    FSubscriptions[Result]^.blkid:=blkid;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TMCPool.Unsubscribe(const idx: integer);
begin
  Dispose(FSubscriptions[idx]);
  FSubscriptions[idx]:=Nil;
end;

function TMCPool.FindFreeSub: integer;
var
  i: integer;
begin
  Result:=-1;
  EnterCriticalSection(FLock);
  try
    for i:=0 to High(FSubscriptions) do
      if FSubscriptions[i] = Nil then
      begin
        Result:=i;
        Break;
      end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TMCPool.FireWriteEvent(const card, blkid: integer);
var
  i: integer;
begin
  EnterCriticalSection(FLock);
  try
    for i:=0 to High(FSubscriptions) do
      if Assigned(FSubscriptions[i]) then
        if (FSubscriptions[i]^.card = card) and (FSubscriptions[i]^.blkid = blkid) then
          SendEventPacket(FSubscriptions[i]);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TMCPool.SendEventPacket(sub: PSubscription);
var
  buf: TMCProtocol;
  card: ^Word;
  blkid: ^Word;
begin
  buf.op:=120;
  card:=@buf.data[0];
  blkid:=@buf.data[2];
  card^:=sub^.card;
  blkid^:=sub^.blkid;
  sub^.socket.Write(buf, SizeOf(buf));
end;

end.

