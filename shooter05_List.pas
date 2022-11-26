PROGRAM Shooter5_Linked_List;

{$COPERATORS OFF} {$mode FPC} {$H+}
USES CRT;

CONST MAX_KEYBOARD_KEYS = 350;

TYPE                                       { "T" short for "TYPE" }
     PEntity    = ^TEntity;                { "P" short for "Pointer" }
     TEntity    = RECORD
                    x, y : byte;
                    next : PEntity;
                  end;
     TStage    = RECORD
                   fighterHead, fighterTail,
                   bulletHead, bulletTail : PEntity;
                 end;

VAR
    stage            : TStage;
    player,
    bullet           : PEntity;

// *****************   INIT   *****************

procedure initEntity(VAR e : PEntity);
begin
  with e^ do
  begin
    x := 0;
    y := 0;  next := NIL;
  end;
end;

procedure drawBullets;
VAR b : PEntity;
begin
  b := stage.bulletHead^.next;
  while (b <> NIL) do
  begin
    writeln(b^.x:2,' ',b^.y:2);
    b := b^.next;
  end;
end;

procedure drawPlayer;
VAR b : PEntity;
begin
  b := stage.fighterHead^.next;
  while (b <> NIL) do
  begin
    writeln('Player: ',b^.x:2,' ',b^.y:2);
    b := b^.next;
  end;
end;

procedure fireBullet(a : integer);
begin
  NEW(bullet);
  initEntity(bullet);
  stage.bulletTail^.next := bullet;
  stage.bulletTail := bullet;
  bullet^.x := player^.x + a;
  bullet^.y := player^.y + a;
end;

procedure initPlayer;
begin
  NEW(player);
  initEntity(player);
  stage.fighterTail^.next := player;
  stage.fighterTail := player;
  player^.x := 100;
  player^.y := 100;
end;

procedure initStage;
var i : byte;
begin
  NEW(stage.fighterHead);
  NEW(stage.bulletHead);
  initEntity(stage.fighterHead);
  initEntity(stage.bulletHead);
  stage.fighterTail := stage.fighterHead;
  stage.bulletTail  := stage.bulletHead;
  initPlayer;
  for i:=1 to 10 do
    fireBullet(i);
end;

procedure Loesch_Liste(a : PEntity);
VAR t : PEntity;
begin
  while (a <> NIL) do
  begin
    t := a^.next;
    DISPOSE(a);
    a := t;
  end;
end;

procedure cleanUp;
begin
  Loesch_Liste(stage.bulletHead^.next);
  DISPOSE(player);
  DISPOSE(stage.fighterHead);
  DISPOSE(stage.bulletHead);
  if ExitCode <> 0 then WriteLn('CleanUp complete!');
end;

procedure AtExit;
begin
  if ExitCode <> 0 then cleanUp;
  if Exitcode <> 0 then WriteLn('GetError()');
end;

begin
  clrscr;
  AddExitProc(@AtExit);
  InitStage;
  drawPlayer;
  drawBullets;

  cleanUp;
  AtExit;
end.
