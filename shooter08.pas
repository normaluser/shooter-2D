{**************************************************************************
Copyright (C) 2015-2018 Parallel Realities

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, OR (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

***************************************************************************
The original source and a lot of explanations can be found at:
https://www.parallelrealities.co.uk/tutorials/#Shooter
converted from "C" to "Pascal" by Ulrich 2021
***************************************************************************
*** Enemies shoot back!
***************************************************************************}

PROGRAM Shooter8;

{$COPERATORS OFF}
USES CRT, SDL2, SDL2_Image, Math;

CONST SCREEN_WIDTH  = 1280;            { size of the grafic window }
      SCREEN_HEIGHT = 720;             { size of the grafic window }
      PLAYER_SPEED  = 4;
      PLAYER_BULLET_SPEED = 20.0;
      ALIEN_BULLET_SPEED = 8;
      RAND_MAX = 3276;
      MAX_KEYBOARD_KEYS = 350;
      SIDE_PLAYER = 0;
      SIDE_ALIEN = 1;
      FPS = 60;

TYPE                                        { "T" short for "TYPE" }
     TDelegating = (Logo, Highsc, Game);
     TDelegate = RECORD
                    logic, draw : TDelegating;
                  end;
     TApp     = RECORD
                  Window   : PSDL_Window;
                  Renderer : PSDL_Renderer;
                  keyboard : Array[0..MAX_KEYBOARD_KEYS] OF integer;
                  Delegate : TDelegate;
                end;
     PEntity  = ^TEntity;
     TEntity  = RECORD
                  x, y, dx, dy : double;
                  w, h, health, reload, side : integer;
                  Texture : PSDL_Texture;
                  next : PEntity;
                end;
     TStage   = RECORD
                  fighterHead, fighterTail,
                  bulletHead,  bulletTail    : PEntity;
                end;

VAR app                  : TApp;
    stage                : TStage;
    player,
    enemy,
    bullet               : PEntity;
    enemyTexture,
    bulletTexture,
    alienbulletTexture,
    playerTexture        : PSDL_Texture;
    Event                : PSDL_EVENT;
    exitLoop             : BOOLEAN;
    gTicks               : UInt32;
    gRemainder           : double;
    enemyspawnTimer,
    resetTimer           : integer;

// *****************   INIT   *****************

procedure initEntity(VAR e : PEntity);
begin
  e^.x := 0.0; e^.y := 0.0; e^.dx := 0.0;   e^.dy := 0.0;   e^.Texture := NIL;
  e^.w := 0;   e^.h := 0;   e^.health := 0; e^.reload := 0; e^.next := NIL;
end;

// *****************   UTIL   *****************

function collision(x1, y1, w1, h1, x2, y2, w2, h2 : integer) : BOOLEAN;
VAR a_Rect, b_Rect : TSDL_Rect;
begin
  collision := FALSE;
  a_Rect.x := x1; a_Rect.y := y1; a_Rect.w := w1; a_Rect.h := h1;
  b_Rect.x := x2; b_Rect.y := y2; b_Rect.w := w2; b_Rect.h := h2;
  if (SDL_HasIntersection(@a_Rect, @b_Rect) = SDL_TRUE) then collision := TRUE;
end;

procedure calcSlope(x1, y1, x2, y2 : integer; VAR dx, dy : double);
VAR steps : integer;
begin
  steps := MAX(ABS(x1-x2), ABS(y1-y2));
  if steps = 0 then
  begin
    dx := 0;
    dy := 0;
  end
  else
  begin
    dx := x1 - x2;
    dx := dx / steps;
    dy := y1 - y2;
    dy := dy / steps;
  end;
end;

procedure errorMessage(Message : PChar);
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,'Error Box',Message,NIL);
  HALT(1);
end;

// *****************   DRAW   *****************

procedure blit(Texture : PSDL_Texture; x, y : integer);
VAR dest : TSDL_Rect;
begin
  dest.x := x;
  dest.y := y;
  SDL_QueryTexture(Texture, NIL, NIL, @dest.w, @dest.h);
  SDL_RenderCopy(app.Renderer, Texture, NIL, @dest);
end;

function loadTexture(Pfad : PChar) : PSDL_Texture;
begin
  loadTexture := IMG_LoadTexture(app.Renderer, Pfad);
  if loadTexture = NIL then errorMessage(SDL_GetError());
end;

procedure prepareScene;
begin
  SDL_SetRenderDrawColor(app.Renderer, 32, 32, 32, 255);
  SDL_RenderClear(app.Renderer);
end;

procedure presentScene;
begin
  SDL_RenderPresent(app.Renderer);
end;

// *****************   Stage  *****************

procedure drawBullets;
VAR b : PEntity;
begin
  b := stage.bulletHead^.next;
  while (b <> NIL) do
  begin
    blit(b^.Texture, TRUNC(b^.x), TRUNC(b^.y));
    b := b^.next;
  end;
end;

procedure drawFighters;
VAR e : PEntity;
begin
  e := stage.fighterHead^.next;
  while (e <> NIL) do
  begin
    blit(e^.Texture, TRUNC(e^.x), TRUNC(e^.y));
    e := e^.next;
  end;
end;

procedure draw_Game;
begin
  drawFighters;
  drawBullets;
end;

procedure clipPlayer;
begin
  if player <> NIL then
  begin
    if (player^.x < 0) then player^.x := 0;
    if (player^.y < 0) then player^.y := 0;
    if (player^.x > (SCREEN_WIDTH  - player^.w)) then player^.x := (SCREEN_WIDTH  - player^.w);
    if (player^.y > (SCREEN_HEIGHT - player^.h)) then player^.y := (SCREEN_HEIGHT - player^.h);
  end;
end;

procedure spawnEnemies;
VAR dest : TSDL_Rect;
begin
  DEC(enemyspawnTimer);
  if enemyspawnTimer <= 0 then
  begin
    NEW(enemy);
    initEntity(enemy);
    stage.fighterTail^.next := enemy;
    stage.fighterTail := enemy;
    enemy^.Texture := enemyTexture;
    SDL_QueryTexture(enemy^.Texture, NIL, NIL, @dest.w, @dest.h);
    enemy^.w := dest.w;
    enemy^.h := dest.h;
    enemy^.x := SCREEN_WIDTH;
    enemy^.y := RANDOM(SCREEN_HEIGHT - enemy^.h);
    enemy^.dx := -1 * (2 + (RANDOM(RAND_MAX) MOD 4));
    enemy^.side := SIDE_ALIEN;
    enemy^.health := 1;
    enemy^.reload := FPS * (1 + (RANDOM(RAND_MAX) MOD 3));
    enemyspawnTimer := 30 + (RANDOM(RAND_MAX) MOD FPS);
  end;
end;

function bulletHitFighter(b : PEntity) : BOOLEAN;    { b = Bullet; e = Fighter }
VAR e : PEntity;
begin
  e := stage.fighterHead^.next;
  bulletHitFighter := FALSE;
  while (e <> NIL) do
  begin
    if (e^.side <> b^.side) then
    begin
      if (collision(TRUNC(b^.x), TRUNC(b^.y), b^.w, b^.h, TRUNC(e^.x), TRUNC(e^.y), e^.w, e^.h) = TRUE) then
      begin
        b^.health := 0;
        e^.health := 0;
        bulletHitFighter := TRUE;
      end;
    end;
    e := e^.next;
  end;
end;

procedure doBullets;
VAR b, prev : PEntity;
begin
  prev := stage.bulletHead;
  b := stage.bulletHead^.next;
  while (b <> NIL) do
  begin
    b^.x := b^.x + b^.dx;
    b^.y := b^.y + b^.dy;
    if ((bulletHitFighter(b) = TRUE) OR (b^.x < -b^.w) OR (b^.y < -b^.h) OR
        (b^.x > SCREEN_WIDTH) OR (b^.y > SCREEN_HEIGHT)) then
    begin
      if (b = stage.bulletTail) then
        stage.bulletTail := prev;
      prev^.next := b^.next;
      DISPOSE(b);
      b := prev;
    end;
    prev := b;
    b := b^.next;
  end;
end;

procedure doFighters;
VAR e, prev : PEntity;
begin
  prev := stage.fighterHead;
  e := stage.fighterHead^.next;
  while (e <> NIL) do
  begin
    e^.x := e^.x + e^.dx;
    e^.y := e^.y + e^.dy;
    if ((e <> player) AND (e^.x < -e^.w)) then
      e^.health := 0;
    if (e^.health = 0) then
    begin
      if (e = player) then
        player := NIL;
      if (e = stage.fighterTail) then
        stage.fighterTail := prev;
      prev^.next := e^.next;
      DISPOSE(e);
      e := prev;
    end;
    prev := e;
    e := e^.next;
  end;
end;

procedure fireAlienbullet(e : PEntity);
VAR dest : TSDL_Rect;
begin
  NEW(bullet);
  initEntity(bullet);
  stage.bulletTail^.next := bullet;
  stage.bulletTail := bullet;
  bullet^.x := e^.x;
  bullet^.y := e^.y;
  bullet^.health := 1;
  bullet^.Texture := alienbulletTexture;
  bullet^.side := e^.SIDE;
  SDL_QueryTexture(bullet^.Texture, NIL, NIL, @dest.w, @dest.h);
  bullet^.w := dest.w;
  bullet^.h := dest.h;
  bullet^.x := bullet^.x + (e^.w DIV 2) - (bullet^.w DIV 2);
  bullet^.y := bullet^.y + (e^.h DIV 2) - (bullet^.h DIV 2);
  calcSlope(TRUNC(player^.x + (player^.w DIV 2)), TRUNC(player^.y + (player^.h DIV 2)), TRUNC(e^.x), TRUNC(e^.y), bullet^.dx, bullet^.dy);
  bullet^.dx := bullet^.dx * ALIEN_BULLET_SPEED;
  bullet^.dy := bullet^.dy * ALIEN_BULLET_SPEED;
  bullet^.side := SIDE_ALIEN;
  e^.reload := RANDOM(FPS * 2);
end;

procedure doEnemies;
VAR e : PEntity;
begin
  e := stage.fighterHead^.next;
  while (e <> NIL) do
  begin
    if ((e <> player) AND (player <> NIL)) then
    begin
      DEC(e^.reload);
      if (e^.reload <= 0) then
        fireAlienbullet(e);
    end;
    e := e^.next;
  end;
end;

procedure fireBullet;
VAR dest : TSDL_Rect;
begin
  NEW(bullet);
  initEntity(bullet);
  stage.bulletTail^.next := bullet;
  stage.bulletTail := bullet;
  bullet^.x := player^.x;
  bullet^.y := player^.y;
  bullet^.dx := PLAYER_BULLET_SPEED;
  bullet^.health := 1;
  bullet^.Texture := bulletTexture;
  SDL_QueryTexture(bullet^.Texture, NIL, NIL, @dest.w, @dest.h);
  bullet^.w := dest.w;
  bullet^.h := dest.h;
  bullet^.x := bullet^.x + (player^.w DIV 2);
  bullet^.y := bullet^.y + (player^.h DIV 2) - (bullet^.h DIV 2);
  bullet^.side := SIDE_PLAYER;
  player^.reload := 8;
end;

procedure doPlayer;
begin
  if (player <> NIL) then
  begin
    player^.dx := 0;
    player^.dy := 0;
    if (player^.reload > 0) then DEC(player^.reload);
    if (app.keyboard[SDL_ScanCode_UP]    OR app.keyboard[SDL_ScanCode_KP_8]) = 1 then player^.dy := (-1 * PLAYER_SPEED);
    if (app.keyboard[SDL_ScanCode_DOWN]  OR app.keyboard[SDL_ScanCode_KP_2]) = 1 then player^.dy :=       PLAYER_SPEED;
    if (app.keyboard[SDL_ScanCode_LEFT]  OR app.keyboard[SDL_ScanCode_KP_4]) = 1 then player^.dx := (-1 * PLAYER_SPEED);
    if (app.keyboard[SDL_ScanCode_RIGHT] OR app.keyboard[SDL_ScanCode_KP_6]) = 1 then player^.dx :=       PLAYER_SPEED;
    if ((app.keyboard[SDL_ScanCode_LCTRL] = 1) AND (player^.reload <= 0))        then fireBullet;
  end;
end;

procedure initPlayer;
VAR dest : TSDL_Rect;
begin
  NEW(player);
  initEntity(player);
  stage.fighterTail^.next := player;
  stage.fighterTail := player;
  player^.health := 1;
  player^.x := 100;
  player^.y := 100;
  player^.Texture := playerTexture;
  SDL_QueryTexture(player^.Texture, NIL, NIL, @dest.w, @dest.h);
  player^.w := dest.w;
  player^.h := dest.h;
  player^.reload := 0;
  player^.side := SIDE_PLAYER;
end;

procedure resetStage;
VAR e : PEntity;
begin
  e := stage.fighterHead^.next;
  while (e <> NIL) do
  begin
    e := stage.fighterHead^.next;
    stage.fighterHead^.next := e^.next;
    DISPOSE(e);
    e := e^.next;
  end;

  e := stage.bulletHead^.next;
  while (e <> NIL) do
  begin
    e := stage.bulletHead^.next;
    stage.bulletHead^.next := e^.next;
    DISPOSE(e);
    e := e^.next;
  end;

  stage.fighterTail := stage.fighterHead;
  stage.bulletTail  := stage.bulletHead;

  initPlayer;

  enemyspawnTimer := 0;

  resetTimer := FPS * 3;
end;

procedure logic_Game;
begin
  doPlayer;
  doEnemies;
  doFighters;
  doBullets;
  spawnEnemies;
  clipPlayer;
  if (player = NIL) then
  begin
    DEC(resetTimer);
    if (resetTimer <= 0) then
      resetStage;
  end;
end;

procedure initStage;
begin
  app.delegate.logic := Game;
  app.delegate.draw  := Game;
  NEW(stage.fighterHead);
  NEW(stage.bulletHead);
  initEntity(stage.fighterHead);
  initEntity(stage.bulletHead);
  stage.fighterTail  := stage.fighterHead;
  stage.bulletTail   := stage.bulletHead;
  bulletTexture      := loadTexture('gfx/playerBullet.png');
  enemyTexture       := loadTexture('gfx/enemy.png');
  alienbulletTexture := loadTexture('gfx/alienBullet.png');
  playerTexture      := loadTexture('gfx/player.png');
  resetStage;
  NEW(Event);
end;

// ***************   INIT SDL   ***************

procedure initSDL;
VAR rendererFlags, windowFlags : integer;
begin
  rendererFlags := SDL_RENDERER_PRESENTVSYNC OR SDL_RENDERER_ACCELERATED;
  windowFlags := 0;

  if SDL_Init(SDL_INIT_VIDEO) < 0 then
    errorMessage(SDL_GetError());

  app.Window := SDL_CreateWindow('Shooter 08', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
  if app.Window = NIL then
    errorMessage(SDL_GetError());

  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');
  app.Renderer := SDL_CreateRenderer(app.Window, -1, rendererFlags);
  if app.Renderer = NIL then
    errorMessage(SDL_GetError());

  IMG_INIT(IMG_INIT_PNG OR IMG_INIT_JPG);
  SDL_ShowCursor(0);
end;

procedure cleanUp;
begin
  DISPOSE(Event);
  DISPOSE(player);
  DISPOSE(stage.fighterHead);
  DISPOSE(stage.bulletHead);
  if ExitCode <> 0 then WriteLn('CleanUp complete!');
end;

procedure AtExit;
begin
  if ExitCode <> 0 then cleanUp;
  SDL_DestroyTexture (alienbulletTexture);
  SDL_DestroyTexture (playerTexture);
  SDL_DestroyTexture (bulletTexture);
  SDL_DestroyRenderer(app.Renderer);
  SDL_DestroyWindow  (app.Window);
  SDL_Quit;
  if Exitcode <> 0 then WriteLn(SDL_GetError());
end;

// *****************   Input  *****************

procedure doInput;
begin
  while SDL_PollEvent(Event) = 1 do
  begin
    CASE Event^.Type_ of

      SDL_QUITEV:          exitLoop := TRUE;        { close Window }
      SDL_MOUSEBUTTONDOWN: exitLoop := TRUE;        { if Mousebutton pressed }

      SDL_KEYDOWN: begin
                     if ((Event^.key._repeat = 0) AND (Event^.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                       app.keyboard[Event^.key.keysym.scancode] := 1;
                     if (app.keyboard[SDL_ScanCode_ESCAPE]) = 1 then exitLoop := TRUE;
                   end;   { SDL_Keydown }

      SDL_KEYUP:   begin
                     if ((Event^.key._repeat = 0) AND (Event^.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                       app.keyboard[Event^.key.keysym.scancode] := 0;
                   end;   { SDL_Keyup }
    end;  { CASE Event }
  end;    { SDL_PollEvent }
end;

// *************   CAPFRAMERATE   *************

procedure CapFrameRate(VAR remainder : double; VAR Ticks : UInt32);
VAR wait, FrameTime : longint;
begin
  wait := 16 + TRUNC(remainder);
  remainder := remainder - TRUNC(remainder);
  frameTime := SDL_GetTicks - Ticks;
  DEC(wait, frameTime);
  if (wait < 1) then wait := 1;
  SDL_Delay(wait);
  remainder := remainder + 0.667;
  Ticks := SDL_GetTicks;
end;

// *************   DELEGATE LOGIC   ***********

procedure delegate_logic(Wahl : TDelegating);
begin
  CASE Wahl of
  Game : begin
           logic_Game;
           draw_Game;
         end;
  end;
end;

// *****************   MAIN   *****************

begin
  CLRSCR;
  RANDOMIZE;
  InitSDL;
  AddExitProc(@AtExit);
  InitStage;
  exitLoop := FALSE;
  gTicks := SDL_GetTicks;
  gRemainder := 0;

  while exitLoop = FALSE do
  begin
    prepareScene;
    doInput;
    delegate_logic(app.delegate.logic);
    presentScene;
    CapFrameRate(gRemainder, gTicks);
  end;

  resetStage;
  cleanUp;
  AtExit;
end.
