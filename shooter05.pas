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
*** Refactoring: pointers for linked lists
***************************************************************************}

PROGRAM Shooter5;

{$COPERATORS OFF} {$mode FPC} {$H+}
USES CRT, SDL2, SDL2_Image;

CONST SCREEN_WIDTH  = 1280;            { size of the grafic window }
      SCREEN_HEIGHT = 720;             { size of the grafic window }
      PLAYER_SPEED  = 4;
      PLAYER_BULLET_SPEED = 16.0;
      MAX_KEYBOARD_KEYS = 350;

TYPE                                        { "T" short for "TYPE" }
     TDelegating = procedure;
     TDelegate  = RECORD                    { "T" short for "TYPE" }
                    logic, draw : TDelegating;
                  end;
     TApp       = RECORD
                    Window   : PSDL_Window;
                    Renderer : PSDL_Renderer;
                    keyboard : Array[0..MAX_KEYBOARD_KEYS] OF integer;
                    Delegate : TDelegate;
                  end;
     PEntity    = ^TEntity;                { "P" short for "Pointer" }
     TEntity    = RECORD
                    x, y, dx, dy : double;
                    w, h, health, reload : integer;
                    Texture : PSDL_Texture;
                    next : PEntity;
                  end;
     TStage    = RECORD
                   fighterHead, fighterTail,
                   bulletHead, bulletTail : PEntity;
                 end;

VAR app              : TApp;
    stage            : TStage;
    player,
    bullet           : PEntity;
    CacheBulletTex   : PSDL_Texture;
    Event            : PSDL_EVENT;
    exitLoop         : BOOLEAN;
    gTicks           : UInt32;
    gRemainder       : double;

// *****************   INIT   *****************

procedure initEntity(VAR e : PEntity);
begin
  with e^ do
  begin
    x := 0.0; dx := 0.0; w := 0; health := 0; Texture := NIL;
    y := 0.0; dy := 0.0; h := 0; reload := 0; next := NIL;
  end;
end;

// *****************   UTIL   *****************

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

procedure drawPlayer;
begin
  blit(player^.Texture, TRUNC(player^.x), TRUNC(player^.y));
end;

procedure draw_Game;
begin
  drawPlayer;
  drawBullets;
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
    if (b^.x > SCREEN_WIDTH) then
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
  bullet^.Texture := CacheBulletTex;
  SDL_QueryTexture(bullet^.Texture, NIL, NIL, @dest.w, @dest.h);
  bullet^.w := dest.w;
  bullet^.h := dest.h;
  bullet^.x := bullet^.x + (player^.w DIV 2);
  bullet^.y := bullet^.y + (player^.h DIV 2) - (bullet^.h DIV 2);
  player^.reload := 8;
end;

procedure doPlayer;
begin
  player^.dx := 0;
  player^.dy := 0;
  if (player^.reload > 0) then DEC(player^.reload);
  if (app.keyboard[SDL_ScanCode_UP]    OR app.keyboard[SDL_ScanCode_KP_8]) = 1 then player^.dy := (-1 * PLAYER_SPEED);
  if (app.keyboard[SDL_ScanCode_DOWN]  OR app.keyboard[SDL_ScanCode_KP_2]) = 1 then player^.dy :=       PLAYER_SPEED;
  if (app.keyboard[SDL_ScanCode_LEFT]  OR app.keyboard[SDL_ScanCode_KP_4]) = 1 then player^.dx := (-1 * PLAYER_SPEED);
  if (app.keyboard[SDL_ScanCode_RIGHT] OR app.keyboard[SDL_ScanCode_KP_6]) = 1 then player^.dx :=       PLAYER_SPEED;
  if ((app.keyboard[SDL_ScanCode_LCTRL] = 1) AND (player^.reload <= 0))        then fireBullet;
  player^.x := player^.x + player^.dx;
  player^.y := player^.y + player^.dy;
end;

procedure initPlayer;
VAR dest : TSDL_Rect;
begin
  NEW(player);
  initEntity(player);
  stage.fighterTail^.next := player;
  stage.fighterTail := player;
  player^.x := 100;
  player^.y := 100;
  player^.reload := 8;
  player^.Texture := loadTexture('gfx/player.png');
  SDL_QueryTexture(player^.Texture, NIL, NIL, @dest.w, @dest.h);
  player^.w := dest.w;
  player^.h := dest.h;
end;

procedure logic_Game;
begin
  doPlayer;
  doBullets;
end;

procedure initStage;
begin
  app.delegate.logic := @logic_Game;
  app.delegate.draw  := @draw_Game;
  NEW(stage.fighterHead);
  NEW(stage.bulletHead);
  initEntity(stage.fighterHead);
  initEntity(stage.bulletHead);
  stage.fighterTail := stage.fighterHead;
  stage.bulletTail  := stage.bulletHead;
  initPlayer;
  CacheBulletTex    := loadTexture('gfx/playerBullet.png');
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

  app.Window := SDL_CreateWindow('Shooter 05', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
  if app.Window = NIL then
    errorMessage(SDL_GetError());

  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');
  app.Renderer := SDL_CreateRenderer(app.Window, -1, rendererFlags);
  if app.Renderer = NIL then
    errorMessage(SDL_GetError());

  IMG_INIT(IMG_INIT_PNG OR IMG_INIT_JPG);
  SDL_ShowCursor(0);
end;

procedure Loesch_Liste(a : PEntity);
VAR t : PEntity;
begin
  t := a;
  while (t <> NIL) do
  begin a := t;
    DISPOSE(t);
    t := a^.next;
  end;
end;

procedure cleanUp;
begin
  DISPOSE(Event);
  Loesch_Liste(stage.bulletHead^.next);
  DISPOSE(player);
  DISPOSE(stage.fighterHead);
  DISPOSE(stage.bulletHead);
  if ExitCode <> 0 then WriteLn('CleanUp complete!');
end;

procedure AtExit;
begin
  if ExitCode <> 0 then cleanUp;
  SDL_DestroyTexture (player^.Texture);
  SDL_DestroyTexture (CacheBulletTex);
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

// *****************   MAIN   *****************

begin
  clrscr;
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
    app.delegate.logic;
    app.delegate.draw;
    presentScene;
    CapFrameRate(gRemainder, gTicks);
  end;

  cleanUp;
  AtExit;
end.
