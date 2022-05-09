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
*** Firing with the "LEFT Strg / Ctrl" key
***************************************************************************}

PROGRAM Shooter4;

{$COPERATORS OFF} {$mode FPC} {$H+}
USES CRT, SDL2, SDL2_Image;

CONST SCREEN_WIDTH  = 1280;            { size of the grafic window }
      SCREEN_HEIGHT = 720;             { size of the grafic window }

TYPE                                        { "T" short for "TYPE" }
     TApp    = RECORD
                  Window   : PSDL_Window;
                  Renderer : PSDL_Renderer;
                  up, down, left, right, fire : integer;
                end;
     TEntity = RECORD
                  x, y, dx, dy, health : integer;
                  Texture : PSDL_Texture;
                end;

VAR app              : TApp;
    player,
    bullet           : TEntity;
    Event            : TSDL_EVENT;
    exitLoop         : BOOLEAN;

// *****************   UTIL   *****************

procedure errorMessage(Message : string);
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,'Error Box',PChar(message),NIL);
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

function loadTexture(Pfad : string) : PSDL_Texture;
begin
  loadTexture := IMG_LoadTexture(app.Renderer, PChar(Pfad));
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

// ***************   INIT SDL   ***************

procedure initSDL;
VAR rendererFlags, windowFlags : integer;
begin
  rendererFlags := SDL_RENDERER_PRESENTVSYNC OR SDL_RENDERER_ACCELERATED;
  windowFlags := 0;

  if SDL_Init(SDL_INIT_VIDEO) < 0 then
    errorMessage(SDL_GetError());

  app.Window := SDL_CreateWindow('Shooter 04', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
  if app.Window = NIL then
    errorMessage(SDL_GetError());

  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');
  app.Renderer := SDL_CreateRenderer(app.Window, -1, rendererFlags);
  if app.Renderer = NIL then
    errorMessage(SDL_GetError());

  IMG_INIT(IMG_INIT_PNG OR IMG_INIT_JPG);
  SDL_ShowCursor(0);
end;

procedure AtExit;
begin
  SDL_DestroyTexture (player.Texture);
  SDL_DestroyRenderer(app.Renderer);
  SDL_DestroyWindow  (app.Window);
  SDL_Quit;
  if Exitcode <> 0 then WriteLn(SDL_GetError());
end;

// *****************   Input  *****************

procedure doKeyDown;
begin
  if Event.key._repeat = 0 then
  begin
    CASE Event.key.keysym.sym of
      SDLK_ESCAPE: exitLoop := TRUE;                { close Window with ESC-Key }

      SDLK_LEFT,  SDLK_KP_4: app.left  := 1;
      SDLK_RIGHT, SDLK_KP_6: app.right := 1;
      SDLK_UP,    SDLK_KP_8: app.up    := 1;
      SDLK_DOWN,  SDLK_KP_2: app.down  := 1;
      SDLK_LCTRL:            app.fire  := 1;
    end; { CASE }
  end;   { IF }
end;

procedure doKeyUp;
begin
  if Event.key._repeat = 0 then
  begin
    CASE Event.key.keysym.sym of
      SDLK_LEFT,   SDLK_KP_4: app.left  := 0;
      SDLK_RIGHT,  SDLK_KP_6: app.right := 0;
      SDLK_UP,     SDLK_KP_8: app.up    := 0;
      SDLK_DOWN,   SDLK_KP_2: app.down  := 0;
      SDLK_LCTRL:             app.fire  := 0;
    end; { CASE }
  end;   { IF }
end;

procedure doInput;
begin
  while SDL_PollEvent(@Event) = 1 do
  begin
    CASE Event.Type_ of

      SDL_QUITEV:          exitLoop := TRUE;        { close Window }
      SDL_MOUSEBUTTONDOWN: exitLoop := TRUE;        { if Mousebutton pressed }

      SDL_KEYDOWN: doKeyDown;
      SDL_KEYUP:   doKeyUp;

    end;  { CASE Event }
  end;    { SDL_PollEvent }
end;

// *****************   MAIN   *****************

begin
  CLRSCR;
  InitSDL;
  AddExitProc(@AtExit);
  exitLoop := FALSE;
  player.Texture := loadTexture('gfx/player.png');
  bullet.Texture := loadTexture('gfx/playerBullet.png');
  player.x := 100;
  player.y := 100;

  while exitLoop = FALSE do
  begin
    prepareScene;
    doInput;
    if app.up = 1 then    player.y := player.y - 4;
    if app.down = 1 then  player.y := player.y + 4;
    if app.left = 1 then  player.x := player.x - 4;
    if app.right = 1 then player.x := player.x + 4;
    if ((app.fire = 1) AND (bullet.health = 0)) then
    begin
      bullet.x := player.x;
      bullet.y := player.y;
      bullet.dx := 16;
      bullet.dy := 0;
      bullet.health := 1;
    end;
    bullet.x := bullet.x + bullet.dx;
    bullet.y := bullet.y + bullet.dy;
    if (bullet.x > SCREEN_WIDTH) then bullet.health := 0;
    blit(player.Texture, player.x, player.y);
    if (bullet.health > 0) then blit(bullet.Texture, bullet.x, bullet.y);
    presentScene;
    SDL_Delay(16);
  end;

  AtExit;
end.
