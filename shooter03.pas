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
*** Moving the player with cursorkeys or NUMPAD-Keys
***************************************************************************}

PROGRAM Shooter3;

{$COPERATORS OFF}
USES SDL2, SDL2_Image;

CONST SCREEN_WIDTH  = 1280;
      SCREEN_HEIGHT = 720;

TYPE { "S_" short for "Struct" from "C" }
     S_App    = RECORD
                  Window   : PSDL_Window;
                  Renderer : PSDL_Renderer;
                  up, down, left, right : integer;
                end;
     S_Entity = RECORD
                  x, y : integer;
                  Texture : PSDL_Texture;
                end;

VAR app      : S_App;
    player   : S_Entity;
    Event    : PSDL_EVENT;
    exitLoop : BOOLEAN;

// *****************   UTIL   *****************

procedure errorMessage(Message : PChar);
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,'Error Box',Message,NIL); HALT(1);
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
  if loadTexture = NIL then errorMessage(SDL_GetError);
end;

procedure prepareScene;
begin
  SDL_SetRenderDrawColor(app.Renderer, 96, 128, 255, 255);
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
  begin
    writeln('Couldn''t initialize SDL: ', SDL_GetError());
    HALT(1);
  end;

  app.Window := SDL_CreateWindow('Shooter 03', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
  if app.Window = NIL then
  begin
    writeln('Failed to open ',SCREEN_WIDTH,' x ',SCREEN_HEIGHT,' window: ',SDL_GetError());
    HALT(1);
  end;

  app.Renderer := SDL_CreateRenderer(app.Window, -1, rendererFlags);
  if app.Renderer = NIL then
  begin
    writeln('Failed to create renderer: ',SDL_GetError());
    HALT(1);
  end;

  IMG_INIT(IMG_INIT_PNG OR IMG_INIT_JPG);
end;

procedure cleanUp;
begin
  SDL_DestroyTexture (player.Texture);
  SDL_DestroyRenderer(app.Renderer);
  SDL_DestroyWindow  (app.Window);
  SDL_Quit;
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
                     CASE Event^.key.keysym.sym of
                       SDLK_ESCAPE: exitLoop := TRUE;

                       SDLK_LEFT,  SDLK_KP_4: app.left  := 1;
                       SDLK_RIGHT, SDLK_KP_6: app.right := 1;
                       SDLK_UP,    SDLK_KP_8: app.up    := 1;
                       SDLK_DOWN,  SDLK_KP_2: app.down  := 1;
                     end; { CASE }
                   end;   { SDL_Keydown }

      SDL_KEYUP: begin
                   CASE Event^.key.keysym.sym of
                     SDLK_LEFT,   SDLK_KP_4: app.left  := 0;
                     SDLK_RIGHT,  SDLK_KP_6: app.right := 0;
                     SDLK_UP,     SDLK_KP_8: app.up    := 0;
                     SDLK_DOWN,   SDLK_KP_2: app.down  := 0;
                   end; { CASE }
                 end;   { SDL_KEYUP }
    end;  { CASE Event }
  end;    { SDL_PollEvent }
end;

// *****************   MAIN   *****************

begin
  InitSDL;
  exitLoop := FALSE;
  NEW(Event);
  player.Texture := loadTexture('gfx/player.png');
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
    blit(player.Texture, player.x, player.y);
    presentScene;
    SDL_Delay(16);
  end;
  cleanUp;
  DISPOSE(Event);
end.