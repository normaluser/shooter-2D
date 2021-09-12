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
*** Opening a SDL2 - Window
***************************************************************************}

PROGRAM Shooter1;

{$COPERATORS OFF}
USES SDL2;

CONST SCREEN_WIDTH  = 1280;         { size of the grafic window }
      SCREEN_HEIGHT = 720;          { size of the grafic window }

TYPE                                { "S_" short for "Struct" from "C" }
     S_App    = RECORD
                  Window   : PSDL_Window;
                  Renderer : PSDL_Renderer;
                end;

VAR app      : S_App;
    Event    : PSDL_EVENT;
    exitLoop : BOOLEAN;

// *****************   DRAW   *****************

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

  app.Window := SDL_CreateWindow('Shooter 01', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
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
end;

procedure cleanUp;
begin
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
    end;  { CASE Event }
  end;    { SDL_PollEvent }
end;

// *****************   MAIN   *****************

begin
  InitSDL;
  exitLoop := FALSE;
  NEW(Event);

  while exitLoop = FALSE do
  begin
    prepareScene;
    doInput;
    presentScene;
    SDL_Delay(16);
  end;
  cleanUp;
  DISPOSE(Event);
end.
