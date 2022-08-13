{**************************************************************************
Copyright (C) 2015-2018 Parallel Realities

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

***************************************************************************
converted from "C" to "Pascal" by Ulrich 2022
***************************************************************************
* changed all PChar to string Types for better string handling!
***************************************************************************}

PROGRAM ppp04;

{$COPERATORS OFF} {$mode FPC} {$H+}
USES CRT, SDL2, SDL2_Image, SDL2_Mixer, Math, sysutils;

CONST SCREEN_WIDTH      = 1280;            { size of the grafic window }
      SCREEN_HEIGHT     = 720;             { size of the grafic window }
      MAX_TILES         = 7;
      TILE_SIZE         = 64;
      MAP_WIDTH         = 40;
      MAP_HEIGHT        = 20;
      MAP_RENDER_WIDTH  = 20;
      MAP_RENDER_HEIGHT = 12;
      PLAYER_MOVE_SPEED = 6;

      MAX_KEYBOARD_KEYS = 350;
      MAX_SND_CHANNELS  = 16;
      EF_NONE           = 0;
      EF_WEIGHTLESS     = (2 << 0);   //2
      EF_SOLID          = (2 << 1);   //4

TYPE                                        { "T" short for "TYPE" }
     TDelegating = (Game);
     TDelegate   = RECORD
                     logic, draw : TDelegating;
                   end;
     PTextur     = ^TTexture;
     TTexture    = RECORD
                     name : string;
                     texture : PSDL_Texture;
                     next : PTextur;
                   end;
     TApp        = RECORD
                     Window   : PSDL_Window;
                     Renderer : PSDL_Renderer;
                     keyboard : Array[0..MAX_KEYBOARD_KEYS] OF integer;
                     textureHead, textureTail : PTextur;
                     Delegate : TDelegate;
                   end;
     PEntity     = ^TEntity;
     TEntity     = RECORD
                     x, y, dx, dy : double;
                     w, h : integer;
                     isOnGround : Boolean;
                     texture : PSDL_Texture;
                     flags : longint;
                     next : PEntity;
                   end;
     TStage      = RECORD
                     camera : TSDL_Point;
                     map : ARRAY[0..PRED(MAP_WIDTH),0..PRED(MAP_HEIGHT)] of integer;
                     EntityHead, EntityTail : PEntity;
                   end;

VAR app        : TApp;
    stage      : TStage;
    event      : TSDL_EVENT;
    exitLoop   : BOOLEAN;
    gTicks     : UInt32;
    gRemainder : double;
    tiles      : ARRAY[1..MAX_TILES] of PSDL_Texture;
    pete       : ARRAY[0..1] of PSDL_Texture;
    player,
    selv       : PEntity;

// *****************   UTIL   *****************

procedure errorMessage(Message : string);
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,'Error Box',PChar(Message),NIL);
  HALT(1);
end;

function collision(x1, y1, w1, h1, x2, y2, w2, h2 : integer) : Boolean;
begin
  collision := (MAX(x1, x2) < MIN(x1 + w1, x2 + w2)) AND (MAX(y1, y2) < MIN(y1 + h1, y2 + h2));
end;

procedure InitEntity(VAR e : PEntity);
begin
  e^.x := 0.0; e^.y := 0.0; e^.dx := 0.0; e^.dy := 0.0; e^.w := 0; e^.h := 0;
  e^.isOnGround := FALSE; e^.texture := NIL; e^.flags := EF_NONE; e^.next := NIL;
end;

// *****************   DRAW   *****************

procedure blit(texture : PSDL_Texture; x, y, center : integer);
VAR dest : TSDL_Rect;
begin
  dest.x := x;
  dest.y := y;
  SDL_QueryTexture(texture, NIL, NIL, @dest.w, @dest.h);

  if center <> 0 then
  begin
    dest.x := dest.w DIV 2;
    dest.y := dest.h DIV 2;
  end;

  SDL_RenderCopy(app.Renderer, texture, NIL, @dest);
end;

procedure blitRect(texture : PSDL_Texture; src : PSDL_Rect; x, y : integer);
VAR dest : TSDL_Rect;
begin
  dest.x := x;
  dest.y := y;
  dest.w := src^.w;
  dest.h := src^.h;
  SDL_RenderCopy(app.Renderer, texture, src, @dest);
end;

// ****************   TEXTURE   ***************

procedure addTextureToCache(LName : string; LTexture : PSDL_Texture);
VAR cache : PTextur;
begin
  NEW(cache);
  app.textureTail^.next := cache;
  app.textureTail := cache;
  cache^.name := LName;
  cache^.texture := LTexture;
  cache^.next := NIL;
end;

function getTexture(name : string) : PSDL_Texture;
VAR tf : PTextur;
begin
  getTexture := NIL;
  tf := app.textureHead^.next;
  while (tf <> NIL) do
  begin
    if tf^.name = name then
      getTexture := tf^.texture;
    tf := tf^.next;
  end;
end;

function loadTexture(Pfad : string) : PSDL_Texture;
VAR tg : PSDL_Texture;
begin
  tg := getTexture(Pfad);
  if tg = NIL then
  begin
    tg := IMG_LoadTexture(app.Renderer, PChar(Pfad));
    if tg = NIL then
      errorMessage(SDL_GetError());
    addTextureToCache(Pfad, tg);
  end;
  loadTexture := tg;
end;

procedure loadTiles;
VAR i : integer;
    filename : string;
begin
  for i := 1 to MAX_TILES do
  begin
    filename := 'gfx/tile' + IntToStr(i) + '.png';
    tiles[i] := loadTexture(filename);
  end;
end;

// *****************    MAP   *****************

procedure drawMap;
VAR x, y, n, x1, x2, y1, y2, mx, my : integer;
begin
  x1 := (stage.camera.x MOD TILE_SIZE) * (-1);
  if (x1 = 0) then x2 := x1 + MAP_RENDER_WIDTH * TILE_SIZE
              else x2 := x1 + TILE_SIZE + MAP_RENDER_WIDTH * TILE_SIZE;

  y1 := (stage.camera.y MOD TILE_SIZE) * (-1);
  if (y1 = 0) then y2 := y1 + MAP_RENDER_HEIGHT * TILE_SIZE
              else y2 := y1 + TILE_SIZE + MAP_RENDER_HEIGHT * TILE_SIZE;

  mx := stage.camera.x DIV TILE_SIZE;
  my := stage.camera.y DIV TILE_SIZE;

  y := y1;
  while y <= y2 do
  begin
    x := x1;
    while x <= x2 do
    begin
      if ((mx >= 0) AND (my >= 0) AND (mx < MAP_WIDTH) AND (my < MAP_HEIGHT)) then
      begin
        n := stage.map[mx,my];
        if (n > 0) then
          blit(tiles[n], x, y, 0);
      end;
      INC(mx);
      INC(x, TILE_SIZE);
    end;
    mx := stage.camera.x DIV TILE_SIZE;
    INC(my);
    INC(y, TILE_SIZE);
  end;
end;

procedure loadMap(filename : string);
VAR i, x, y, le : integer;
    FileIn : text;
    line : string;
begin
  x := 0;
  assign (FileIn, filename);
  {$i-}; reset(FileIn); {$i+};
  if IOresult = 0 then
  begin
    for y := 0 to PRED(MAP_HEIGHT) do
    begin
      x := 0;
      readln(FileIn,line);
      line := stringReplace(line, ' ','',[rfReplaceAll]);
      le := length(line);

      for i := 1 to le do
      begin
        stage.map[x,y] := ORD(line[i]) - 48;
        INC(x);
      end;
    end;
    close(FileIn);
  end
  else errorMessage(filename + ' not found!');
end;

procedure initMap;
begin
  FillChar(stage.map, sizeof(stage.map), 0);
  loadTiles;
  loadMap('data/map01.dat');
end;


// ***************   ENTITIES   ***************

function isInsideMap(x, y : integer) : Boolean;
begin
  if ((x >= 0) AND (y >= 0) AND (x < MAP_WIDTH) AND (y < MAP_HEIGHT))
    then isInsideMap := TRUE     //1
    else isInsideMap := FALSE;   //0
end;

procedure addEntFromLine(line : string);
VAR e : PEntity;
    namen : string;
    l, a, b : integer;
begin
  NEW(e);
  initEntity(e);
  stage.EntityTail^.next := e;
  stage.EntityTail := e;
  l := SScanf(line, '%s %d %d', [@namen, @a, @b]);
  if namen = 'BLOCK' then
  begin
    e^.x := a; e^.y := b;
    e^.texture := loadTexture('gfx/block.png');
    SDL_QueryTexture(e^.texture, NIL, NIL, @e^.w, @e^.h);
    e^.flags := EF_SOLID + EF_WEIGHTLESS;
  end;
end;

procedure loadEnts(filename : string);
VAR Datei: Text;               (* Dateizeiger *)
    zeile : string;
BEGIN
  assign (Datei, filename);    (* Pfad festlegen *)
  {$i-}; reset(Datei); {$i+};  (* Datei zum Lesen oeffnen *)
  if IOResult = 0 then
  begin
    REPEAT
      readLn (Datei, zeile);     (* eine Zeile lesen *)
      addEntFromLine(zeile);
    UNTIL EOF (Datei);  (* Abbruch, wenn das Zeilenende erreicht ist; also wenn EOF TRUE liefert *)
    close (Datei);      (* Datei schliessen *)
  end
  else errorMessage(filename + ' not found!');
end;

procedure drawEntities;
VAR e : PEntity;
begin
  e := stage.entityHead^.next;
  while e <> NIL do
  begin
    blit(e^.texture, Round(e^.x - stage.camera.x), Round(e^.y - stage.camera.y), 0);
    e := e^.next;
  end;
end;

procedure moveToEntities(e : PEntity; dx, dy : double);
VAR other : PEntity;
    adj : integer;
begin
  other := stage.entityHead^.next;
  while other <> NIL do
  begin
    if ((other <> e) AND collision(Round(e^.x), Round(e^.y), Round(e^.w), Round(e^.h), Round(other^.x), Round(other^.y), Round(other^.w), Round(other^.h))) then
    begin
      if (other^.flags AND EF_SOLID) <> 0 then
      begin
        if (dy <> 0) then
        begin
          //adj = dy > 0 ? -e^.h : other^.h;
          if dy > 0 then
            adj := -e^.h
          else
            adj := other^.h;

          e^.y := other^.y + adj;
          e^.dy := 0;

          if dy > 0 then e^.isOnGround := TRUE;
        end;

        if (dx <> 0) then
        begin
          //adj = dx > 0 ? -e^.w : other^.w;
          if dx > 0 then
            adj := -e^.w
          else
            adj := other^.w;

          e^.x := other^.x + adj;
          e^.dx := 0;
        end;
      end;
    end;
    other := other^.next;
  end;
end;

procedure moveToWorld(e : PEntity; dx, dy : double);
VAR mx, my, hit, adj : integer;
begin
  if (dx <> 0) then
  begin
    //mx = dx > 0 ? (e->x + e->w) : e->x;
    if dx > 0 then mx := Round(e^.x + e^.w)
              else mx := Round(e^.x);
    mx := mx DIV TILE_SIZE;
    my := Round(e^.y) DIV TILE_SIZE;
    hit := 0;

    if ((NOT isInsideMap(mx, my)) OR (stage.map[mx][my] <> 0)) then
      hit := 1;

    my := (Round(e^.y + e^.h) - 1) DIV TILE_SIZE;

    if ((NOT isInsideMap(mx, my)) OR (stage.map[mx][my] <> 0)) then
      hit := 1;

    if hit = 1 then
    begin
      //adj = dx > 0 ? -e^.w : TILE_SIZE
      if dx > 0 then adj := -e^.w
                else adj := TILE_SIZE;
      e^.x := (mx * TILE_SIZE) + adj;
      e^.dx := 0;
    end;
  end;

  if (dy <> 0) then
  begin
    //my = dy > 0 ? (e^.y + e^.h) : e^.y;
    if dy > 0 then my := Round(e^.y + e^.h)
              else my := Round(e^.y);
    my := my DIV TILE_SIZE;
    mx := Round(e^.x) DIV TILE_SIZE;
    hit := 0;

    if ((NOT isInsideMap(mx, my)) OR (stage.map[mx][my] <> 0)) then
      hit := 1;

    mx := (Round(e^.x + e^.w) - 1) DIV TILE_SIZE;

    if ((NOT isInsideMap(mx, my)) OR (stage.map[mx][my] <> 0)) then
      hit := 1;

    if (hit = 1) then
    begin
      //adj = dy > 0 ? -e^.h : TILE_SIZE;
      if dy > 0 then adj := -e^.h
                else adj := TILE_SIZE;
      e^.y := (my * TILE_SIZE) + adj;
      e^.dy := 0;

      //e^.isOnGround = dy > 0;
      if dy > 0 then e^.isOnGround := TRUE;
    end;
  end;
end;

procedure move(e : PEntity);
begin
  if (NOT(e^.flags AND EF_WEIGHTLESS <> 0)) then
  begin
    e^.dy := e^.dy + 1.5;
    e^.dy := MAX(MIN(e^.dy, 18), -999);
  end;
  e^.isOnGround := FALSE;

  e^.x := e^.x + e^.dx;
  moveToWorld(e, e^.dx, 0);
  moveToEntities(e, e^.dx, 0);

  e^.y := e^.y + e^.dy;
  moveToWorld(e, 0, e^.dy);
  moveToEntities(e, 0, e^.dy);

  e^.x := MIN(MAX(e^.x, 0), MAP_WIDTH  * TILE_SIZE);
  e^.y := MIN(MAX(e^.y, 0), MAP_HEIGHT * TILE_SIZE);
end;

procedure doEntities;
VAR e : PEntity;
begin
  e := stage.EntityHead^.next;
  while e <> NIL do
  begin
    selv := e;
    move(e);
    e := e^.next;
  end;
end;

procedure initEntities;
begin
  loadEnts('data/ents01.dat');
end;

// ****************   CAMERA   ****************

procedure doCamera;
begin
  stage.camera.x := Round(player^.x + (player^.w / 2));
  stage.camera.y := Round(player^.y + (player^.h / 2));

  stage.camera.x := stage.camera.x - (SCREEN_WIDTH DIV 2);
  stage.camera.y := stage.camera.y - (SCREEN_HEIGHT DIV 2);

  stage.camera.x := MIN(MAX(stage.camera.x, 0), (MAP_WIDTH  * TILE_SIZE) - SCREEN_WIDTH);
  stage.camera.y := MIN(MAX(stage.camera.y, 0), (MAP_HEIGHT * TILE_SIZE) - SCREEN_HEIGHT);
end;

procedure doPlayer;
begin
  player^.dx := 0;

  if ((app.keyboard[SDL_SCANCODE_A] = 1) OR (app.keyboard[SDL_SCANCODE_LEFT] = 1)) then
  begin
    player^.dx := player^.dx - PLAYER_MOVE_SPEED;
    player^.texture := pete[1];
  end;

  if ((app.keyboard[SDL_SCANCODE_D] = 1) OR (app.keyboard[SDL_SCANCODE_RIGHT] = 1)) then
  begin
    player^.dx := player^.dx + PLAYER_MOVE_SPEED;
    player^.texture := pete[0];
  end;

  if ((app.keyboard[SDL_SCANCODE_I] = 1) AND (player^.isOnGround = TRUE)) then
    player^.dy := -20;

  if (app.keyboard[SDL_SCANCODE_SPACE] = 1) then
  begin
    player^.x := 0;
    player^.y := 0;
    app.keyboard[SDL_SCANCODE_SPACE] := 0;
  end;
end;

procedure initPlayer;
begin
  NEW(player);
  initEntity(player);
  stage.EntityTail^.next := player;
  stage.EntityTail := player;

  pete[0] := loadTexture('gfx/pete01.png');
  pete[1] := loadTexture('gfx/pete02.png');

  player^.texture := pete[0];

  SDL_QueryTexture(player^.texture, NIL, NIL, @player^.w, @player^.h);
end;

procedure prepareScene;
begin
  SDL_SetRenderDrawColor(app.Renderer, 0, 0, 0, 255);
  SDL_RenderClear(app.Renderer);
end;

procedure presentScene;
begin
  SDL_RenderPresent(app.Renderer);
end;

// *****************   STAGE   *****************

procedure draw_Game;
begin
  SDL_SetRenderDrawColor(app.renderer, 128, 192, 255, 255);
  SDL_RenderFillRect(app.renderer, NIL);

  drawMap;
  drawEntities;
end;

procedure logic_Game;
begin
  doPlayer;
  doEntities;
  doCamera;
end;

procedure initStage;
begin
  NEW(app.textureHead);
  app.textureHead^.name := '';
  app.textureHead^.texture := NIL;
  app.textureHead^.next := NIL;
  app.textureTail := app.textureHead;

  NEW(stage.entityHead);
  stage.entityHead^.next := NIL;
  stage.entityTail := stage.entityHead;

  initEntities;
  initPlayer;
  initMap;
end;

// ***************   INIT SDL   ***************

procedure initSDL;
VAR rendererFlags, windowFlags : integer;
begin
  rendererFlags := SDL_RENDERER_PRESENTVSYNC OR SDL_RENDERER_ACCELERATED;
  windowFlags := 0;
  if SDL_Init(SDL_INIT_VIDEO) < 0 then
    errorMessage(SDL_GetError());

  app.Window := SDL_CreateWindow('Pete''s Pizza Party 4', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
  if app.Window = NIL then
    errorMessage(SDL_GetError());

  if MIX_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 1024) < 0 then
    errorMessage(SDL_GetError());
  Mix_AllocateChannels(MAX_SND_CHANNELS);

  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');
  app.Renderer := SDL_CreateRenderer(app.Window, -1, rendererFlags);
  if app.Renderer = NIL then
    errorMessage(SDL_GetError());

  IMG_INIT(IMG_INIT_PNG OR IMG_INIT_JPG);
  SDL_ShowCursor(0);
end;

procedure destroyTexture;
VAR tex : PTextur;
begin
  tex := app.textureHead^.next;
  while (tex <> NIL) do
  begin
    tex := app.textureHead^.next;
    app.textureHead^.next := tex^.next;
    DISPOSE(tex);
    tex := tex^.next;
  end;
  DISPOSE(app.TextureHead);
end;

procedure destroyEntity;
VAR ent : PEntity;
begin
  ent := stage.EntityHead^.next;
  while (ent <> NIL) do
  begin
    ent := stage.EntityHead^.next;
    stage.EntityHead^.next := ent^.next;
    DISPOSE(ent);
    ent := ent^.next;
  end;
  DISPOSE(stage.EntityHead);
end;

procedure cleanUp;
begin
  destroyTexture;
  destroyEntity;
  if ExitCode <> 0 then WriteLn('CleanUp complete!');
end;

procedure initGame;
begin
end;

procedure atExit;
VAR i : byte;
begin
  for i := 1 to MAX_TILES do
    SDL_DestroyTexture (Tiles[i]);

  if ExitCode <> 0 then cleanUp;
  Mix_CloseAudio;
  SDL_DestroyRenderer(app.Renderer);
  SDL_DestroyWindow (app.Window);
  MIX_Quit;   { Quits the Music / Sound }
  IMG_Quit;   { Quits the SDL_Image }
  SDL_Quit;   { Quits the SDL }
  if Exitcode <> 0 then WriteLn(SDL_GetError());
  SDL_ShowCursor(1);
end;

// *****************   INPUT   *****************

procedure doInput;
begin
  while SDL_PollEvent(@event) = 1 do
  begin
    CASE event.Type_ of

      SDL_QUITEV:          exitLoop := TRUE;        { close Window }
      SDL_MOUSEBUTTONDOWN: exitLoop := TRUE;        { if Mousebutton pressed }

      SDL_KEYDOWN: begin
                     if ((event.key._repeat = 0) AND (event.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                       app.keyboard[event.key.keysym.scancode] := 1;
                     if (app.keyboard[SDL_ScanCode_ESCAPE]) = 1 then exitLoop := TRUE;
                   end;   { SDL_Keydown }

      SDL_KEYUP:   begin
                     if ((event.key._repeat = 0) AND (event.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                       app.keyboard[event.key.keysym.scancode] := 0;
                   end;   { SDL_Keyup }
    end;  { CASE event }
  end;    { SDL_PollEvent }
end;

// ***************   DELEGATE   ***************

procedure delegate_Logic(Wahl : TDelegating);
begin
  CASE Wahl of
  Game : begin
           logic_Game;
           draw_Game;
         end;
  end;
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
  CLRSCR;
  initSDL;
  initGame;
  initStage;
  addExitProc(@atExit);
  exitLoop := FALSE;

  while exitLoop = FALSE do
  begin
    prepareScene;
    doInput;
    delegate_Logic(Game);
    presentScene;
    CapFrameRate(gRemainder, gTicks);
  end;

  cleanUp;
  atExit;
end.
