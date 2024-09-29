{*****************************************************************************
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

******************************************************************************
The original source and a lot of explanations can be found at:
https://www.parallelrealities.co.uk/tutorials/#Shooter
converted from "C" to "Pascal" by Ulrich 2021
******************************************************************************
*** Title screen and finishing touches
*** without memory holes; tested with: fpc -Criot -gl -gh shooter15-fa.pas
*** shooter15-fa should run in it's own directory,
*** or rename "Higscore.json" to "Highscore1.json" in line 71 for to have less problems... :)
******************************************************************************}

PROGRAM Shooter15_fps_atlas;
{$mode FPC} {$H+}    { "$H+" necessary for conversion of String to PChar !!; H+ => AnsiString }
{$COPERATORS OFF}
USES SDL2, SDL2_Image, SDL2_Mixer, Math, JsonTools, sysutils;

CONST SCREEN_WIDTH  = 1280;            { size of the grafic window }
      SCREEN_HEIGHT = 720;             { size of the grafic window }
      PLAYER_SPEED  = 4.0;
      PLAYER_BULLET_SPEED = 20.0;
      ALIEN_BULLET_SPEED = 8.0;
      POINTSPOD_TIME = 10.0;
      RAND_MAX = 3276;
      NUM_HighScores = 8;
      MAX_KEYBOARD_KEYS = 350;
      MAX_SCORE_NAME_LENGTH = 16;
      MAX_STRING_LENGTH = 50;
      SIDE_PLAYER = 0;
      SIDE_ALIEN = 1;
      cFPS = 60;                       { Ganzzahlig }
      LOGIC_RATE = (cFPS / 1000);      { Logic_Rate => real number }
      MAX_STARS = 500;

      MAX_SND_CHANNELS = 8;
      SND_PLAYER_FIRE  = 1;
      SND_ALIEN_FIRE   = 2;
      SND_PLAYER_DIE   = 3;
      SND_ALIEN_DIE    = 4;
      SND_POINTS       = 5;
      SND_MAX          = 6;
      CH_ANY           = -1;
      CH_PLAYER        = 0;
      CH_ALIEN_FIRE    = 1;
      CH_POINTS        = 2;

      GLYPH_HEIGHT     = 28;
      GLYPH_WIDTH      = 18;

      NUMATLASBUCKETS  = 11;
      Json_Path        = 'data/atlas.json';
      Tex_Path         = 'gfx/atlas.png';
      Font_Path        = 'gfx/font.png';
      ScorePath        = 'Highscore.json';

TYPE TDelegating = Procedure;               { "T" short for "TYPE" }
     TString16   = String[MAX_SCORE_NAME_LENGTH];
     TString50   = String[MAX_STRING_LENGTH];
     String255   = String[255];
     TDev        = RECORD
                     TFPS : integer;
                   end;
     PAtlasImage = ^TAtlasImage;
     TAtlasImage = RECORD
                      FNam : String255;
                      Rec  : TSDL_Rect;
                      Rot  : integer;
                      Tex  : PSDL_Texture;
                      next : PAtlasImage;
                    end;
     AtlasArr    = ARRAY[0..NUMATLASBUCKETS] of PAtlasImage;
     TDelegate   = RECORD
                     logic, draw : TDelegating;
                   end;
     TApp        = RECORD
                     Window   : PSDL_Window;
                     Renderer : PSDL_Renderer;
                     keyboard : ARRAY[0..MAX_KEYBOARD_KEYS] OF integer;
                     inputText : String;
                     deltaTime : double;
                     dev : TDev;
                     delegate : TDelegate;
                   end;
     PEntity     = ^TEntity;
     TEntity     = RECORD
                     x, y, dx, dy, reload, health : double;
                     w, h, side : integer;
                     Texture : PAtlasImage;
                     next : PEntity;
                   end;
     PExplosion  = ^TExplosion;
     TExplosion  = RECORD
                     x, y, a, dx, dy : double;
                     r, g, b : integer;
                     next : PExplosion;
                   end;
     PDebris     = ^TDebris;
     TDebris     = RECORD
                     x, y, dx, dy : double;
                     rect : TSDL_Rect;
                     Texture : PAtlasImage;
                     life : double;
                     next : PDebris;
                   end;
     TStage      = RECORD
                     fighterHead,   fighterTail,
                     bulletHead,    bulletTail,
                     pointsHead,    pointsTail    : PEntity;
                     explosionHead, explosionTail : PExplosion;
                     debrisHead,    debrisTail    : PDebris;
                     score : integer;
                   end;
     TStar       = RECORD
                     x, y : double;
                     speed : integer;
                   end;
     THighScoreDef = RECORD
                       name : TString16;
                       recent, score : integer;
                     end;
     THighScoreARRAY =     ARRAY[0..PRED(NUM_HighScores)] OF THighScoreDef;
     TnewHighScoresARRAY = ARRAY[0..NUM_HighScores] OF THighScoreDef;

     TAlignment = (TEXT_LEFT, TEXT_CENTER, TEXT_RIGHT);

VAR app              : TApp;
    stage            : TStage;
    player,
    enemy,
    bullet           : PEntity;
    explode,
    fontTexture,
    atlasTex         : PSDL_Texture;
    atlases          : AtlasArr;
    SDL2Texture,
    ShooterTexture,
    backgroundAtlas,
    explosionAtlas   : PAtlasImage;
    Event            : TSDL_EVENT;
    newHighScoreFlag,
    exitLoop         : BOOLEAN;
    FPS              : integer;
    thentime,
    nextFPS          : LongInt;
    resetTimer,
    enemyspawnTimer,
    reveal,
    reveal_max,
    timeout,
    cursorBlink,
    backgroundX      : double;
    stars            : ARRAY[0..MAX_STARS] OF TStar;
    sounds           : ARRAY[1..SND_MAX] OF PMix_Chunk;
    music            : PMix_Music;
    SoundVol         : integer;
    MusicVol         : integer;
    HighScores       : THighScoreARRAY;
    newHighScore     : THighScoreDef;

// *****************   INIT   *****************

procedure initEntity(e : PEntity);
begin
  e^.x := 0.0; e^.y := 0.0; e^.dx := 0.0;   e^.dy := 0.0;   e^.Texture := NIL;  e^.side := 0;
  e^.w := 0;   e^.h := 0;   e^.health := 0; e^.reload := 0; e^.next := NIL;
end;

procedure initDebris(e : PDebris);
begin
  e^.x := 0.0;  e^.y := 0.0;  e^.dx := 0.0;  e^.dy := 0.0;
  e^.life := 0; e^.next := NIL; e^.Texture := NIL;
end;

procedure initExplosion(e : PExplosion);
begin
  e^.x := 0.0; e^.y := 0.0; e^.dx := 0.0; e^.dy := 0.0;
  e^.r := 0;   e^.g := 0;   e^.b  := 0;   e^.a  := 0;   e^.next := NIL;
end;

procedure initAtlasImage(e : PAtlasImage);
begin
  e^.FNam := ''; e^.Rot := 0; e^.Tex := NIL; e^.next := NIL;
end;

procedure initStageListenPointer;
begin
  NEW(stage.fighterHead);
  NEW(stage.bulletHead);
  NEW(stage.explosionHead);
  NEW(stage.debrisHead);
  NEW(stage.pointsHead);

  initEntity(stage.fighterHead);
  initEntity(stage.bulletHead);
  initExplosion(stage.explosionHead);
  initDebris(stage.debrisHead);
  initEntity(stage.pointsHead);

  stage.fighterTail := stage.fighterHead;
  stage.bulletTail  := stage.bulletHead;
  stage.explosionTail := stage.explosionHead;
  stage.debrisTail  := stage.debrisHead;
  stage.pointsTail  := stage.pointsHead;
end;

// *****************   UTIL   *****************

function collision(x1, y1, w1, h1, x2, y2, w2, h2 : double) : Boolean;
begin
  collision := (MAX(x1, x2) < MIN(x1 + w1, x2 + w2)) AND (MAX(y1, y2) < MIN(y1 + h1, y2 + h2));
end;

procedure calcSlope(x1, y1, x2, y2 : double; VAR dx, dy : double);
VAR steps : double;
begin
  steps := MAX(ABS((x1-x2)), ABS((y1-y2)));
  if steps <> 0.0 then
  begin
    dx := (x1 - x2) / steps;
    dy := (y1 - y2) / steps;
  end
  else
  begin
    dx := 0.0;
    dy := 0.0;
  end;
end;

function HashCode(Value : String255) : UInt32;     // DJB hash function
VAR i, x, Result : UInt32;
begin
  Result := 0;
  for i := 1 to Length(Value) do
  begin
    Result := (Result shl 4) + Ord(Value[i]);
    x := Result and $F0000000;
    if (x <> 0) then
      Result := Result xor (x shr 24);
    Result := Result and (not x);
  end;
  HashCode := Result;
end;

procedure errorMessage(Message1 : String);
begin
  SDL_ShowSimpleMessageBox(SDL_MessageBOX_ERROR,'Error Box',PChar(Message1),NIL);
  HALT(1);
end;

procedure logMessage(Message1 : String);
VAR Fmt : PChar;
begin
  Fmt := 'File not found: %s'#13;    // Formatstring und "ARRAY of const" als Parameteruebergabe in [ ]
  SDL_LogMessage(SDL_LOG_CATEGORY_APPLICATION, SDL_LOG_PRIORITY_WARN, Fmt, [PChar(Message1)]);
end;

procedure pathTest;
begin
  if NOT FileExists(Json_Path) then errorMessage(Json_Path + ' not found!');
  if NOT FileExists(Tex_Path)  then errorMessage(Tex_Path  + ' not found!');
  if NOT FileExists(Font_Path) then errorMessage(Font_Path + ' not found!');
end;

// *****************   SOUND  *****************

procedure loadSounds;
VAR i : byte;
begin
  sounds[1] := Mix_LoadWAV('sound/334227__jradcoolness__laser.ogg');
  if sounds[1] = NIL then logMessage('Soundfile: "334227__jradcoolness__laser.ogg"');
  sounds[2] := Mix_LoadWAV('sound/196914__dpoggioli__laser-gun.ogg');
  if sounds[2] = NIL then logMessage('Soundfile: "196914__dpoggioli__laser-gun.ogg"');
  sounds[3] := Mix_LoadWAV('sound/245372__quaker540__hq-explosion.ogg');
  if sounds[3] = NIL then logMessage('Soundfile: "245372__quaker540__hq-explosion.ogg"');
  sounds[4] := Mix_LoadWAV('sound/10 Guage Shotgun-SoundBible.com-74120584.ogg');
  if sounds[4] = NIL then logMessage('Soundfile: "10 Guage Shotgun-SoundBible.com-74120584.ogg"');
  sounds[5] := Mix_LoadWAV('sound/342749__rhodesmas__notification-01.ogg');
  if sounds[5] = NIL then logMessage('Soundfile: "342749__rhodesmas__notification-01.ogg"');

  for i := 1 to 5 do
    Mix_VolumeChunk(sounds[i], MIX_MAX_VOLUME);
end;

procedure loadMusic;
begin
  if music <> NIL then
  begin
    Mix_HaltMusic;
    Mix_FreeMusic(music);
    music := NIL;
  end;
  music := Mix_LoadMUS('music/Mercury.ogg');
  if music = NIL then logMessage('Music: "Mercury.ogg"');
  Mix_VolumeMusic(MIX_MAX_VOLUME);
end;

procedure playMusic(play : BOOLEAN);
VAR m : integer;
begin
  if play = TRUE then m := -1 else m := 0;
  Mix_PlayMusic(music, m);
end;

procedure playSound(id, channel : integer);
begin
  Mix_PlayChannel(channel, sounds[id], 0);
end;

procedure initSounds;
begin
  loadSounds;
end;

// *****************   DRAW   *****************

procedure blit(Texture : PSDL_Texture; x, y : double);
VAR dest : TSDL_Rect;
begin
  dest.x := TRUNC(x);
  dest.y := TRUNC(y);
  SDL_QueryTexture(Texture, NIL, NIL, @dest.w, @dest.h);
  SDL_RenderCopy(app.Renderer, Texture, NIL, @dest);
end;

procedure blitAtlasImage(atlas : PAtlasImage; x, y : double; center : integer);
VAR dest : TSDL_Rect;
    p : TSDL_Point;
begin
  dest.x := TRUNC(x);
  dest.y := TRUNC(y);
  dest.w := atlas^.Rec.w;
  dest.h := atlas^.Rec.h;

  if atlas^.Rot = 0 then
  begin
    if center <> 0 then
    begin
      dest.x := dest.x - (dest.w DIV 2);
      dest.y := dest.y - (dest.h DIV 2);
    end;
    SDL_RenderCopy(app.Renderer, atlas^.Tex, @atlas^.Rec, @dest);
  end
  else
  begin
    if center <> 0 then
    begin
      dest.x := dest.x - (dest.h DIV 2);
      dest.y := dest.y - (dest.w DIV 2);
    end;
    p.x := 0;
    p.y := 0;
    dest.y := dest.y + atlas^.Rec.w;
    SDL_RenderCopyEx(app.Renderer, atlas^.Tex, @atlas^.Rec, @dest, -90, @p, SDL_FLIP_NONE);
  end;
end;

procedure blitAtlasImageScaled(atlas : PAtlasImage; x, y : double; w, h : integer);
VAR dest : TSDL_Rect;
    p    : TSDL_Point;
begin
  dest.x := TRUNC(x);
  dest.y := TRUNC(y);
  dest.w := w;
  dest.h := h;

  if (atlas^.rot = 0) then
  begin
    p.x := 0;
    p.y := 0;
    dest.y := dest.y + atlas^.Rec.w;
    SDL_RenderCopyEx(app.Renderer, atlas^.tex, @atlas^.Rec, @dest, -90, @p, SDL_FLIP_NONE);
  end
  else
  begin
    SDL_RenderCopy(app.Renderer, atlas^.tex, @atlas^.Rec, @dest);
  end;
end;

procedure blitRectText(Texture : PSDL_Texture; src : PSDL_Rect; x, y : double);
VAR dest : TSDL_Rect;
begin
  dest.x := TRUNC(x);
  dest.y := TRUNC(y);
  dest.w := src^.w;
  dest.h := src^.h;
  SDL_RenderCopy(app.Renderer, Texture, src, @dest);
end;

procedure blitRect(Texture : PAtlasImage; src : PSDL_Rect; x, y : double);
VAR dest : TSDL_Rect;
begin
  dest.x := TRUNC(x);
  dest.y := TRUNC(y);
  dest.w := src^.w;
  dest.h := src^.h;
  SDL_RenderCopy(app.Renderer, Texture^.Tex, src , @dest);
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

// ****************   TEXTURE   ***************

function getAtlasImage(filename : String255) : PAtlasImage;
VAR a : PAtlasImage;
    i : UInt32;
begin
  i := HashCode(filename) MOD NUMATLASBUCKETS;
  a := atlases[i]^.next;
  getAtlasImage := NIL;
  while (a <> NIL) do
  begin
    if a^.fnam = filename then
      getAtlasImage := a;
    a := a^.next;
  end;
  if getAtlasImage = NIL then
    logMessage(filename);
end;

procedure loadAtlasTexture;
begin
  atlasTex := IMG_LoadTexture(app.Renderer, Tex_Path);
  if atlasTex = NIL then
    errorMessage(SDL_GetError());
end;

procedure loadAtlasData;
VAR i, x, y, w, h, r : integer;
    a, AtlasNew : PAtlasImage;
    N, C : TJsonNode;
    filename : String255;
begin
  if FileExists(Json_Path) then
  begin
    //Get the JSON data
    N := TJsonNode.Create;
    N.LoadFromFile(Json_Path);

    for c in n do
    begin
      filename  := c.Find('filename').AsString;
      x := c.Find('x').AsInteger;
      y := c.Find('y').AsInteger;
      w := c.Find('w').AsInteger;
      h := c.Find('h').AsInteger;
      r := c.Find('rotated').AsInteger;

      i := HashCode(filename) MOD NUMATLASBUCKETS;

      a := atlases[i];            // must be created and initialized before!

      while (a^.next <> NIL) do
        begin a := a^.next; end;

      NEW(AtlasNEW);
      initAtlasImage(AtlasNEW);

      AtlasNEW^.Fnam := filename;
      AtlasNEW^.Rec.x := x;
      AtlasNEW^.Rec.y := y;
      AtlasNEW^.Rec.w := w;
      AtlasNEW^.Rec.h := h;
      AtlasNEW^.Rot   := r;
      AtlasNEW^.Tex   := atlasTex;
      AtlasNEW^.next  := NIL;

      a^.next := atlasNEW;
    end;
    N.free;
  end
  else
  errorMessage('Atlas-Json not found!');
end;

procedure initAtlas;
VAR i : integer;
begin
  for i := 0 to NUMATLASBUCKETS do
  begin
    NEW(atlases[i]);
    initAtlasImage(atlases[i]);                // create and initialize PAtlasImage
  end;

  loadAtlasTexture;
  loadAtlasData;
end;

// *****************   TEXT   *****************

procedure drawText(x, y, r, g, b : integer; align : TAlignment; outText : TString50);
VAR i, len : integer;
    rect : TSDL_Rect;
begin
  len := LENGTH(outText);
  CASE align of
    TEXT_RIGHT :  begin x := x - (len * GLYPH_WIDTH);       end;
    TEXT_CENTER : begin x := x - (len * GLYPH_WIDTH) DIV 2; end;
  end;
  outText := UPCASE(outText);  { all capital letters }
  rect.w := GLYPH_WIDTH;
  rect.h := GLYPH_HEIGHT;
  rect.y := 0;
  SDL_SetTextureColorMod(fontTexture, r, g, b);

  for i := 1 to len do
  begin
    if (outText[i] IN [' '..'Z']) then
    begin
      rect.x := (ORD(outText[i]) - ORD(' ')) * GLYPH_WIDTH;
      blitRectText(fontTexture, @rect, x, y);
      INC(x, GLYPH_WIDTH);
    end;
  end;
end;

function numberfill(a : integer) : TString50;
VAR FMT : String;
begin
  Fmt := '[%.3d]';                  { Fmt: arguments for Format }
  numberfill := Format(Fmt, [a]);   { Format: format a String with given arguments (=> Fmt) }
end;

procedure initFonts;
begin
  fontTexture := IMG_loadTexture(app.Renderer, Font_Path);
  if fontTexture = NIL then
    errorMessage(SDL_GetError());
end;

// **************   Background  ***************

procedure drawBackground;
VAR dest : TSDL_Rect;
    x : integer;
begin
  x := TRUNC(backgroundX);
  while x < SCREEN_WIDTH do
  begin
    dest.x := x;
    dest.y := 0;
    dest.w := SCREEN_WIDTH;
    dest.h := SCREEN_HEIGHT;
    SDL_RenderCopy(app.Renderer, backgroundAtlas^.tex, @backgroundAtlas^.rec, @dest);
    INC(x, SCREEN_WIDTH);
  end;
end;

procedure drawStarfield;
VAR i, c : integer;
begin
  for i := 0 to PRED(MAX_STARS) do
  begin
    c := 32 * stars[i].speed;
    if c > 255 then c := 255;
    SDL_SetRenderDrawColor(app.Renderer, c, c, c, 255);
    SDL_RenderDrawLine(app.Renderer, TRUNC(stars[i].x), TRUNC(stars[i].y), TRUNC(stars[i].x + 3), TRUNC(stars[i].y));
  end;
end;

procedure doStarField;
VAR i : integer;
begin
  for i := 0 to PRED(MAX_STARS) do
  begin
    stars[i].x := stars[i].x - stars[i].speed * app.deltatime;
    if stars[i].x <= 0 then
      stars[i].x := stars[i].x + SCREEN_WIDTH;
  end;
end;

procedure doBackGround;
begin
  backgroundX := backgroundX - app.deltaTime;
  if backgroundX < (-SCREEN_WIDTH) then
    backgroundX := 0;
end;

procedure initStarfield;
VAR i : integer;
begin
  for i := 0 to PRED(MAX_STARS) do
  begin
    stars[i].x := (RANDOM(RAND_MAX) MOD SCREEN_WIDTH);
    stars[i].y := (RANDOM(RAND_MAX) MOD SCREEN_HEIGHT);
    stars[i].speed := 1 + (RANDOM(RAND_MAX) MOD 8);
  end;
end;

procedure initBackground;
begin
  backgroundAtlas := getAtlasImage('gfx/background.png');
  backgroundX := 0;
end;

// *****************   Stage  *****************

procedure drawHud;
begin
  drawText(SCREEN_WIDTH DIV 2, 10, 255, 255, 255, TEXT_CENTER, 'FPS: ' + numberfill(app.dev.TFPS));
  drawText(10, 10, 255, 255, 255, TEXT_LEFT, 'SCORE: ' + numberfill(stage.score));
  if ((stage.score < HighScores[0].score))
  then drawText(SCREEN_WIDTH - 10, 10, 255, 255, 255, TEXT_RIGHT, 'HIGHSCORE: ' + numberfill(HighScores[0].score))
  else drawText(SCREEN_WIDTH - 10, 10,   0, 255,   0, TEXT_RIGHT, 'HIGHSCORE: ' + numberfill(stage.score));
end;

procedure drawExplosions;
VAR e : PExplosion;
begin
  SDL_SetRenderDrawBlendMode(app.Renderer, SDL_Blendmode_ADD);
  SDL_SetTextureBlendMode(explode, SDL_Blendmode_ADD);
  e := stage.explosionHead^.next;
  while (e <> NIL) do
  begin
    SDL_SetTextureColorMod(explode, e^.r, e^.g, e^.b);
    SDL_SetTextureAlphaMod(explode, TRUNC(e^.a));
    blit(explode, e^.x, e^.y);
    e := e^.next;
  end;
  SDL_SetRenderDrawBlendMode(app.Renderer, SDL_BLENDMODE_NONE);
end;

procedure drawDebris;
VAR d : PDebris;
begin
  d := stage.debrisHead^.next;
  while (d <> NIL) do
  begin
    blitRect(d^.Texture, @d^.rect, d^.x, d^.y);
    d := d^.next;
  end;
end;

procedure drawBullets;
VAR b : PEntity;
begin
  b := stage.bulletHead^.next;
  while (b <> NIL) do
  begin
    blitAtlasImage(b^.Texture, b^.x, b^.y, 0);
    b := b^.next;
  end;
end;

procedure drawFighters;
VAR e : PEntity;
begin
  e := stage.fighterHead^.next;
  while (e <> NIL) do
  begin
    blitAtlasImage(e^.Texture, e^.x, e^.y, 0);
    e := e^.next;
  end;
end;

procedure drawPointsPods;
VAR p : PEntity;
begin
  p := stage.pointsHead^.next;
  while (p <> NIL) do
  begin
    if ((p^.health > (CFPS * 2)) OR ((p^.health MOD 12) < 6)) then
      blitAtlasImage(p^.Texture, p^.x, p^.y, 0);
    p := p^.next;
  end;
end;

procedure draw_Game;
begin
  drawBackground;
  drawStarfield;
  drawPointsPods;
  drawFighters;
  drawDebris;
  drawExplosions;
  drawBullets;
  drawHud;
end;

procedure addPointsPod(x, y : integer);
VAR e : PEntity;
begin
  NEW(e);
  initEntity(e);
  stage.pointsTail^.next := e;
  stage.pointsTail := e;
  e^.x := x;
  e^.y := y;
  e^.dx := -1 * (RANDOM(RAND_MAX) MOD 5);
  e^.dy := (RANDOM(RAND_MAX) MOD 5) - (RANDOM(RAND_MAX) MOD 5);
  e^.health := CFPS * POINTSPOD_TIME;
  e^.Texture := getAtlasImage('gfx/points.png');
  e^.w := e^.texture^.rec.w;
  e^.h := e^.texture^.rec.h;
  e^.x := e^.x - (e^.w DIV 2);
  e^.y := e^.y - (e^.h DIV 2);
end;

procedure addDebris(e : PEntity);
VAR d : PDebris;
    x, y, w, h : integer;
begin
  w := e^.texture^.rec.w DIV 2;
  h := e^.texture^.rec.h DIV 2;
  x := 0; y := 0;
  while y <= h do
  begin
    while x <= w do
    begin
      NEW(d);
      initDebris(d);
      stage.debrisTail^.next := d;
      stage.debrisTail := d;
      d^.x := e^.x + (e^.w DIV 2);
      d^.y := e^.y + (e^.h DIV 2);
      d^.dx := (RANDOM(RAND_MAX) MOD 5) - (RANDOM(RAND_MAX) MOD 5);
      d^.dy := -1 * (5 + (RANDOM(RAND_MAX) MOD 12));
      d^.life := CFPS * 2;
      d^.Texture := e^.Texture;
      d^.rect.x := e^.texture^.rec.x + x;
      d^.rect.y := e^.texture^.rec.y + y;
      d^.rect.w := w;
      d^.rect.h := h;
      INC(x, w);   { Inkrement of x-loop }
    end;           { end of while x loop }
    x := 0;        { reset for the y loop }
    INC(y, h);     { Inkrement of y-loop }
  end;
end;

procedure addExplosions(x, y: double; num : integer);
VAR e : PExplosion;
    i : integer;
begin
  for i := 0 to PRED(num) do
  begin
    NEW(e);
    initExplosion(e);
    stage.explosionTail^.next := e;
    stage.explosionTail := e;
    e^.x  := TRUNC(x) + (RANDOM(RAND_MAX) MOD 32) - (RANDOM(RAND_MAX) MOD 32);
    e^.y  := TRUNC(y) + (RANDOM(RAND_MAX) MOD 32) - (RANDOM(RAND_MAX) MOD 32);
    e^.dx :=            (RANDOM(RAND_MAX) MOD 10) - (RANDOM(RAND_MAX) MOD 10);
    e^.dy :=            (RANDOM(RAND_MAX) MOD 10) - (RANDOM(RAND_MAX) MOD 10);
    e^.dx := e^.dx / 10;
    e^.dy := e^.dy / 10;
    CASE (RANDOM(RAND_MAX) MOD 4) of
      0 : begin e^.r := 255; end;
      1 : begin e^.r := 255;
                e^.g := 128; end;
      2 : begin e^.r := 255;
                e^.g := 255; end;
    else  begin e^.r := 255;
                e^.g := 255;
                e^.b := 255; end;
    end;   { end of CASE }
    e^.a := (RANDOM(RAND_MAX) MOD (CFPS * 3));
  end;
end;

procedure doPointsPods;
VAR e, prev : PEntity;
begin
  prev := stage.pointsHead;
  e := stage.pointsHead^.next;
  while (e <> NIL) do
  begin
    if (e^.x < 0)                    then begin e^.x := 0;                    e^.dx := (-1 * e^.dx); end;
    if (e^.x + e^.w > SCREEN_WIDTH)  then begin e^.x := SCREEN_WIDTH - e^.w;  e^.dx := (-1 * e^.dx); end;
    if (e^.y < 0)                    then begin e^.y := 0;                    e^.dy := (-1 * e^.dy); end;
    if (e^.y + e^.h > SCREEN_HEIGHT) then begin e^.y := SCREEN_HEIGHT - e^.h; e^.dy := (-1 * e^.dy); end;
    e^.x := e^.x + (e^.dx * app.deltatime);
    e^.y := e^.y + (e^.dy * app.deltatime);
    e^.health := MAX(e^.health - app.deltatime, 0);
    if ((player <> NIL) AND collision(e^.x, e^.y, e^.w, e^.h, player^.x, player^.y, player^.w, player^.h)) then
    begin
      e^.health := 0;
      INC(stage.score);
      playSound(SND_POINTS, CH_POINTS);
    end;

    if (e^.health = 0) then
    begin
      if (e = stage.pointsTail) then
        stage.pointsTail := prev;
      prev^.next := e^.next;
      DISPOSE(e);
      e := prev;
    end;
    prev := e;
    e := e^.next;
  end;
end;

procedure doDebris;
VAR d, prev : PDebris;
begin
  prev := stage.debrisHead;
  d := stage.debrisHead^.next;
  while (d <> NIL) do
  begin
    d^.x := d^.x + (d^.dx * app.deltatime);
    d^.y := d^.y + (d^.dy * app.deltatime);
    d^.dy := d^.dy + (0.5 * app.deltatime);
    d^.life := MAX(d^.life - app.deltatime, 0);
    if (d^.life = 0) then
    begin
      if (d = stage.debrisTail) then
        stage.debrisTail := prev;
      prev^.next := d^.next;
      DISPOSE(d);
      d := prev;
    end;
    prev := d;
    d := d^.next;
  end;
end;

procedure doExplosions;
VAR e, prev : PExplosion;
begin
  prev := stage.ExplosionHead;
  e := stage.ExplosionHead^.next;
  while (e <> NIL) do
  begin
    e^.x := e^.x + (e^.dx * app.deltatime);
    e^.y := e^.y + (e^.dy * app.deltatime);
    e^.a := MAX(e^.a - app.deltatime, 0);
    if (e^.a = 0) then
    begin
      if (e = stage.ExplosionTail) then
        stage.ExplosionTail := prev;
      prev^.next := e^.next;
      DISPOSE(e);
      e := prev;
    end;
    prev := e;
    e := e^.next;
  end;
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
begin
  enemyspawnTimer := MAX(enemyspawnTimer - app.DeltaTime, 0);
  if enemyspawnTimer = 0 then
  begin
    NEW(enemy);
    initEntity(enemy);
    stage.fighterTail^.next := enemy;
    stage.fighterTail := enemy;

    enemy^.Texture := getAtlasImage('gfx/enemy.png');
    enemy^.w := enemy^.texture^.rec.w;
    enemy^.h := enemy^.texture^.rec.h;
    enemy^.x := SCREEN_WIDTH;
    enemy^.y := RANDOM(SCREEN_HEIGHT - enemy^.h);
    enemy^.dx := -1 * (2 + (RANDOM(RAND_MAX) MOD 4));
    enemy^.dy :=    -100 + (RANDOM(RAND_MAX) MOD 200);
    enemy^.dy := enemy^.dy / 100;
    enemy^.side := SIDE_ALIEN;
    enemy^.health := 1;
    enemy^.reload := CFPS * (1 + (RANDOM(RAND_MAX) MOD 3));
    enemyspawnTimer := 30 + (RANDOM(RAND_MAX) MOD CFPS);
  end;
end;

function bulletHitFighter(b : PEntity) : BOOLEAN;    { b = Bullet; f = Fighter }
VAR f : PEntity;
begin
  bulletHitFighter := FALSE;
  f := stage.fighterHead^.next;
  while (f <> NIL) do
  begin
    if (f^.side <> b^.side) then
    begin
      if (collision(b^.x, b^.y, b^.w, b^.h, f^.x, f^.y, f^.w, f^.h) = TRUE) then
      begin
        b^.health := 0;
        f^.health := 0;
        addExplosions(f^.x, f^.y, 32);
        addDebris(f);
        if (f = player) then
        begin
          playSound(SND_PLAYER_DIE, CH_PLAYER);
        end
        else
        begin
          addPointsPod(TRUNC(f^.x + (f^.w DIV 2)), TRUNC(f^.y + (f^.h DIV 2)));
          playSound(SND_ALIEN_DIE, CH_ANY);
        end;
        bulletHitFighter := TRUE;
      end;
    end;
    f := f^.next;
  end;
end;

procedure doBullets;
VAR b, prev : PEntity;
begin
  prev := stage.bulletHead;
  b := stage.bulletHead^.next;
  while (b <> NIL) do
  begin
    b^.x := b^.x + (b^.dx * app.deltatime);
    b^.y := b^.y + (b^.dy * app.deltatime);
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
    e^.x := e^.x + (e^.dx * app.deltatime);
    e^.y := e^.y + (e^.dy * app.deltatime);
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
begin
  NEW(bullet);
  initEntity(bullet);
  stage.bulletTail^.next := bullet;
  stage.bulletTail := bullet;
  bullet^.x := e^.x;
  bullet^.y := e^.y;
  bullet^.health := 1;
  bullet^.Texture := getAtlasImage('gfx/alienBullet.png');
  bullet^.w := bullet^.texture^.rec.w;
  bullet^.h := bullet^.texture^.rec.h;
  bullet^.x := bullet^.x + (e^.w DIV 2) - (bullet^.w DIV 2);
  bullet^.y := bullet^.y + (e^.h DIV 2) - (bullet^.h DIV 2);
  calcSlope(TRUNC(player^.x + (player^.w DIV 2)), TRUNC(player^.y + (player^.h DIV 2)), TRUNC(e^.x), TRUNC(e^.y), bullet^.dx, bullet^.dy);
  bullet^.dx := bullet^.dx * ALIEN_BULLET_SPEED;
  bullet^.dy := bullet^.dy * ALIEN_BULLET_SPEED;
  bullet^.side := SIDE_ALIEN;
end;

procedure doEnemies;
VAR e : PEntity;
begin
  e := stage.fighterHead^.next;
  while (e <> NIL) do
  begin
    if (e <> player) then
    begin
      e^.y := MIN(MAX(e^.y, 0), (SCREEN_HEIGHT - e^.h));
      e^.reload := MAX(e^.reload - app.deltatime, 0);
      if ((player <> NIL) AND (e^.reload = 0)) then
      begin
        fireAlienbullet(e);
        e^.reload := (RANDOM(RAND_MAX) MOD (CFPS * 2));
        playSound(SND_ALIEN_FIRE, CH_ALIEN_FIRE);
      end;
    end;
    e := e^.next;
  end;
end;

procedure fireBullet;
begin
  NEW(bullet);
  initEntity(bullet);
  stage.bulletTail^.next := bullet;
  stage.bulletTail := bullet;
  bullet^.x := player^.x;
  bullet^.y := player^.y;
  bullet^.dx := PLAYER_BULLET_SPEED;
  bullet^.health := 1;
  bullet^.Texture := getAtlasImage('gfx/playerBullet.png');
  bullet^.w := bullet^.texture^.rec.w;
  bullet^.h := bullet^.texture^.rec.h;
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
    //if (player^.reload > 0) then
    player^.reload := MAX(player^.reload - app.deltatime, 0);
    if (app.keyboard[SDL_ScanCode_UP]    OR app.keyboard[SDL_ScanCode_KP_8]) = 1 then player^.dy := (-1 * PLAYER_SPEED);
    if (app.keyboard[SDL_ScanCode_DOWN]  OR app.keyboard[SDL_ScanCode_KP_2]) = 1 then player^.dy :=       PLAYER_SPEED;
    if (app.keyboard[SDL_ScanCode_LEFT]  OR app.keyboard[SDL_ScanCode_KP_4]) = 1 then player^.dx := (-1 * PLAYER_SPEED);
    if (app.keyboard[SDL_ScanCode_RIGHT] OR app.keyboard[SDL_ScanCode_KP_6]) = 1 then player^.dx :=       PLAYER_SPEED;
    if ((app.keyboard[SDL_ScanCode_LCTRL] = 1) AND (player^.reload = 0)) then
      begin fireBullet; playSound(SND_PLAYER_FIRE, CH_PLAYER); end;
  end;
end;

//###################################################
procedure initHighScore; FORWARD;
procedure addHighScore(score : integer); FORWARD;
//###################################################

procedure logic_Game;
begin
  doBackGround;
  doStarfield;
  doPlayer;
  doEnemies;
  doFighters;
  doBullets;
  doExplosions;
  doDebris;
  doPointsPods;
  spawnEnemies;
  clipPlayer;
  if (player = NIL) then
  begin
    resetTimer := MAX(resetTimer - app.deltaTime, 0);
    if (resetTimer = 0) then
    begin
      addHighScore(stage.score);
      initHighScore;
    end;
  end;
end;

procedure initPlayer;
begin
  NEW(player);
  initEntity(player);
  stage.fighterTail^.next := player;
  stage.fighterTail := player;
  player^.health := 1;
  player^.x := 100;
  player^.y := 100;
  player^.Texture := getAtlasImage('gfx/player.png');
  player^.w := player^.texture^.rec.w;
  player^.h := player^.texture^.rec.h;
  player^.side := SIDE_PLAYER;
end;

procedure resetStage;
VAR e, t  : PEntity;
begin
  e := stage.fighterHead^.next;
  while (e <> NIL) do
  begin
    t := e^.next;
    DISPOSE(e);
    e := t;
  end;

  e := stage.bulletHead^.next;
  while (e <> NIL) do
  begin
    t := e^.next;
    DISPOSE(e);
    e := t;
  end;

  e := stage.pointsHead^.next;
  while (e <> NIL) do
  begin
    t := e^.next;
    DISPOSE(e);
    e := t;
  end;

  initEntity(stage.fighterHead);
  initEntity(stage.bulletHead);
  initEntity(stage.pointsHead);
  stage.fighterTail := stage.fighterHead;
  stage.bulletTail  := stage.bulletHead;
  stage.pointsTail  := stage.pointsHead;
end;

procedure resetLists;
VAR ex, u : PExplosion;
    d, v  : PDebris;
begin
  ex := stage.explosionHead^.next;
  while (ex <> NIL) do
  begin
    u := ex^.next;
    DISPOSE(ex);
    ex := u;
  end;

  d := stage.debrisHead^.next;
  while (d <> NIL) do
  begin
    v := d^.next;
    DISPOSE(d);
    d := v;
  end;

  stage.explosionTail := stage.explosionHead;
  stage.debrisTail  := stage.debrisHead;
end;

procedure initStage;
begin
  app.delegate.logic := @logic_Game;
  app.delegate.draw  := @draw_Game;
  explosionAtlas     := getAtlasImage('gfx/explosion.png');
  explode            := SDL_CreateTexture(app.Renderer,SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_TARGET, explosionAtlas^.rec.w, explosionAtlas^.rec.h);
  SDL_SetRenderTarget(app.Renderer, explode);
  SDL_RenderCopy(app.Renderer, explosionAtlas^.tex, @explosionAtlas^.rec, NIL);
  SDL_SetRenderTarget(app.Renderer, NIL);
  FillChar(app.keyboard, SizeOf(app.Keyboard), 0);     { empty keyboard puffer }
  resetStage;
  stage.score := 0;
  initPlayer;
  enemyspawnTimer := 0;
  resetTimer := CFPS * 3;
end;

//*****************  TITLE  **********************

{ procedure drawLogo;     // with *blitRect* to draw
VAR r : TSDL_Rect;
begin
  r.x := SDL2Texture^.rec.x;
  r.y := SDL2Texture^.rec.y;
  r.w := SDL2Texture^.rec.w;
//  r.h := SDL2Texture^.rec.h;
  r.h :=  MIN(reveal, r.w);
  blitrect(SDL2Texture, @r, (SCREEN_WIDTH DIV 2) - (r.w DIV 2), 100);

  r.x := ShooterTexture^.rec.x;
  r.y := ShooterTexture^.rec.y;
  r.w := ShooterTexture^.rec.w;
//  r.h := ShooterTexture^.rec.h;
  r.h := MIN(reveal, r.w);
  blitrect(shooterTexture, @r, (SCREEN_WIDTH DIV 2) - (r.w DIV 2), 250);
end; }

procedure drawLogo;     // with *blitAtlasImage* to draw
VAR r : TSDL_Rect;
begin
  r.w := SDL2Texture^.rec.w;
  SDL2Texture^.rec.h := MIN(TRUNC(reveal), r.w);
  blitAtlasImage(sdl2Texture, (SCREEN_WIDTH DIV 2) - (r.w DIV 2), 100, 0);

  r.w := ShooterTexture^.rec.w;
  ShooterTexture^.rec.h := MIN(TRUNC(reveal), r.w);
  blitAtlasImage(shooterTexture, (SCREEN_WIDTH DIV 2) - (r.w DIV 2), 250, 0);
end;

procedure draw_Title;
begin
  drawBackground;
  drawStarfield;
  drawLogo;
  if (timeout MOD 40) < 20 then
    drawText(SCREEN_WIDTH DIV 2, 600, 255, 255, 255, TEXT_CENTER, 'PRESS FIRE TO PLAY!');
end;

procedure logic_Title;
begin
  doBackGround;
  doStarfield;
  if (reveal < reveal_max) then
    reveal := reveal + app.deltatime;
  timeout := MAX(timeout - app.deltatime, 0);
  if timeout = 0 then
  begin
    initHighScore;
    reveal := 0;
  end;
  if (app.keyboard[SDL_ScanCode_LCTRL] = 1) then
    initStage;
end;

procedure initTitle;
begin
  app.delegate.logic := @logic_Title;
  app.delegate.draw  := @draw_Title;
  FillChar(app.keyboard, SizeOf(app.Keyboard), 0);     { empty keyboard puffer }
  SDL2Texture := getAtlasImage('gfx/sdl2.png');
  shooterTexture := getAtlasImage('gfx/shooter.png');
  reveal_max := ShooterTexture^.rec.h;
  reveal := 0;
  timeout := CFPS * 5;
end;

// ***************  HIGHSCORE  ****************

procedure emptyHighScore;
VAR i : integer;
begin
  for i := 0 to PRED(NUM_HighScores) do
  begin
    HighScores[i].score := NUM_HighScores - i;
    HighScores[i].name := 'ANONYMOUS';
  end;
end;

procedure readHighScore;
VAR i : integer;
    N, C : TJsonNode;
begin
  if FileExists(ScorePath) then
  begin
    N := TJsonNode.Create;
    N.LoadFromFile(ScorePath);

    if (N.Exists('Volume/sound')) then
    begin
      C := N.Find('Volume');
      SoundVol := C.Find('sound').asInteger;
      MusicVol := C.Find('music').AsInteger;
    end
    else
    begin
      SoundVol := -1;
      MusicVol := -1;
    end;

    C := N.Find('Highscore');
    for i := 0 to 7 do
    begin
      HighScores[i].name  := C.Child(i).Child(0).asString;
      HighScores[i].score := C.Child(i).Child(1).asInteger;
    end;
    N.Free;
  end
  else
  begin
    emptyHighScore;
    SoundVol := -1;
    MusicVol := -1;
  end;
end;

procedure writeHighScore;
VAR i : integer;
    N : TJsonNode;
begin
  N := TJsonNode.Create;
  if (SoundVol <> -1) OR (MusicVol <> -1) then
  begin
    N.Force('Volume').Add('sound',SoundVol);
    N.Force('Volume').Add('music',MusicVol);
  end;

  for i := 0 to PRED(NUM_HighScores) do
  begin
    N.Force('Highscore').Add.Add('name', HighScores[i].name).Parent.Add('score:', HighScores[i].score);
  end;
  N.SaveToFile(ScorePath);
  N.Free;
end;

{procedure writeHighScore;
VAR i : integer;
    N : TJsonNode;
begin
  N := TJsonNode.Create;
  for i := 0 to PRED(NUM_HighScores) do
  begin
    N.Force('Highscore').Add.Add('name', HighScores[i].name).Parent.Add('score:', HighScores[i].score);
  end;
  N.SaveToFile(ScorePath);
  N.Free;
end;}

procedure Order(VAR p, q : integer);
VAR temp : integer;
begin
  temp := p; p := q; q := temp;
end;

procedure Bubble(VAR B : TnewHighScoresARRAY; n : integer);
VAR i, j, min : integer;
begin
  for i := 0 to PRED(n) do
  begin
    min := i;
    for j := SUCC(i) to n do
    begin
      if (B[min].score < B[j].score) then min := j;
      if B[i].score < B[min].score then
      begin
        Order(B[i].score,  B[min].score);
        Order(B[i].recent, B[min].recent);
      end;
    end;
  end;
end;

procedure addHighScore(score : integer);
VAR newHighScores : TnewHighScoresARRAY;
    k : integer;
begin
  for k := 0 to PRED(NUM_HighScores) do
  begin
    newHighScores[k] := HighScores[k];
    newHighScores[k].recent := 0;
    if ((newHighScores[k].score = score) AND (newHighScores[k].name = 'ANONYMOUS')) then
      newHighScores[k].score := 0;   { erase the Anonymous - entry }
  end;
  newHighScores[NUM_HighScores].score := score;
  newHighScores[NUM_HighScores].recent := 1;

  Bubble(newHighScores, NUM_HighScores);

  for k := 0 to PRED(NUM_HighScores) do
  begin
    HighScores[k] := newHighScores[k];
    if (newHighScores[k].recent = 1) then
    begin
      HighScores[k].recent := 1;
      newHighScoreFlag := TRUE;
    end;
  end;
end;

procedure doNameInput;
VAR i, n, o : integer;
    c : char;
begin
  n := LENGTH(newHighScore.name);
  for i := 1 to LENGTH(app.inputText) do
  begin
    c := UPCASE(app.inputText[i]);
    if ((n < PRED(MAX_SCORE_NAME_LENGTH)) AND (c IN [' '..'Z'])) then
      newHighScore.name := newHighScore.name + c;
  end;
  if ((n > 0) AND (app.keyboard[SDL_SCANCODE_BACKSPACE] = 1)) then
  begin
    o := LENGTH(newHighScore.name);
    newHighScore.name := LEFTSTR(newHighScore.name, o - 1);
    app.Keyboard[SDL_SCANCODE_BACKSPACE] := 0;
  end;
  if (app.Keyboard[SDL_SCANCODE_RETURN] = 1) then
  begin
    if LENGTH(newHighScore.name) = 0 then
      newHighScore.name := 'ANONYMOUS';
    for i := 0 to PRED(NUM_HighScores) do
    begin
      if (HighScores[i].recent = 1) then
      begin
        HighScores[i].name := newHighScore.name;
        newHighScore.name := '';
      end;
    end;
    newHighScoreFlag := FALSE;
  end;
end;

procedure drawNameInput;
VAR r : TSDL_Rect;
begin
  drawText(SCREEN_WIDTH DIV 2,  70, 255, 255, 255, TEXT_CENTER, 'CONGRATULATIONS, YOU''VE GAINED A HIGHSCORE!');
  drawText(SCREEN_WIDTH DIV 2, 120, 255, 255, 255, TEXT_CENTER, 'ENTER YOUR NAME BELOW:');
  drawText(SCREEN_WIDTH DIV 2, 250, 128, 255, 128, TEXT_CENTER, newHighScore.name);
  if (cursorBlink < (CFPS DIV  2)) then
  begin
    r.x := ((SCREEN_WIDTH DIV 2) + (LENGTH(newHighScore.name) * GLYPH_WIDTH) DIV 2) + 5;
    r.y := 250;
    r.w := GLYPH_WIDTH;
    r.h := GLYPH_HEIGHT;
    SDL_SetRenderDrawColor(app.Renderer, 0 , 255, 0, 255);
    SDL_RenderFillRect(app.Renderer, @r);
  end;
  drawText(SCREEN_WIDTH DIV 2, 625, 255, 255, 255, TEXT_CENTER, 'PRESS RETURN WHEN FINISHED');
end;

procedure drawHighScores;
VAR i, y, r, g, b, o : integer;
  //p : TString16;
    a, Fmt : TString50;
begin
//p := ' ............';
  y := 150;
  drawText(SCREEN_WIDTH DIV 2, 70, 255, 255, 255, TEXT_CENTER, 'HIGHSCORES');
  for i := 0 to PRED(NUM_HighScores) do
  begin
    r := 255;
    g := 255;
    b := 255;
//    o := LENGTH(HighScores[i].name);
//    a := '#' + IntToStr(i + 1) + ' ' + HighScores[i].name  +
//         LEFTSTR(p, (MAX_SCORE_NAME_LENGTH - o)) + '..... ' + numberfill(HighScores[i].score);
    o := MAX_SCORE_NAME_LENGTH - LENGTH(HighScores[i].name) + 5;
    Fmt := '[%s%.d %s %-*.*s %.3d]';
    a := Format(Fmt, ['#',i + 1, HighScores[i].name, o, o, '....................',HighScores[i].score]);
    if HighScores[i].recent = 1 then
      b := 0;
    drawText(SCREEN_WIDTH DIV 2, y, r, g, b, TEXT_CENTER, a);
    INC(y, 50);
  end;
end;

// *******  HIGHSCORE / TITLE LOGIC  **********

procedure logic_HighSC;
begin
  doBackGround;
  doStarfield;
  if newHighScoreFlag = TRUE then
    doNameInput
  else
  begin
    timeout := MAX(timeout - app.deltatime, 0);
    if timeout = 0 then
      initTitle;
    if (app.keyboard[SDL_ScanCode_LCTRL] = 1) then
      initStage;
  end;
  cursorBlink := cursorBlink + app.deltatime;
  if cursorBlink >= CFPS then
    cursorBlink := 0;
end;

procedure draw_HighSC;
begin
  drawBackground;
  drawStarField;
  if newHighScoreFlag = TRUE then
    drawNameInput
  else
  begin
    drawHighScores;
    if ((timeout MOD 40) < 20) then
      drawText(SCREEN_WIDTH DIV 2, 600, 255, 255, 255, TEXT_CENTER, 'PRESS FIRE TO PLAY!');
  end;
end;

procedure initHighScore;
begin
  FillChar(app.keyboard, SizeOf(app.Keyboard), 0);     { empty keyboard puffer }
  app.delegate.logic := @logic_HighSC;
  app.delegate.draw  := @draw_HighSC;
  timeout := CFPS * 5;
end;

procedure initHighScoreTable;
begin
  FillChar(HighScores, SizeOf(THighScoreDef), 0);
  readHighScore;
  newHighScoreFlag := FALSE;
  cursorBlink := 0;
end;

// ***************   INIT SDL   ***************

procedure initSDL;
VAR rendererFlags, windowFlags : integer;
begin
  rendererFlags := SDL_RENDERER_ACCELERATED;
  windowFlags := 0;

  if SDL_Init(SDL_INIT_VIDEO OR SDL_INIT_AUDIO) < 0 then
    errorMessage(SDL_GetError());

  app.Window := SDL_CreateWindow('Shooter 15', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
  if app.Window = NIL then
    errorMessage(SDL_GetError());

  if MIX_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 1024) < 0 then
    errorMessage(SDL_GetError());
  Mix_AllocateChannels(MAX_SND_CHANNELS);

  SDL_SetHint(SDL_HINT_RENDER_BATCHING, '1');
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');
  app.Renderer := SDL_CreateRenderer(app.Window, -1, rendererFlags);
  if app.Renderer = NIL then
    errorMessage(SDL_GetError());

  IMG_INIT(IMG_INIT_PNG OR IMG_INIT_JPG);
  SDL_ShowCursor(0);
end;

procedure initGame;
begin
  app.inputText    := '';
  newHighScoreFlag := FALSE;
  exitLoop         := FALSE;
  music            := NIL;
  initStageListenPointer;
  initBackground;
  initStarfield;
  initSounds;
  initFonts;
  initHighScoreTable;
  loadMusic;
  playMusic(TRUE);
end;

procedure emptyArray;
VAR i : integer;
    c, b : PAtlasImage;
begin
  for i := 0 to NUMATLASBUCKETS do
  begin
    c := atlases[i]^.next;    // Dispose the list
    while (c <> NIL) do
    begin
      b := c^.next;
      DISPOSE(c);
      c := b;
    end;
    DISPOSE(atlases[i]);      // Dispose element / header of the array
  end;
end;

procedure cleanUp;
VAR i : byte;
begin
  SDL_DestroyTexture(atlasTex);
  SDL_DestroyTexture(explode);
  SDL_DestroyTexture(fontTexture);
  writeHighScore;
  emptyArray;
  resetStage;
  resetLists;
  if stage.fighterHead   <> NIL then DISPOSE(stage.fighterHead);
  if stage.bulletHead    <> NIL then DISPOSE(stage.bulletHead);
  if stage.explosionHead <> NIL then DISPOSE(stage.explosionHead);
  if stage.debrisHead    <> NIL then DISPOSE(stage.debrisHead);
  if stage.pointsHead    <> NIL then DISPOSE(stage.pointsHead);

  for i := 5 downto 1 do
    Mix_FreeChunk(sounds[i]);
  Mix_FreeMusic(music);
  if ExitCode <> 0 then WriteLn('CleanUp complete!');
end;

procedure AtExit;
begin
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

// *****************   Input  *****************

procedure doInput;
begin
  app.inputText := '';
  while SDL_PollEvent(@Event) = 1 do
  begin
    CASE Event.Type_ of

      SDL_QUITEV:          exitLoop := TRUE;        { close Window }
      SDL_MOUSEBUTTONDOWN: exitLoop := TRUE;        { if Mousebutton pressed }

      SDL_KEYDOWN: begin
                     if ((Event.key.repeat_ = 0) AND (Event.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                       app.keyboard[Event.key.keysym.scancode] := 1;
                     if (app.keyboard[SDL_ScanCode_ESCAPE]) = 1 then exitLoop := TRUE;
                   end;   { SDL_Keydown }

      SDL_KEYUP:   begin
                     if ((Event.key.repeat_ = 0) AND (Event.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                       app.keyboard[Event.key.keysym.scancode] := 0;
                   end;   { SDL_Keyup }
      SDL_TEXTINPUT: begin
                       app.inputText := app.inputText + Event.Text.text;
                     end;
    end;  { CASE Event }
  end;    { SDL_PollEvent }
end;

// *************   CAPFRAMERATE   *************

procedure logic1;
VAR tmpDelta : double;
begin
  // don't exceed target logic rate
  while (app.deltaTime > 1) do
  begin
    tmpDelta := app.deltaTime;
    app.deltaTime := 1;
    app.delegate.logic;
    app.deltaTime := (tmpDelta - 1);
  end;
  app.delegate.logic;
end;

procedure doFPS;
begin
  INC(FPS);
  if (SDL_GetTicks >= nextFPS) then
  begin
    app.dev.TFPS := FPS;
    FPS := 0;
    nextFPS := SDL_GetTicks + 1000;
  end;
end;

// *****************   MAIN   *****************

begin
  RANDOMIZE;
  pathTest;
  InitSDL;
  AddExitProc(@AtExit);
  initAtlas;
  initGame;
  initTitle;
  FPS := 0;
  app.deltatime := 0;
  app.dev.TFPS := 0;
  nextFPS := SDL_GetTicks + 1000;

  while exitLoop = FALSE do
  begin
    thentime := SDL_GetTicks;
    prepareScene;
    doInput;
    logic1;
    app.delegate.draw;
    presentScene;
    SDL_Delay(1);
    app.deltaTime := LOGIC_RATE * (SDL_GetTicks - thentime);
    doFPS;
  end;

  cleanUp;
  AtExit;
end.
