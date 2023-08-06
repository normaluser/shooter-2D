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
-quick & dirty Menu included, "ESC" show menu during game
-Highscore saved as Json-file
-without memory holes; testet with: fpc -Criot -gl -gh shooter18.pas
***************************************************************************}

PROGRAM Shooter18;
{$mode FPC} {$H+}    { "$H+" necessary for conversion of String to PChar !!; H+ => AnsiString }
{$COPERATORS OFF}
USES crt, SDL2, SDL2_Image, SDL2_Mixer, Math, JsonTools, sysutils;

CONST SCREEN_WIDTH  = 1280;            { size of the grafic window }
      SCREEN_HEIGHT = 720;             { size of the grafic window }
      PLAYER_SPEED  = 4.0;
      PLAYER_BULLET_SPEED = 20.0;
      ALIEN_BULLET_SPEED = 8.0;
      RAND_MAX = 3276;
      NUM_HighScores = 8;
      MAX_KEYBOARD_KEYS = 350;
      MAX_SCORE_NAME_LENGTH = 16;
      MAX_STRING_LENGTH = 50;
      SIDE_PLAYER = 0;
      SIDE_ALIEN = 1;
      FPS = 60;
      MAX_STARS = 500;
      MAX_Menu = 5;     { 4 Eintraege }

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
      ScorePath        = 'Highscore.json';

TYPE TDelegating = Procedure;               { "T" short for "TYPE" }
     TString16   = String[MAX_SCORE_NAME_LENGTH];
     TString50   = String[MAX_STRING_LENGTH];
     TDelegate   = RECORD
                     logic, draw : TDelegating;
                   end;
     PTextur     = ^TTexture;
     TTexture    = RECORD
                     name : String;
                     Texture : PSDL_Texture;
                     next : PTextur;
                   end;
     TApp        = RECORD
                     Window   : PSDL_Window;
                     Renderer : PSDL_Renderer;
                     keyboard : ARRAY[0..MAX_KEYBOARD_KEYS] OF integer;
                     textureHead, textureTail : PTextur;
                     inputText : String;
                     delegate : TDelegate;
                     r_delegate : TDelegate;      { "R_" = short for Recent Value }
                   end;
     PEntity     = ^TEntity;
     TEntity     = RECORD
                     x, y, dx, dy : double;
                     w, h, health, reload, side : integer;
                     Texture : PSDL_Texture;
                     next : PEntity;
                   end;
     PExplosion  = ^TExplosion;
     TExplosion  = RECORD
                     x, y, dx, dy : double;
                     r, g, b, a : integer;
                     next : PExplosion;
                   end;
     PDebris     = ^TDebris;
     TDebris     = RECORD
                     x, y, dx, dy : double;
                     rect : TSDL_Rect;
                     Texture : PSDL_Texture;
                     life : integer;
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
                     x, y, speed : integer;
                   end;
     TM_place    = RECORD
                     x, y, r, g, b : integer;
                     Text  : TString16;
                     HText : TString50;
                   end;

     THighScoreDef = RECORD
                       name : TString16;
                       recent, score : integer;
                     end;
     THighScoreARRAY =     ARRAY[0..PRED(NUM_HighScores)] OF THighScoreDef;
     TnewHighScoresARRAY = ARRAY[0..NUM_HighScores] OF THighScoreDef;

     TAlignment = (TEXT_LEFT, TEXT_CENTER, TEXT_RIGHT);

VAR app                  : TApp;
    stage                : TStage;
    player,
    enemy,
    bullet               : PEntity;
    fontTexture,
    pointsTexture,
    enemyTexture,
    bulletTexture,
    alienbulletTexture,
    playerTexture,
    SDL2Texture,
    shooterTexture,
    background,
    explosionTexture     : PSDL_Texture;
    Event                : TSDL_EVENT;
    newHighScoreFlag,
    bMenue,
    exitLoop             : BOOLEAN;
    gTicks               : UInt32;
    reveal,
    reveal_max,
    timeout,
    cursorBlink,
    backgroundX,
    enemyspawnTimer,
    resetTimer           : integer;
    PM                   : ARRAY[1..MAX_Menu + 1] of TM_place;
    stars                : ARRAY[0..MAX_STARS] OF TStar;
    sounds               : ARRAY[1..SND_MAX] OF PMix_Chunk;
    music                : PMix_Music;
    HighScores           : THighScoreARRAY;
    newHighScore         : THighScoreDef;
    SoundVol             : integer = 16;
    MusicVol             : integer = 32;
    gRemainder           : double = 0;
    Auswahl              : byte = 1;

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

procedure initTex(te : PTextur);
begin
  te^.name := '';
  te^.Texture := NIL;
  te^.next := NIL;
end;

procedure initStageListenPointer;
begin
  NEW(app.textureHead);
  NEW(stage.fighterHead);
  NEW(stage.bulletHead);
  NEW(stage.explosionHead);
  NEW(stage.debrisHead);
  NEW(stage.pointsHead);

  initTex(app.textureHead);
  initEntity(stage.fighterHead);
  initEntity(stage.bulletHead);
  initExplosion(stage.explosionHead);
  initDebris(stage.debrisHead);
  initEntity(stage.pointsHead);

  app.textureTail     := app.textureHead;
  stage.fighterTail := stage.fighterHead;
  stage.bulletTail  := stage.bulletHead;
  stage.explosionTail := stage.explosionHead;
  stage.debrisTail  := stage.debrisHead;
  stage.pointsTail  := stage.pointsHead;
end;

// *****************   UTIL   *****************

{function collision(x1, y1, w1, h1, x2, y2, w2, h2 : double) : BOOLEAN;
VAR a_Rect, b_Rect : TSDL_Rect;
begin
  collision := FALSE;
  a_Rect.x := ROUND(x1); a_Rect.y := ROUND(y1); a_Rect.w := ROUND(w1); a_Rect.h := ROUND(h1);
  b_Rect.x := ROUND(x2); b_Rect.y := ROUND(y2); b_Rect.w := ROUND(w2); b_Rect.h := ROUND(h2);
  if (SDL_HasIntersection(@a_Rect, @b_Rect) = SDL_TRUE) then collision := TRUE;
end;  }

function collision(x1, y1, w1, h1, x2, y2, w2, h2 : double) : Boolean;
begin
  collision := (MAX(x1, x2) < MIN(x1 + w1, x2 + w2)) AND (MAX(y1, y2) < MIN(y1 + h1, y2 + h2));
end;

procedure calcSlope(x1, y1, x2, y2 : double; VAR dx, dy : double); INLINE;
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

procedure errorMessage(Message1 : String);
begin
  SDL_ShowSimpleMessageBox(SDL_MessageBOX_ERROR,'Error Box',PChar(Message1),NIL);
  HALT(1);
end;

procedure logMessage(Message1 : string);
VAR Fmt : PChar;
begin
  Fmt := 'File not found: %s'#13;    // Formatstring und "ARRAY of const" als Parameteruebergabe in [ ]
  SDL_LogMessage(SDL_LOG_CATEGORY_APPLICATION, SDL_LOG_PRIORITY_WARN, Fmt, [PChar(Message1)]);
end;

// *****************   SOUND  *****************

procedure loadSounds;
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

  Mix_VolumeChunk(sounds[1], SoundVol);          {Orginal: MIX_MAX_VOLUME = 128 !!!}
  Mix_VolumeChunk(sounds[2], SoundVol);
  Mix_VolumeChunk(sounds[3], SoundVol);
  Mix_VolumeChunk(sounds[4], SoundVol);
  Mix_VolumeChunk(sounds[5], SoundVol);
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
  Mix_VolumeMusic(MusicVol);                     {Orginal: MIX_MAX_VOLUME = 128 !!!}
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

procedure blit(Texture : PSDL_Texture; x, y : double); INLINE;
VAR dest : TSDL_Rect;
begin
  dest.x := TRUNC(x);
  dest.y := TRUNC(y);
  SDL_QueryTexture(Texture, NIL, NIL, @dest.w, @dest.h);
  SDL_RenderCopy(app.Renderer, Texture, NIL, @dest);
end;

procedure blitRect(Texture : PSDL_Texture; src : PSDL_Rect; x, y : double); INLINE;
VAR dest : TSDL_Rect;
begin
  dest.x := TRUNC(x);
  dest.y := TRUNC(y);
  dest.w := src^.w;
  dest.h := src^.h;
  SDL_RenderCopy(app.Renderer, Texture, src, @dest);
end;

procedure addTextureToCache(Lname : String; LTexture : PSDL_Texture);
VAR cache : PTextur;
begin
  NEW(cache);
  initTex(cache);
  app.textureTail^.next := cache;
  app.textureTail := cache;
  cache^.name := Lname;
  cache^.Texture := LTexture;
end;

function getTexture(name : String) : PSDL_Texture;
VAR tg : PTextur;
begin
  getTexture := NIL;
  tg := app.textureHead^.next;
  while (tg <> NIL) do
  begin
    if (tg^.name = name)
    //if compareText(tg^.name, name) = 0
      then getTexture := tg^.Texture;
    tg := tg^.next;
  end;
end;

function loadTexture(Pfad : String) : PSDL_Texture;
VAR tl : PSDL_Texture;
    Fmt : PChar;
begin
  tl := getTexture(Pfad);
  if tl = NIL then
  begin
    tl := IMG_LoadTexture(app.Renderer, PChar(Pfad));
    if tl = NIL then errorMessage(SDL_GetError());
    addTextureToCache(Pfad, tl);
  end;
  Fmt := 'Loading %s'#13;
  SDL_LogMessage(SDL_LOG_CATEGORY_APPLICATION, SDL_LOG_PRIORITY_INFO,  Fmt, [PChar(Pfad)]);
  loadTexture := tl;
end;

procedure prepareScene; INLINE;
begin
  SDL_SetRenderDrawColor(app.Renderer, 32, 32, 32, 255);
  SDL_RenderClear(app.Renderer);
end;

procedure presentScene; INLINE;
begin
  SDL_RenderPresent(app.Renderer);
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
      blitRect(fontTexture, @rect, x, y);
      INC(x, GLYPH_WIDTH);
    end;
  end;
end;

function numberfill(a : integer) : TString50;
VAR FMT : TString16;
begin
  Fmt := '[%.3d]';                  { Fmt: arguments for Format }
  numberfill := Format(Fmt, [a]);   { Format: format a String with given arguments (=> Fmt) }
end;

procedure initFonts;
begin
  fontTexture := loadTexture('gfx/font.png');
end;

// **************   Background  ***************

procedure drawBackground; INLINE;
VAR dest : TSDL_Rect;
    x : integer;
begin
  x := backgroundX;
  while x < SCREEN_WIDTH do
  begin
    dest.x := x;
    dest.y := 0;
    dest.w := SCREEN_WIDTH;
    dest.h := SCREEN_HEIGHT;
    SDL_RenderCopy(app.Renderer, background, NIL, @dest);
    INC(x, SCREEN_WIDTH);
  end;
end;

procedure drawStarfield; INLINE;
VAR i, c : integer;
begin
  for i := 0 to PRED(MAX_STARS) do
  begin
    c := 32 * stars[i].speed;
    if c > 255 then c := 255;
    SDL_SetRenderDrawColor(app.Renderer, c, c, c, 255);
    SDL_RenderDrawLine(app.Renderer, stars[i].x, stars[i].y, stars[i].x + 3, stars[i].y);
  end;
end;

procedure doStarField; INLINE;
VAR i : integer;
begin
  for i := 0 to PRED(MAX_STARS) do
  begin
    DEC(stars[i].x, stars[i].speed);
    if stars[i].x < 0 then
      INC(stars[i].x, SCREEN_WIDTH);
  end;
end;

procedure doBackGround; INLINE;
begin
  DEC(backgroundX);
  if backgroundX < (-SCREEN_WIDTH) then
    backgroundX := 0;
end;

procedure initStarfield; INLINE;
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
  background := loadTexture('gfx/background.png');
  backgroundX := 0;
end;

// *****************   Stage  *****************

procedure drawHud; INLINE;
begin
  drawText(10, 10, 255, 255, 255, TEXT_LEFT, 'SCORE: ' + numberfill(stage.score));
  if ((stage.score <  HighScores[0].score))
  then drawText(SCREEN_WIDTH - 10, 10, 255, 255, 255, TEXT_RIGHT, 'MAX HIGHSCORE: ' + numberfill(HighScores[0].score))
  else drawText(SCREEN_WIDTH - 10, 10,   0, 255,   0, TEXT_RIGHT, 'NEW HIGHSCORE: ' + numberfill(stage.score));
end;

procedure drawExplosions;
VAR e : PExplosion;
begin
  SDL_SetRenderDrawBlendMode(app.Renderer, SDL_Blendmode_ADD);
  SDL_SetTextureBlendMode(explosionTexture, SDL_Blendmode_ADD);
  e := stage.explosionHead^.next;
  while (e <> NIL) do
  begin
    SDL_SetTextureColorMod(explosionTexture, e^.r, e^.g, e^.b);
    SDL_SetTextureAlphaMod(explosionTexture, e^.a);
    blit(explosionTexture, e^.x, e^.y);
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

procedure drawBullets; INLINE;
VAR b : PEntity;
begin
  b := stage.bulletHead^.next;
  while (b <> NIL) do
  begin
    blit(b^.Texture, b^.x, b^.y);
    b := b^.next;
  end;
end;

procedure drawFighters; INLINE;
VAR e : PEntity;
begin
  e := stage.fighterHead^.next;
  while (e <> NIL) do
  begin
    blit(e^.Texture, e^.x, e^.y);
    e := e^.next;
  end;
end;

procedure drawPointsPods; INLINE;
VAR p : PEntity;
begin
  p := stage.pointsHead^.next;
  while (p <> NIL) do
  begin
    if ((p^.health > (FPS * 2)) OR ((p^.health MOD 12) < 6)) then
      blit(p^.Texture, p^.x, p^.y);
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
    dest : TSDL_Rect;
begin
  NEW(e);
  initEntity(e);
  stage.pointsTail^.next := e;
  stage.pointsTail := e;
  e^.x := x;
  e^.y := y;
  e^.dx := -1 * ((RANDOM(RAND_MAX) MOD 5) - 0.1);
  e^.dy := (RANDOM(RAND_MAX) MOD 5) - (RANDOM(RAND_MAX) MOD 5);
  e^.health := FPS * 10;
  e^.Texture := pointsTexture;
  SDL_QueryTexture(e^.Texture, NIL, NIL, @dest.w, @dest.h);
  e^.w := dest.w;
  e^.h := dest.h;
  e^.x := e^.x - (e^.w DIV 2);
  e^.y := e^.y - (e^.h DIV 2);
end;

procedure addDebris(e : PEntity);
VAR d : PDebris;
    x, y, w, h : integer;
begin
  w := e^.w DIV 2;
  h := e^.h DIV 2;
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
      d^.life := FPS * 2;
      d^.Texture := e^.Texture;
      d^.rect.x := x;
      d^.rect.y := y;
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
    e^.x  := x + (RANDOM(RAND_MAX) MOD 32) - (RANDOM(RAND_MAX) MOD 32);
    e^.y  := y + (RANDOM(RAND_MAX) MOD 32) - (RANDOM(RAND_MAX) MOD 32);
    e^.dx :=     (RANDOM(RAND_MAX) MOD 10) - (RANDOM(RAND_MAX) MOD 10);
    e^.dy :=     (RANDOM(RAND_MAX) MOD 10) - (RANDOM(RAND_MAX) MOD 10);
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
    e^.a := (RANDOM(RAND_MAX) MOD (FPS * 3));
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
    e^.x := e^.x + e^.dx;
    e^.y := e^.y + e^.dy;
    DEC(e^.health);
    if ((player <> NIL) AND collision(e^.x, e^.y, e^.w, e^.h, player^.x, player^.y, player^.w, player^.h)) then
    begin
      e^.health := 0;
      INC(stage.score);
      playSound(SND_POINTS, CH_POINTS);
    end;
    if (e^.health <= 0) then
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
    d^.x := d^.x + d^.dx;
    d^.y := d^.y + d^.dy;
    d^.dy := d^.dy + 0.5;
    DEC(d^.life);
    if (d^.life <= 0) then
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
    e^.x := e^.x + e^.dx;
    e^.y := e^.y + e^.dy;
    DEC(e^.a);
    if (e^.a <= 0) then
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
    enemy^.dy := -100 + (RANDOM(RAND_MAX) MOD 200);
    enemy^.dy := enemy^.dy / 100;
    enemy^.side := SIDE_ALIEN;
    enemy^.health := 1;
    enemy^.reload := FPS * (1 + (RANDOM(RAND_MAX) MOD 3));
    enemyspawnTimer := 30 + (RANDOM(RAND_MAX) MOD FPS);
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
  SDL_QueryTexture(bullet^.Texture, NIL, NIL, @dest.w, @dest.h);
  bullet^.w := dest.w;
  bullet^.h := dest.h;
  bullet^.x := bullet^.x + (e^.w DIV 2) - (bullet^.w DIV 2);
  bullet^.y := bullet^.y + (e^.h DIV 2) - (bullet^.h DIV 2);
  calcSlope(player^.x + (player^.w DIV 2), player^.y + (player^.h DIV 2), e^.x, e^.y, bullet^.dx, bullet^.dy);
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
      if e^.y <= 0 then e^.dy := -1 * e^.dy;
      if e^.y >= (SCREEN_HEIGHT - e^.h) then e^.dy := -1 * e^.dy;
      DEC(e^.reload);
      if ((player <> NIL) AND (e^.reload <= 0)) then
      begin
        fireAlienbullet(e);
        e^.reload := (RANDOM(RAND_MAX) MOD (FPS * 2));
        playSound(SND_ALIEN_FIRE, CH_ALIEN_FIRE);
      end;
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
    if ((app.keyboard[SDL_ScanCode_LCTRL] = 1) AND (player^.reload <= 0)) then
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
    DEC(resetTimer);
    if (resetTimer <= 0) then
    begin
      addHighScore(stage.score);
      initHighScore;
    end;
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
  bulletTexture      := loadTexture('gfx/playerBullet.png');
  enemyTexture       := loadTexture('gfx/enemy.png');
  alienbulletTexture := loadTexture('gfx/alienBullet.png');
  playerTexture      := loadTexture('gfx/player.png');
  explosionTexture   := loadTexture('gfx/explosion.png');
  pointsTexture      := loadTexture('gfx/points.png');
  FillChar(app.keyboard, SizeOf(app.Keyboard), 0);     { empty keyboard puffer }
  resetStage;
  stage.score := 0;
  initPlayer;
  enemyspawnTimer := 0;
  resetTimer := FPS * 3;
end;

//*****************  TITLE  **********************

procedure draw_Title;
VAR r : TSDL_Rect;
begin
  drawBackground;
  drawStarfield;
  r.x := 0;
  r.y := 0;
  SDL_QueryTexture(SDL2Texture, NIL, NIL, @r.w, @r.h);
  r.h := MIN(reveal, r.h);
  blitRect(SDL2Texture, @r, (SCREEN_WIDTH DIV 2) - (r.w DIV 2), 100);
  SDL_QueryTexture(shooterTexture, NIL, NIL, @r.w, @r.h);
  r.h := MIN(reveal, r.h);
  blitRect(shooterTexture, @r, (SCREEN_WIDTH DIV 2) - (r.w DIV 2), 250);
  if (timeout MOD 40) < 20 then
    drawText(SCREEN_WIDTH DIV 2, 600, 255, 255, 255, TEXT_CENTER, 'PRESS FIRE TO PLAY!');
end;

procedure logic_Title;
begin
  doBackGround;
  doStarfield;
  if (reveal < reveal_max) then
    INC(reveal);
  DEC(timeout);
  if timeout <= 0 then
  begin
    initHighScore;
    reveal := 0;
  end;
  if (app.keyboard[SDL_ScanCode_LCTRL] = 1) then
    initStage;
end;

procedure initTitle;
VAR r : TSDL_Rect;
begin
  app.delegate.logic := @logic_Title;
  app.delegate.draw  := @draw_Title;
  FillChar(app.keyboard, SizeOf(app.Keyboard), 0);     { empty keyboard puffer }
  SDL2Texture := loadTexture('gfx/sdl2.png');
  shooterTexture := loadTexture('gfx/shooter.png');
  SDL_QueryTexture(SDL2Texture, NIL, NIL, @r.w, @r.h);
  reveal_max := r.h;
  reveal := 0;
  timeout := FPS * 5;
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
    if (N.Child(0).name = 'Volume') then
    begin
      soundvol := N.Find('Volume/sound').asinteger;
      musicvol := N.Find('Volume/music').asinteger;
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
  end;
end;

procedure writeHighScore;
VAR i : integer;
    N : TJsonNode;
begin
  N := TJsonNode.Create;
  N.Force('Volume').Add('sound',SoundVol);
  N.Force('Volume').Add('music',MusicVol);
  for i := 0 to PRED(NUM_HighScores) do
  begin
    N.Force('Highscore').Add.Add('name', HighScores[i].name).Parent.Add('score:', HighScores[i].score);
  end;
  N.SaveToFile(ScorePath);
  N.Free;
end;

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
  if (cursorBlink < (FPS DIV  2)) then
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
    a, Fmt : TString50;
begin
  y := 150;
  drawText(SCREEN_WIDTH DIV 2, 70, 255, 255, 255, TEXT_CENTER, 'HIGHSCORES');
  for i := 0 to PRED(NUM_HighScores) do
  begin
    r := 255;
    g := 255;
    b := 255;
    o := MAX_SCORE_NAME_LENGTH - LENGTH(HighScores[i].name) + 5;
    Fmt := '[%s%.d %s %-*.*s %.3d]';
    a := Format(fmt, ['#',i + 1, HighScores[i].name, o, o, '....................',HighScores[i].score]);

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
    DEC(timeout);
    if timeout <= 0 then
      initTitle;
    if (app.keyboard[SDL_ScanCode_LCTRL] = 1) then
      initStage;
  { if (app.keyboard[SDL_ScanCode_DELETE] = 1) then
       emptyHighScore;  }
  end;
  INC(cursorBlink);
  if cursorBlink >= FPS then
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
   {  drawText(SCREEN_WIDTH DIV 2, 650, 255, 255, 255, TEXT_CENTER, 'PRESS DEL TO RESET HIGHSCORE!');  }
  end;
end;

procedure initHighScore;
begin
  FillChar(app.keyboard, SizeOf(app.Keyboard), 0);     { empty keyboard puffer }
  app.delegate.logic := @logic_HighSC;
  app.delegate.draw  := @draw_HighSC;
  timeout := FPS * 5;
end;

procedure initHighScoreTable;
begin
  readHighScore;
  newHighScoreFlag := FALSE;
  cursorBlink := 0;
end;

// *****************   MENU   *****************

procedure logic_Menue;
VAR rep : boolean;
begin
  doBackGround;
  doStarfield;
  if (app.keyboard[SDL_ScanCode_UP]   = 1) then begin DEC(Auswahl); rep := FALSE; end;
  if (app.keyboard[SDL_ScanCode_DOWN] = 1) then begin INC(Auswahl); rep := FALSE; end;
  if Auswahl < 1 then Auswahl := MAX_Menu;
  if Auswahl > Max_Menu then Auswahl := 1;

  if ((app.keyboard[SDL_ScanCode_LEFT]   = 1) AND (Auswahl = 1)) then begin DEC(SoundVol, 1); rep := TRUE;  end;     { SoundVolume }
  if ((app.keyboard[SDL_ScanCode_RIGHT]  = 1) AND (Auswahl = 1)) then begin INC(SoundVol, 1); rep := TRUE;  end;
  if ((app.keyboard[SDL_ScanCode_LEFT]   = 1) AND (Auswahl = 2)) then begin DEC(MusicVol, 4); rep := FALSE; end;     { MusicVolume }
  if ((app.keyboard[SDL_ScanCode_RIGHT]  = 1) AND (Auswahl = 2)) then begin INC(MusicVol, 4); rep := FALSE; end;

  writeHighScore;    { save the Volume }

  if ((app.keyboard[SDL_ScanCode_DELETE] = 1) AND (Auswahl = 3)) then   { DELETE Highscore }
  begin
    Auswahl := 1;                                                       { Menue set to 1 }
    timeout := FPS * 5;                                                 { reset the timer for to show Highscore }
    bMenue := FALSE;                                                    { no Menue active now }
    emptyHighScore;                                                     { delete Highscore }
    app.delegate.logic := @logic_Highsc;                                { switch to Highscore logic }
    app.delegate.draw  := @draw_Highsc;                                 { switch to Highscore draw  }
  end;
  if (((app.keyboard[SDL_ScanCode_RETURN] = 1)
    OR (app.keyboard[SDL_ScanCode_SPACE] = 1)) AND (Auswahl = 4)) then  { leave the Menu }
  begin
    Auswahl := 1;                                                       { Menue set to 1 }
    bMenue := FALSE;                                                    { no Menue active now }
    app.delegate.logic := app.r_delegate.logic;                         { reset the old state of Logic }
    app.delegate.draw  := app.r_delegate.draw;                          { reset the old state of Draw }
  end;
  if (((app.keyboard[SDL_ScanCode_RETURN] = 1)
    OR (app.keyboard[SDL_ScanCode_SPACE] = 1)) AND (Auswahl = MAX_Menu)) then { EXIT the Game }
    exitloop := true;

  if (rep = FALSE) then FillChar(app.keyboard, SizeOf(app.Keyboard), 0); { empty keyboard puffer }

  SoundVol := MIN(MAX(SoundVol, 0), MIX_MAX_VOLUME);                    { MIX_MAX_VOLUME = 128 !!! }
  MusicVol := MIN(MAX(MusicVol, 0), MIX_MAX_VOLUME);                    { MIX_MAX_VOLUME = 128 !!! }
  Mix_VolumeChunk(sounds[1], SoundVol);
  Mix_VolumeChunk(sounds[2], SoundVol);
  Mix_VolumeChunk(sounds[3], SoundVol);
  Mix_VolumeChunk(sounds[4], SoundVol);
  Mix_VolumeChunk(sounds[5], SoundVol);
  Mix_VolumeMusic(MusicVol);
end;

procedure draw_Bar(a : TSDL_Rect; wwith, vol, max : integer);
begin
  a.w := round((wwith - 4) * vol DIV max);                              { Dreisatz zur Balkenbreite }
  SDL_SetRenderDrawColor(app.renderer, 0, 255, 0, 255);                 { gruen }
  SDL_RenderFillRect(app.renderer, @a);                                 { Volume }
  a.x := a.x - 3;   a.y := a.y - 3;
  a.w := wwith + 2; a.h := a.h + 6;
  SDL_SetRenderDrawColor(app.renderer, 255, 255, 255, 255);             { weiss }
  SDL_RenderDrawRect(app.renderer, @a);                                 { Umrandung }
end;

procedure draw_Menue;
VAR i, k : byte;
    r : TSDL_Rect;
begin
  k := Max_Menu + 1;
  PM[1].x := 650;                  PM[1].y := 200;                                      { Menupoint 1 }
  PM[2].x := PM[1].x;              PM[2].y := 280;                                      { Menupoint 2 }
  PM[3].x := PM[1].x;              PM[3].y := 360;                                      { Menupoint 3 }
  PM[4].x := PM[1].x;              PM[4].y := 440;                                      { Menupoint 4 }
  PM[5].x := PM[1].x;              PM[5].y := 520;                                      { Menupoint 5 }
  PM[k].x := SCREEN_WIDTH DIV 2;   PM[k].y := SCREEN_HEIGHT - 50;                       { Hilfstext }
  PM[1].Text := 'SOUND VOLUME:';   PM[1].HText := 'PRESS ARROW-KEYS TO CHANGE SOUND VOLUME!';
  PM[2].Text := 'MUSIC VOLUME:';   PM[2].HText := 'PRESS ARROW-KEYS TO CHANGE MUSIC VOLUME!';
  PM[3].Text := 'RESET HIGHSCORE'; PM[3].HText := 'PRESS DEL TO RESET THE HIGHSCORE';
  PM[4].Text := 'BACK TO GAME!';   PM[4].HText := 'PRESS SPACE OR ENTER TO PLAY GAME!';
  PM[5].Text := 'QUIT THE GAME!';  PM[5].HText := 'PRESS SPACE OR ENTER TO QUIT GAME!';

  drawBackGround;
  drawStarfield;
  for i := 1 to Max_Menu do                                                             { schreibe Menue }
  begin
    if i = Auswahl then
    begin
      drawText(PM[i].x - 170, PM[i].y, 0, 255, 0, TEXT_CENTER, '>');                    { gruener Cursor }
      drawText(PM[i].x - 155, PM[i].y, 0, 255, 0, TEXT_LEFT, PM[i].Text);               { gruener Text }
    end
    else
      drawText(PM[i].x - 155, PM[i].y, 255, 255, 255, TEXT_LEFT, PM[i].Text);           { weisser Text }
  end;
  drawText(PM[k].x, PM[k].y, 255, 255, 255, TEXT_CENTER, PM[Auswahl].HText);            { Hilfstext Anzeige }

  r.x := PM[1].x + 90; r.y := 190; r.w := 260; r.h :=  40;                              { Balkenanzeige }
  draw_Bar(r, 284, SoundVol, MIX_MAX_VOLUME);
  r.x := PM[1].x + 90; r.y := 270; r.w := 260; r.h :=  40;
  draw_Bar(r, 284, MusicVol, MIX_MAX_VOLUME);

  drawText(PM[1].x + 240, PM[1].y, 255, 255, 255, TEXT_CENTER, NumberFill(SoundVol*100 div 128));   { Sound Volumen in % }
  drawText(PM[2].x + 240, PM[2].y, 255, 255, 255, TEXT_CENTER, NumberFill(MusicVol*100 div 128));   { Music Volumen in % }
  drawText(PM[1].x + 260, PM[1].y, 255, 255, 255, TEXT_LEFT, '%');
  drawText(PM[2].x + 260, PM[2].y, 255, 255, 255, TEXT_LEFT, '%');
end;

// ***************   INIT SDL   ***************

procedure initSDL;
VAR rendererFlags, windowFlags : integer;
begin
  rendererFlags := {SDL_RENDERER_PRESENTVSYNC OR} SDL_RENDERER_ACCELERATED;
  windowFlags := 0;

  if SDL_Init(SDL_INIT_VIDEO OR SDL_INIT_AUDIO) < 0 then
    errorMessage(SDL_GetError());

  app.Window := SDL_CreateWindow('Shooter 18', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
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

procedure initGame;
begin
  app.inputText    := '';
  newHighScoreFlag := FALSE;
  exitLoop         := FALSE;
  bMenue           := FALSE;
  gTicks           := SDL_GetTicks;
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

procedure destroyTexture;
VAR e, tex : PTextur;
begin
  tex := app.textureHead^.next;
  while (tex <> NIL) do
  begin
    e := tex^.next;
    DISPOSE(tex);
    tex := e;
  end;
end;

procedure cleanUp;
VAR i : byte;
begin
  writeHighScore;
  resetStage;
  resetLists;
  if stage.fighterHead   <> NIL then DISPOSE(stage.fighterHead);
  if stage.bulletHead    <> NIL then DISPOSE(stage.bulletHead);
  if stage.explosionHead <> NIL then DISPOSE(stage.explosionHead);
  if stage.debrisHead    <> NIL then DISPOSE(stage.debrisHead);
  if stage.pointsHead    <> NIL then DISPOSE(stage.pointsHead);
  destroyTexture;
  if app.textureHead     <> NIL then DISPOSE(app.textureHead);

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

// *************   DELEGATE LOGIC   ***********

{procedure delegate_logic(Wahl : TDelegating);
begin
  CASE Wahl of
  Logo : begin
           logic_Title;
           draw_Title;
         end;
  HighSC : begin
             logic_HighSC;
             draw_HighSC;
           end;
  Menues : begin
             logic_Menue;
             draw_Menue;
           end;
  Game : begin
           logic_Game;
           draw_Game;
         end;
  end;
end;}

// *****************   Input  *****************

procedure doInput;
begin
  app.inputText := '';
  while SDL_PollEvent(@Event) = 1 do
  begin
    CASE Event.Type_ of

      SDL_QUITEV:          exitLoop := TRUE;                        { close Window }
      SDL_MOUSEBUTTONDOWN: exitLoop := TRUE;                        { if Mousebutton pressed }

      SDL_KEYDOWN: begin
                     if ((Event.key.repeat_ = 0) AND (Event.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                       app.keyboard[Event.key.keysym.scancode] := 1;
                     if ((app.keyboard[SDL_ScanCode_ESCAPE] = 1) AND (bMenue = FALSE)) then
                     begin
                       app.r_delegate.logic := app.delegate.logic;  { save the old state }
                       app.r_delegate.draw  := app.delegate.draw;   { save the old state }
                       app.delegate.logic := @logic_Menue;          { switch to menue }
                       app.delegate.draw  := @draw_Menue;           { switch to menue }
                       bMenue := TRUE;                              { menue is active now }
                     end;
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

procedure CapFrameRate(VAR remainder : double; VAR Ticks : UInt32); INLINE;
VAR wait, FrameTime : longInt;
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
  RANDOMIZE;
  InitSDL;
  AddExitProc(@AtExit);
  initGame;
  initTitle;

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
