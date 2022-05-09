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
*** Title screen and finishing touches
***************************************************************************}

PROGRAM Shooter15;
{$mode FPC} {$H+}    { "$H+" necessary for conversion of String to PChar !!; H+ => Ansistring }
{$COPERATORS OFF}
USES CRT, SDL2, SDL2_Image, SDL2_Mixer, Math, sysutils;

CONST SCREEN_WIDTH  = 1280;            { size of the grafic window }
      SCREEN_HEIGHT = 720;             { size of the grafic window }
      PLAYER_SPEED  = 4.0;
      PLAYER_BULLET_SPEED = 20.0;
      ALIEN_BULLET_SPEED = 8.0;
      POINTSPOD_TIME = 10;
      RAND_MAX = 3276;
      NUM_HighScores = 8;
      MAX_KEYBOARD_KEYS = 350;
      MAX_SCORE_NAME_LENGTH = 16;
      MAX_STRING_LENGTH = 50;
      SIDE_PLAYER = 0;
      SIDE_ALIEN = 1;
      FPS = 60;
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

TYPE                                        { "T" short for "TYPE" }
     TSring16    = String[MAX_SCORE_NAME_LENGTH];
     TSring50    = String[MAX_STRING_LENGTH];

     TDelegating = (Logo, Highsc, Game);
     TDelegate   = RECORD
                     logic, draw : TDelegating;
                   end;
     PTextur     = ^TTexture;
     TTexture    = RECORD
                     name : string;
                     Texture : PSDL_Texture;
                     next : PTextur;
                   end;
     TApp        = RECORD
                     Window   : PSDL_Window;
                     Renderer : PSDL_Renderer;
                     keyboard : Array[0..MAX_KEYBOARD_KEYS] OF integer;
                     textureHead, textureTail : PTextur;
                     inputText : String;
                     Delegate : TDelegate;
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
     THighScoreDef = RECORD
                       name : TSring16;
                       recent, score : integer;
                     end;
     THighScoreArray =     Array[0..PRED(NUM_HighScores)] OF THighScoreDef;
     TnewHighScoresArray = Array[0..NUM_HighScores] OF THighScoreDef;

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
    exitLoop             : BOOLEAN;
    gTicks               : UInt32;
    gRemainder           : double;
    reveal,
    reveal_max,
    timeout,
    cursorBlink,
    backgroundX,
    enemyspawnTimer,
    resetTimer           : integer;
    stars                : Array[0..MAX_STARS] OF TStar;
    music                : PMix_Music;
    sounds               : Array[1..SND_MAX] OF PMix_Chunk;
    HighScores           : THighScoreArray;
    newHighScore         : THighScoreDef;

// *****************   INIT   *****************

procedure initEntity(VAR e : PEntity);
begin
  e^.x := 0.0; e^.y := 0.0; e^.dx := 0.0;   e^.dy := 0.0;   e^.Texture := NIL;
  e^.w := 0;   e^.h := 0;   e^.health := 0; e^.reload := 0; e^.next := NIL;
end;

procedure initDebris(VAR e : PDebris);
begin
  e^.x := 0.0;  e^.y := 0.0;  e^.dx := 0.0;  e^.dy := 0.0;
  e^.life := 0; e^.next := NIL; e^.Texture := NIL;
end;

procedure initExplosion(VAR e : PExplosion);
begin
  e^.x := 0.0; e^.y := 0.0; e^.dx := 0.0; e^.dy := 0.0;
  e^.r := 0;   e^.g := 0;   e^.b  := 0;   e^.a  := 0;   e^.next := NIL;
end;

procedure initTex(VAR te : PTextur);
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
  stage.fighterTail   := stage.fighterHead;
  stage.bulletTail    := stage.bulletHead;
  stage.explosionTail := stage.explosionHead;
  stage.debrisTail    := stage.debrisHead;
  stage.pointsTail    := stage.pointsHead;
end;

// *****************   UTIL   *****************

function collision(x1, y1, w1, h1, x2, y2, w2, h2 : double) : BOOLEAN; INLINE;
VAR a_Rect, b_Rect : TSDL_Rect;
begin
  collision := FALSE;
  a_Rect.x := ROUND(x1); a_Rect.y := ROUND(y1); a_Rect.w := ROUND(w1); a_Rect.h := ROUND(h1);
  b_Rect.x := ROUND(x2); b_Rect.y := ROUND(y2); b_Rect.w := ROUND(w2); b_Rect.h := ROUND(h2);
  if (SDL_HasIntersection(@a_Rect, @b_Rect) = SDL_TRUE) then collision := TRUE;
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

procedure errorMessage(Message : string);
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,'Error Box',PChar(message),NIL);
  HALT(1);
end;

// *****************   SOUND  *****************

procedure loadSounds;
VAR i : byte;
begin
  sounds[1] := Mix_LoadWAV('sound/334227__jradcoolness__laser.ogg');
  if sounds[1] = NIL then errorMessage('Soundfile "334227__jradcoolness__laser.ogg" not found!');
  sounds[2] := Mix_LoadWAV('sound/196914__dpoggioli__laser-gun.ogg');
  if sounds[2] = NIL then errorMessage('Soundfile "196914__dpoggioli__laser-gun.ogg" not found!');
  sounds[3] := Mix_LoadWAV('sound/245372__quaker540__hq-explosion.ogg');
  if sounds[3] = NIL then errorMessage('Soundfile "245372__quaker540__hq-explosion.ogg" not found!');
  sounds[4] := Mix_LoadWAV('sound/10 Guage Shotgun-SoundBible.com-74120584.ogg');
  if sounds[4] = NIL then errorMessage('Soundfile "10 Guage Shotgun-SoundBible.com-74120584.ogg" not found!');
  sounds[5] := Mix_LoadWAV('sound/342749__rhodesmas__notification-01.ogg');
  if sounds[5] = NIL then errorMessage('Soundfile "342749__rhodesmas__notification-01.ogg" not found!');

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
  if music = NIL then errorMessage('Music: "Mercury.ogg" not found!');
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

procedure addTextureToCache(Lname : string; LTexture : PSDL_Texture);
VAR cache : PTextur;
begin
  NEW(cache);
  initTex(cache);
  app.textureTail^.next := cache;
  app.textureTail := cache;
  cache^.name := Lname;
  cache^.Texture := LTexture;
end;

function getTexture(name : string) : PSDL_Texture;
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

function loadTexture(Pfad : string) : PSDL_Texture;
VAR tl : PSDL_Texture;
begin
  tl := getTexture(Pfad);
  if tl = NIL then
  begin
    tl := IMG_LoadTexture(app.Renderer, PChar(Pfad));
    if tl = NIL then errorMessage(SDL_GetError());
    addTextureToCache(Pfad, tl);
  end;
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

procedure drawText(x, y, r, g, b : integer; align : TAlignment; outText : TSring50);
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

function numberfill(a : integer) : TSring50;
VAR FMT : string;
begin
  Fmt := '[%.3d]';                  { Fmt: arguments for Format }
  numberfill := Format(Fmt, [a]);   { Format: format a string with given arguments (=> Fmt) }
end;

procedure initFonts;
begin
  fontTexture := loadTexture('gfx/font.png');
end;

// ***************  HIGHSCORE  ****************

Procedure Order(VAR p, q : integer);
VAR temp : integer;
begin
  temp := p; p := q; q := temp;
end;

Procedure Bubble(VAR B : TnewHighScoresArray; n : integer);
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
VAR newHighScores : TnewHighScoresArray;
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
    p : TSring16;
    a, Fmt : TSring50;
begin
  p := ' ............';
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
    a := Format(fmt, ['#',i + 1, HighScores[i].name, o, o, '....................',HighScores[i].score]);
    if HighScores[i].recent = 1 then
      b := 0;
    drawText(SCREEN_WIDTH DIV 2, y, r, g, b, TEXT_CENTER, a);
    INC(y, 50);
  end;
end;

procedure initHighScore;
begin
  FillChar(app.keyboard, SizeOf(app.Keyboard), 0);
  app.delegate.logic := HighSC;
  app.delegate.draw  := HighSC;
  timeout := FPS * 5;
end;

procedure initHighScoreTable;
VAR i : integer;
begin
  FillChar(HighScores, SizeOf(THighScoreDef), 0);
  for i := 0 to PRED(NUM_HighScores) do
  begin
    HighScores[i].score := NUM_HighScores - i;
    HighScores[i].name := 'ANONYMOUS';
  end;
  newHighScoreFlag := FALSE;
  cursorBlink := 0;
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
  if ((stage.score < HighScores[0].score))
  then drawText(SCREEN_WIDTH - 10, 10, 255, 255, 255, TEXT_RIGHT, 'HIGHSCORE: ' + numberfill(HighScores[0].score))
  else drawText(SCREEN_WIDTH - 10, 10,   0, 255,   0, TEXT_RIGHT, 'HIGHSCORE: ' + numberfill(stage.score));
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
  e^.dx := -1 * (RANDOM(RAND_MAX) MOD 5);
  e^.dy := (RANDOM(RAND_MAX) MOD 5) - (RANDOM(RAND_MAX) MOD 5);
  e^.health := FPS * POINTSPOD_TIME;
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

function bulletHitFighter(VAR b : PEntity) : BOOLEAN;    { b = Bullet; e = Fighter }
VAR e : PEntity;
begin
  bulletHitFighter := FALSE;
  e := stage.fighterHead^.next;
  while (e <> NIL) do
  begin
    if (e^.side <> b^.side) then
    begin
      if (collision(b^.x, b^.y, b^.w, b^.h, e^.x, e^.y, e^.w, e^.h) = TRUE) then
      begin
        b^.health := 0;
        e^.health := 0;
        addExplosions(e^.x, e^.y, 32);
        addDebris(e);
        if (e = player) then
        begin
          playSound(SND_PLAYER_DIE, CH_PLAYER);
        end
        else
        begin
          addPointsPod(TRUNC(e^.x + (e^.w DIV 2)), TRUNC(e^.y + (e^.h DIV 2)));
          playSound(SND_ALIEN_DIE, CH_ANY);
        end;
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
VAR e  : PEntity;
    ex : PExplosion;
    d  : PDebris;
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

  ex := stage.explosionHead^.next;
  while (ex <> NIL) do
  begin
    ex := stage.explosionHead^.next;
    stage.explosionHead^.next := ex^.next;
    DISPOSE(ex);
    ex := ex^.next;
  end;

  d := stage.debrisHead^.next;
  while (d <> NIL) do
  begin
    d := stage.debrisHead^.next;
    stage.debrisHead^.next := d^.next;
    DISPOSE(d);
    d := d^.next;
  end;

  e := stage.pointsHead^.next;
  while (e <> NIL) do
  begin
    e := stage.pointsHead^.next;
    stage.pointsHead^.next := e^.next;
    DISPOSE(e);
    e := e^.next;
  end;

  stage.fighterTail   := stage.fighterHead;
  stage.bulletTail    := stage.bulletHead;
  stage.explosionTail := stage.explosionHead;
  stage.debrisTail    := stage.debrisHead;
  stage.pointsTail    := stage.pointsHead;
end;

procedure initStage;
begin
  app.delegate.logic := Game;
  app.delegate.draw  := Game;
  bulletTexture      := loadTexture('gfx/playerBullet.png');
  enemyTexture       := loadTexture('gfx/enemy.png');
  alienbulletTexture := loadTexture('gfx/alienBullet.png');
  playerTexture      := loadTexture('gfx/player.png');
  explosionTexture   := loadTexture('gfx/explosion.png');
  pointsTexture      := loadTexture('gfx/points.png');
  FillChar(app.keyboard, SizeOf(app.Keyboard), 0);
  resetStage;
  stage.score := 0;
  initPlayer;
  enemyspawnTimer := 0;
  resetTimer := FPS * 3;
end;

//*****************  TITLE  **********************

procedure drawLogo;
VAR r : TSDL_Rect;
begin
  r.x := 0;
  r.y := 0;
  SDL_QueryTexture(SDL2Texture, NIL, NIL, @r.w, @r.h);
  r.h := MIN(reveal, r.h);
  blitRect(SDL2Texture, @r, (SCREEN_WIDTH DIV 2) - (r.w DIV 2), 100);
  SDL_QueryTexture(shooterTexture, NIL, NIL, @r.w, @r.h);
  r.h := MIN(reveal, r.h);
  blitRect(shooterTexture, @r, (SCREEN_WIDTH DIV 2) - (r.w DIV 2), 250);
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
  app.delegate.logic := Logo;
  app.delegate.draw  := Logo;
  FillChar(app.keyboard, SizeOf(app.Keyboard), 0);
  SDL2Texture := loadTexture('gfx/sdl2.png');
  shooterTexture := loadTexture('gfx/shooter.png');
  SDL_QueryTexture(SDL2Texture, NIL, NIL, @r.w, @r.h);
  reveal_max := r.h;
  reveal := 0;
  timeout := FPS * 5;
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
  end;
end;

// ***************   INIT SDL   ***************

procedure initSDL;
VAR rendererFlags, windowFlags : integer;
begin
  rendererFlags := SDL_RENDERER_PRESENTVSYNC OR SDL_RENDERER_ACCELERATED;
  windowFlags := 0;

  if SDL_Init(SDL_INIT_VIDEO OR SDL_INIT_AUDIO) < 0 then
    errorMessage(SDL_GetError());

  app.Window := SDL_CreateWindow('Shooter 15', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
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
  gTicks           := SDL_GetTicks;
  gRemainder       := 0;
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
end;

procedure cleanUp;
VAR i : byte;
begin
  resetStage;
  if stage.fighterHead   <> NIL then begin DISPOSE(stage.fighterHead); end;
  if stage.bulletHead    <> NIL then begin DISPOSE(stage.bulletHead); end;
  if stage.explosionHead <> NIL then begin DISPOSE(stage.explosionHead); end;
  if stage.debrisHead    <> NIL then begin DISPOSE(stage.debrisHead); end;
  if stage.pointsHead    <> NIL then begin DISPOSE(stage.pointsHead); end;
  destroyTexture;
  if app.textureHead     <> NIL then begin DISPOSE(app.textureHead); end;

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
                     if ((Event.key._repeat = 0) AND (Event.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
                       app.keyboard[Event.key.keysym.scancode] := 1;
                     if (app.keyboard[SDL_ScanCode_ESCAPE]) = 1 then exitLoop := TRUE;
                   end;   { SDL_Keydown }

      SDL_KEYUP:   begin
                     if ((Event.key._repeat = 0) AND (Event.key.keysym.scancode < MAX_KEYBOARD_KEYS)) then
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
  Logo : begin
           logic_Title;
           draw_Title;
         end;
  HighSC : begin
             logic_HighSC;
             draw_HighSC;
           end;
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
  initGame;
  initTitle;

  while exitLoop = FALSE do
  begin
    prepareScene;
    doInput;
    delegate_logic(app.delegate.logic);
    presentScene;
    CapFrameRate(gRemainder, gTicks);
  end;

  cleanUp;
  AtExit;
end.
