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
*** Bitmap fonts and scoring
*** without memory holes; tested with: fpc -Criot -gl -gh shooter11.pas
***************************************************************************}

PROGRAM Shooter11;
{$Mode fpc} {$H+}    { "$H+" necessary for conversion of String to PChar !!; H+ => AnsiString }
{$COPERATORS OFF}
USES SDL2, SDL2_Image, SDL2_Mixer, Math, sysutils, cTypes;

CONST SCREEN_WIDTH  = 1280;            { size of the grafic window }
      SCREEN_HEIGHT = 720;             { size of the grafic window }
      PLAYER_SPEED  = 4.0;
      PLAYER_BULLET_SPEED = 20.0;
      ALIEN_BULLET_SPEED = 8.0;
      RAND_MAX = 3276;
      MAX_KEYBOARD_KEYS = 350;
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
      SND_MAX          = 5;
      CH_ANY           = -1;
      CH_PLAYER        = 0;
      CH_ALIEN_FIRE    = 1;

      GLYPH_HEIGHT     = 28;
      GLYPH_WIDTH      = 18;

TYPE TDelegating = Procedure;               { "T" short for "TYPE" }
     TString50   = String[MAX_STRING_LENGTH];
     TDelegate   = RECORD
                     logic, draw : TDelegating;
                   end;
     TApp        = RECORD
                     Window   : PSDL_Window;
                     Renderer : PSDL_Renderer;
                     keyboard : ARRAY[0..MAX_KEYBOARD_KEYS] OF integer;
                     Delegate : TDelegate;
                     TFPS     : integer;
                   end;
     PEntity     = ^TEntity;
     TEntity     = RECORD
                     x, y, dx, dy : double;
                     w, h : cint;
                     health, reload, side : integer;
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
                     bulletHead,    bulletTail    : PEntity;
                     explosionHead, explosionTail : PExplosion;
                     debrisHead,    debrisTail    : PDebris;
                     score : integer;
                   end;
     TStar       = RECORD
                     x, y, speed : integer;
                   end;

VAR app                  : TApp;
    stage                : TStage;
    player,
    enemy,
    bullet               : PEntity;
    fontTexture,
    enemyTexture,
    bulletTexture,
    alienbulletTexture,
    playerTexture,
    background,
    explosionTexture     : PSDL_Texture;
    Event                : TSDL_EVENT;
    exitLoop             : BOOLEAN;
    gTicks               : UInt32;
    gRemainder           : double;
    highScore,
    backgroundX,
    enemyspawnTimer,
    resetTimer           : integer;
    stars                : ARRAY[0..MAX_STARS] OF TStar;
    sounds               : ARRAY[1..pred(SND_MAX)] OF PMix_Chunk;
    music                : PMix_Music;

// *****************   INIT   *****************

procedure initEntity(e : PEntity);
begin
  e^.x := 0.0; e^.y := 0.0; e^.dx := 0.0;   e^.dy := 0.0;   e^.Texture := NIL; e^.side := 0;
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

procedure calcSlope(x1, y1, x2, y2 : integer; VAR dx, dy : double);
VAR steps : integer;
begin
  steps := MAX(ABS(x1-x2), ABS(y1-y2));
  if steps = 0 then
  begin
    dx := 0.0;
    dy := 0.0;
  end
  else
  begin
    dx := x1 - x2;
    dx := dx / steps;
    dy := y1 - y2;
    dy := dy / steps;
  end;
end;

procedure errorMessage(Message1 : String);
begin
  SDL_ShowSimpleMessageBox(SDL_MessageBox_Error,'Error Box',PChar(Message1),NIL);
  HALT(1);
end;

procedure logMessage(Message1 : String);
VAR Fmt : PChar;
begin
  Fmt := 'File not found: %s'#13;    // Formatstring und "ARRAY of const" als Parameteruebergabe in [ ]
  SDL_LogMessage(SDL_LOG_CATEGORY_APPLICATION, SDL_LOG_PRIORITY_WARN, Fmt, [PChar(Message1)]);
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
  //sounds[5] := Mix_LoadWAV('sound/342749__rhodesmas__notification-01.ogg');
  //if sounds[5] = NIL then logMessage('Soundfile: "342749__rhodesmas__notification-01.ogg"');

  for i := 1 to pred(SND_MAX) do
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

procedure blitRect(Texture : PSDL_Texture; src : PSDL_Rect; x, y : double);
VAR dest : TSDL_Rect;
begin
  dest.x := TRUNC(x);
  dest.y := TRUNC(y);
  dest.w := src^.w;
  dest.h := src^.h;
  SDL_RenderCopy(app.Renderer, Texture, src, @dest);
end;

function loadTexture(Pfad : String) : PSDL_Texture;
VAR Fmt : PChar;
begin
  loadTexture := IMG_LoadTexture(app.Renderer, PChar(Pfad));
  if loadTexture = NIL then errorMessage(SDL_GetError());
  Fmt := 'Loading %s'#13;
  SDL_LogMessage(SDL_LOG_CATEGORY_APPLICATION, SDL_LOG_PRIORITY_INFO,  Fmt, [PChar(Pfad)]);
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

// *****************   TEXT   *****************

procedure drawText(x, y, r, g, b : integer; outText : TString50);
VAR i, len : integer;
    rect : TSDL_Rect;
begin
  len := LENGTH(outText);
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
begin
  if (a >= 100) then            begin numberfill :=        IntToStr(a); end;
  if (a < 100) AND (a > 9) then begin numberfill :=  '0' + IntToStr(a); end;
  if (a < 10) then              begin numberfill := '00' + IntToStr(a); end;
end;

procedure drawHud;
begin
  if ((stage.score > 0) AND (stage.score = HighScore))
  then drawText(1020, 10, 0, 255, 0, 'HIGHSCORE: ' + numberfill(HighScore))
  else drawText(1020, 10, 255, 255, 255, 'HIGHSCORE: ' + numberfill(HighScore));
end;

procedure initFonts;
begin
  fontTexture := loadTexture('gfx/font.png');
end;

// **************   Background  ***************

procedure drawBackground;
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

procedure drawStarfield;
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

procedure doStarField;
VAR i : integer;
begin
  for i := 0 to PRED(MAX_STARS) do
  begin
    DEC(stars[i].x, stars[i].speed);
    if stars[i].x < 0 then
      INC(stars[i].x, SCREEN_WIDTH);
  end;
end;

procedure doBackGround;
begin
  DEC(backgroundX);
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

// *****************   Stage  *****************

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

procedure drawBullets;
VAR b : PEntity;
begin
  b := stage.bulletHead^.next;
  while (b <> NIL) do
  begin
    blit(b^.Texture, b^.x, b^.y);
    b := b^.next;
  end;
end;

procedure drawFighters;
VAR e : PEntity;
begin
  e := stage.fighterHead^.next;
  while (e <> NIL) do
  begin
    blit(e^.Texture, e^.x, e^.y);
    e := e^.next;
  end;
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
      INC(x, w);
    end;
    x := 0;
    INC(y, h);
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
    e^.a := (RANDOM(RAND_MAX) MOD (FPS * 3));
  end;
end;

procedure draw_Game;
begin
  drawBackground;
  drawStarfield;
  drawFighters;
  drawDebris;
  drawExplosions;
  drawBullets;
  drawHud;
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
  DEC(enemyspawnTimer);
  if enemyspawnTimer <= 0 then
  begin
    NEW(enemy);
    initEntity(enemy);
    stage.fighterTail^.next := enemy;
    stage.fighterTail := enemy;
    enemy^.Texture := enemyTexture;
    SDL_QueryTexture(enemy^.Texture, NIL, NIL, @enemy^.w, @enemy^.h);
    enemy^.x := SCREEN_WIDTH;
    enemy^.y := RANDOM(SCREEN_HEIGHT - enemy^.h);
    enemy^.dx := -1 * (2 + (RANDOM(RAND_MAX) MOD 4));
    enemy^.side := SIDE_ALIEN;
    enemy^.health := 1;
    enemy^.reload := FPS * (1 + (RANDOM(RAND_MAX) MOD 3));
    enemyspawnTimer := 30 + (RANDOM(RAND_MAX) MOD FPS);
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

function bulletHitFighter(b : PEntity) : BOOLEAN;    { b = Bullet; f = Fighter }
VAR f : PEntity;
begin
  f := stage.fighterHead^.next;
  bulletHitFighter := FALSE;
  while (f <> NIL) do
  begin
    if (f^.side <> b^.side) then
    begin
      if (collision(b^.x, b^.y, b^.w, b^.h, f^.x, f^.y, f^.w, f^.h) = TRUE) then
      begin
        b^.health := 0;
        f^.health := 0;
        if (f = player) then
        begin
          playSound(SND_PLAYER_DIE, CH_PLAYER);
        end
        else
        begin
          playSound(SND_ALIEN_DIE, CH_ANY);
          INC(stage.score);
          highScore := MAX(stage.score, highScore);
        end;
        if f^.x < SCREEN_WIDTH then
        begin
          addExplosions(f^.x, f^.y, 32);
          addDebris(f);
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
begin
  NEW(bullet);
  initEntity(bullet);
  stage.bulletTail^.next := bullet;
  stage.bulletTail := bullet;
  bullet^.x := e^.x;
  bullet^.y := e^.y;
  bullet^.health := 1;
  bullet^.Texture := alienbulletTexture;
  SDL_QueryTexture(bullet^.Texture, NIL, NIL, @bullet^.w, @bullet^.h);
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
      begin
        fireAlienbullet(e);
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
  bullet^.Texture := bulletTexture;
  SDL_QueryTexture(bullet^.Texture, NIL, NIL, @bullet^.w, @bullet^.h);
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

procedure initPlayer;
begin
  NEW(player);
  initEntity(player);
  stage.fighterTail^.next := player;
  stage.fighterTail := player;
  player^.health := 1;
  player^.x := 100;
  player^.y := 100;
  player^.Texture := playerTexture;
  SDL_QueryTexture(player^.Texture, NIL, NIL, @player^.w, @player^.h);
  player^.reload := 0;
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

  stage.fighterTail := stage.fighterHead;
  stage.bulletTail  := stage.bulletHead;
  stage.score := 0;
  initPlayer;
  initStarfield;
  enemyspawnTimer := 0;
  resetTimer := FPS * 3;
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
  app.Delegate.logic := @logic_Game;
  app.Delegate.draw  := @draw_Game;
  NEW(stage.fighterHead);
  NEW(stage.bulletHead);
  NEW(stage.explosionHead);
  NEW(stage.debrisHead);
  initEntity(stage.fighterHead);
  initEntity(stage.bulletHead);
  initExplosion(stage.explosionHead);
  initDebris(stage.debrisHead);
  stage.fighterTail := stage.fighterHead;
  stage.bulletTail  := stage.bulletHead;
  stage.explosionTail := stage.explosionHead;
  stage.debrisTail  := stage.debrisHead;
  bulletTexture       := loadTexture('gfx/playerBullet.png');
  enemyTexture        := loadTexture('gfx/enemy.png');
  alienbulletTexture  := loadTexture('gfx/alienBullet.png');
  playerTexture       := loadTexture('gfx/player.png');
  background          := loadTexture('gfx/background.png');
  explosionTexture    := loadTexture('gfx/explosion.png');
  loadMusic;
  playMusic(TRUE);
  resetStage;
end;

// ***************   INIT SDL   ***************

procedure initSDL;
VAR rendererFlags, windowFlags : integer;
begin
  rendererFlags := {SDL_RENDERER_PRESENTVSYNC OR} SDL_RENDERER_ACCELERATED;
  windowFlags := 0;

  if SDL_Init(SDL_INIT_VIDEO OR SDL_INIT_AUDIO) < 0 then
    errorMessage(SDL_GetError());

  if MIX_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 1024) < 0 then
    errorMessage(SDL_GetError());
  Mix_AllocateChannels(MAX_SND_CHANNELS);

  app.Window := SDL_CreateWindow('Shooter 11', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
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
  DISPOSE(player);
  DISPOSE(stage.debrisHead);
  DISPOSE(stage.explosionHead);
  DISPOSE(stage.fighterHead);
  DISPOSE(stage.bulletHead);

  Mix_FreeMusic(music);
  //Mix_FreeChunk(sounds[5]);
  Mix_FreeChunk(sounds[4]);
  Mix_FreeChunk(sounds[3]);
  Mix_FreeChunk(sounds[2]);
  Mix_FreeChunk(sounds[1]);
  if ExitCode <> 0 then WriteLn('CleanUp complete!');
end;

procedure atExit;
begin
  if ExitCode <> 0 then cleanUp;
  Mix_CloseAudio;
  SDL_DestroyTexture (alienbulletTexture);
  SDL_DestroyTexture (playerTexture);
  SDL_DestroyTexture (bulletTexture);
  SDL_DestroyRenderer(app.Renderer);
  SDL_DestroyWindow  (app.Window);
  SDL_Quit;
  if Exitcode <> 0 then WriteLn(SDL_GetError());
  SDL_ShowCursor(1);
end;

// *****************   Input  *****************

procedure doInput;
begin
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
    end;  { CASE Event }
  end;    { SDL_PollEvent }
end;

// *************   CAPFRAMERATE   *************

procedure CapFrameRate(VAR remainder : double; VAR Ticks : UInt32);
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
  RANDOMIZE;
  InitSDL;
  AddExitProc(@atExit);
  initSounds;
  initFonts;
  InitStage;
  exitLoop := FALSE;
  gTicks := SDL_GetTicks;
  gRemainder := 0;
  highScore := 0;
  music := NIL;

  while exitLoop = FALSE do
  begin
    prepareScene;
    doInput;
    app.Delegate.logic;
    app.Delegate.draw;
    presentScene;
    CapFrameRate(gRemainder, gTicks);
  end;

  resetStage;
  resetLists;
  cleanUp;
  atExit;
end.
