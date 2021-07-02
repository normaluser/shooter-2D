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
*** Score pods
***************************************************************************}

PROGRAM Shooter12;

{$COPERATORS OFF}
USES SDL2, SDL2_Image, SDL2_Mixer, Math, sysutils;

CONST SCREEN_WIDTH  = 1280;
      SCREEN_HEIGHT = 720;
      PLAYER_SPEED  = 4.0;
      PLAYER_BULLET_SPEED = 20.0;
      ALIEN_BULLET_SPEED = 8.0;
      POINTSPOD_TIME = 10;
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
      SND_POINTS       = 5;
      SND_MAX          = 6;
      CH_ANY           = -1;
      CH_PLAYER        = 0;
      CH_ALIEN_FIRE    = 1;
      CH_POINTS        = 2;

      GLYPH_HEIGHT     = 28;
      GLYPH_WIDTH      = 18;

TYPE { "S_" short for "Struct" from "C" }
     String50 = String[MAX_STRING_LENGTH];
     S_App    = RECORD
                  Window   : PSDL_Window;
                  Renderer : PSDL_Renderer;
                  keyboard : Array[0..MAX_KEYBOARD_KEYS] OF integer;
                end;
     Entity   = ^S_Entity;
     S_Entity = RECORD
                  x, y, dx, dy : double;
                  w, h, health, reload, side : integer;
                  Texture : PSDL_Texture;
                  next : Entity;
                end;
     Explosion = ^S_Explosion;
     S_Explosion = RECORD
                     x, y, dx, dy : double;
                     r, g, b, a : integer;
                     next : Explosion;
                   end;
     Debris = ^S_Debris;
     S_Debris = RECORD
                  x, y, dx, dy : double;
                  rect : TSDL_Rect;
                  Texture : PSDL_Texture;
                  life : integer;
                  next : Debris;
                end;
     S_Stage  = RECORD
                  fighterHead,   fighterTail,
                  bulletHead,    bulletTail,
                  pointsHead,    pointsTail    : Entity;
                  explosionHead, explosionTail : Explosion;
                  debrisHead,    debrisTail    : Debris;
                  score : integer;
                end;
     S_Star   = RECORD
                  x, y, speed : integer;
                end;

VAR app                  : S_App;
    stage                : S_Stage;
    player,
    enemy,
    bullet               : Entity;
    fontTexture,
    pointsTexture,
    enemyTexture,
    bulletTexture,
    alienbulletTexture,
    playerTexture,
    background,
    explosionTexture     : PSDL_Texture;
    Event                : PSDL_EVENT;
    exitLoop             : BOOLEAN;
    gTicks               : UInt32;
    gRemainder           : double;
    highScore,
    backgroundX,
    enemyspawnTimer,
    resetTimer           : integer;
    stars                : Array[0..MAX_STARS] OF S_Star;
    music                : PMix_Music;
    sounds               : Array[1..SND_MAX] OF PMix_Chunk;

// *****************   INIT   *****************

procedure initEntity(VAR e : Entity);
begin
  e^.x := 0.0; e^.y := 0.0; e^.dx := 0.0;   e^.dy := 0.0;
  e^.w := 0;   e^.h := 0;   e^.health := 0; e^.reload := 0; e^.next := NIL;
end;

procedure initDebris(VAR e : Debris);
begin
  e^.x := 0.0;  e^.y := 0.0;  e^.dx := 0.0;  e^.dy := 0.0;
  e^.life := 0; e^.next := NIL;
end;

procedure initExplosion(VAR e : Explosion);
begin
  e^.x := 0.0; e^.y := 0.0; e^.dx := 0.0; e^.dy := 0.0;
  e^.r := 0;   e^.g := 0;   e^.b  := 0;   e^.a  := 0;   e^.next := NIL;
end;

// *****************   UTIL   *****************

function collision(x1, y1, w1, h1, x2, y2, w2, h2 : double) : BOOLEAN;
VAR a_Rect, b_Rect : TSDL_Rect;
begin
  collision := FALSE;
  a_Rect.x := ROUND(x1); a_Rect.y := ROUND(y1); a_Rect.w := ROUND(w1); a_Rect.h := ROUND(h1);
  b_Rect.x := ROUND(x2); b_Rect.y := ROUND(y2); b_Rect.w := ROUND(w2); b_Rect.h := ROUND(h2);
  if (SDL_HasIntersection(@a_Rect, @b_Rect) = SDL_TRUE) then collision := TRUE;
end;

procedure calcSlope(x1, y1, x2, y2 : double; VAR dx, dy : double);
VAR steps : integer;
begin
  steps := MAX(ABS(TRUNC(x1-x2)), ABS(TRUNC(y1-y2)));
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

procedure errorMessage(Message : PChar);
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,'Error Box',Message,NIL); HALT(1);
end;

// *****************   SOUND  *****************

procedure loadSounds;
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

  Mix_VolumeChunk(sounds[1], MIX_MAX_VOLUME);
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

function loadTexture(Pfad : PChar) : PSDL_Texture;
begin
  loadTexture := IMG_LoadTexture(app.Renderer, Pfad);
  if loadTexture = NIL then errorMessage(SDL_GetError);
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

procedure drawText(x, y, r, g, b : integer; format : String50);
VAR i, len : integer;
    rect : TSDL_Rect;
begin
  len := LENGTH(format);
  format := UPCASE(format);  { all capital letters }
  rect.w := GLYPH_WIDTH;
  rect.h := GLYPH_HEIGHT;
  rect.y := 0;
  SDL_SetTextureColorMod(fontTexture, r, g, b);

  for i := 1 to len do
  begin
    if (format[i] IN [' '..'Z']) then
    begin
      rect.x := (ORD(format[i]) - ORD(' ')) * GLYPH_WIDTH;
      blitRect(fontTexture, @rect, x, y);
      INC(x, GLYPH_WIDTH);
    end;
  end;
end;

function numberfill(a : integer) : String50;
begin
  if (a >= 100) then            begin numberfill :=        IntToStr(a); end;
  if (a < 100) AND (a > 9) then begin numberfill :=  '0' + IntToStr(a); end;
  if (a < 10) then              begin numberfill := '00' + IntToStr(a); end;
end;

procedure drawHud;
begin
  drawText(10, 10, 255, 255, 255, 'SCORE: ' + numberfill(stage.score));
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
VAR e : Explosion;
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
VAR d : Debris;
begin
  d := stage.debrisHead^.next;
  while (d <> NIL) do
  begin
    blitRect(d^.Texture, @d^.rect, d^.x, d^.y);
    d := d^.next;
  end;
end;

procedure drawBullets;
VAR b : Entity;
begin
  b := stage.bulletHead^.next;
  while (b <> NIL) do
  begin
    blit(b^.Texture, b^.x, b^.y);
    b := b^.next;
  end;
end;

procedure drawFighters;
VAR e : Entity;
begin
  e := stage.fighterHead^.next;
  while (e <> NIL) do
  begin
    blit(e^.Texture, e^.x, e^.y);
    e := e^.next;
  end;
end;

procedure drawPointsPods;
VAR p : Entity;
begin
  p := stage.pointsHead^.next;
  while (p <> NIL) do
  begin
    blit(p^.Texture, p^.x, p^.y);
    p := p^.next;
  end;
end;

procedure addPointsPod(x, y : integer);
VAR e : Entity;
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

procedure addDebris(e : Entity);
VAR d : Debris;
    x, y, w, h : integer;
begin
  w := TRUNC(e^.w / 2);
  h := TRUNC(e^.h / 2);
  x := 0; y := 0;
  while y <= h do
  begin
    while x <= w do
    begin
      NEW(d);
      initDebris(d);
      stage.debrisTail^.next := d;
      stage.debrisTail := d;
      d^.x := e^.x + (e^.w / 2);
      d^.y := e^.y + (e^.h / 2);
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
VAR e : Explosion;
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
VAR e, prev : Entity;
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
    if (player <> NIL) AND collision(e^.x, e^.y, e^.w, e^.h, player^.x, player^.y, player^.w, player^.h) then
    begin
      e^.health := 0;
      INC(stage.score);
      highScore := MAX(stage.score, highScore);
      playSound(SND_POINTS, CH_POINTS);
    end;
    DEC(e^.health);
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

procedure draw;
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

procedure doDebris;
VAR d, prev : Debris;
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
VAR e, prev : Explosion;
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

function bulletHitFighter(b : Entity) : BOOLEAN;    { b = Bullet; e = Fighter }
VAR e : Entity;
begin
  e := stage.fighterHead^.next;
  bulletHitFighter := FALSE;
  while (e <> NIL) do
  begin
    if (e^.side <> b^.side) then
    begin
      if (collision(b^.x, b^.y, b^.w, b^.h, e^.x, e^.y, e^.w, e^.h) = TRUE) then
      begin
        b^.health := 0;
        e^.health := 0;
        if (e = player) then
        begin
          playSound(SND_PLAYER_DIE, CH_PLAYER);
        end
        else
        begin
          addPointsPod(TRUNC(e^.x + (e^.w DIV 2)), TRUNC(e^.y + (e^.h DIV 2)));
          playSound(SND_ALIEN_DIE, CH_ANY);
        end;
        addExplosions(e^.x, e^.y, 32);
        addDebris(e);
        bulletHitFighter := TRUE;
      end;
    end;
    e := e^.next;
  end;
end;

procedure doBullets;
VAR b, prev : Entity;
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
VAR e, prev : Entity;
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

procedure fireAlienbullet(e : Entity);
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
  calcSlope(player^.x + (player^.w DIV 2), player^.y + (player^.h DIV 2), e^.x, e^.y, bullet^.dx, bullet^.dy);
  bullet^.dx := bullet^.dx * ALIEN_BULLET_SPEED;
  bullet^.dy := bullet^.dy * ALIEN_BULLET_SPEED;
  bullet^.side := SIDE_ALIEN;
  e^.reload := RANDOM(FPS * 2);
end;

procedure doEnemies;
VAR e : Entity;
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
VAR e  : Entity;
    ex : Explosion;
    d  : Debris;
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
  stage.score := 0;
  initPlayer;
  initStarfield;
  enemyspawnTimer := 0;
  resetTimer := FPS * 3;
end;

procedure logic;
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
      resetStage;
  end;
end;

procedure initStage;
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
  stage.fighterTail   := stage.fighterHead;
  stage.bulletTail    := stage.bulletHead;
  stage.explosionTail := stage.explosionHead;
  stage.debrisTail    := stage.debrisHead;
  stage.pointsTail    := stage.pointsHead;
  bulletTexture       := loadTexture('gfx/playerBullet.png');
  enemyTexture        := loadTexture('gfx/enemy.png');
  alienbulletTexture  := loadTexture('gfx/alienBullet.png');
  playerTexture       := loadTexture('gfx/player.png');
  background          := loadTexture('gfx/background.png');
  explosionTexture    := loadTexture('gfx/explosion.png');
  pointsTexture       := loadTexture('gfx/points.png');
  loadMusic;
  playMusic(TRUE);
  resetStage;
end;

// ***************   INIT SDL   ***************

procedure initSDL;
VAR rendererFlags, windowFlags : integer;
begin
  rendererFlags := SDL_RENDERER_PRESENTVSYNC OR SDL_RENDERER_ACCELERATED;
  windowFlags := 0;

  if SDL_Init(SDL_INIT_VIDEO OR SDL_INIT_AUDIO) < 0 then
    errorMessage(SDL_GetError());

  app.Window := SDL_CreateWindow('Shooter 12', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, windowFlags);
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

procedure cleanUp;
begin
  DISPOSE(player);
  DISPOSE(stage.pointsHead);
  DISPOSE(stage.debrisHead);
  DISPOSE(stage.explosionHead);
  DISPOSE(stage.fighterHead);
  DISPOSE(stage.bulletHead);

  Mix_FreeMusic(music);
  Mix_FreeChunk(sounds[5]);
  Mix_FreeChunk(sounds[4]);
  Mix_FreeChunk(sounds[3]);
  Mix_FreeChunk(sounds[2]);
  Mix_FreeChunk(sounds[1]);
  Mix_CloseAudio;

  SDL_DestroyTexture (alienbulletTexture);
  SDL_DestroyTexture (playerTexture);
  SDL_DestroyTexture (bulletTexture);
  SDL_DestroyTexture (ExplosionTexture);
  SDL_DestroyTexture (PointsTexture);
  SDL_DestroyTexture (FontTexture);
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
  RANDOMIZE;
  InitSDL;
  initSounds;
  initFonts;
  InitStage;
  exitLoop := FALSE;
  gTicks := SDL_GetTicks;
  gRemainder := 0;
  highScore := 0;
  music := NIL;
  NEW(Event);

  while exitLoop = FALSE do
  begin
    prepareScene;
    doInput;
    logic;
    draw;
    presentScene;
    CapFrameRate(gRemainder, gTicks);
  end;
  resetStage;
  cleanUp;
  DISPOSE(Event);
  SDL_ShowCursor(1);
end.
