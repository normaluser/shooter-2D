Part 2: Drawing the player's ship

#### Introduction

In the first tutorial, we learned how to open a window in SDL2. We're going to take another small step, but an essential one - loading and drawing graphics. This tutorial will show you how to achieve both in SDL2.
Compile the pascal source code to run the program.

A 1280 x 720 window will open, coloured a light shade of blue. A spaceship sprite will also be shown, as in the screenshot above. 
Close the window by clicking on the window's close button or make a mouseclick inside of the window.

#### Inspecting the code

We're going to build upon the code that we wrote in the first tutorial. In the USES line we will see the use of another unit: SDL2_Image. 
In the "initSDL" section we'll notice that a new line has been added:

```pascal
IMG_INIT(IMG_INIT_PNG OR IMG_INIT_JPG);
```

This line initializes SDL Image, to allow us to load PNGs and JPGs, to be used as textures. Next, we'll take a look at the changes in "DRAW". There are two new functions available: loadTexture and blit. These two functions will load an image and display one respectively. We'll start with the loadTexture function.

```pascal
function loadTexture(Pfad : PChar) : PSDL_Texture;
begin
  loadTexture := IMG_LoadTexture(app.Renderer, Pfad);
  if loadTexture = NIL then errorMessage(SDL_GetError());
end;
```

This function is fairly simple. It calls the SDL Image function IMG_LoadTexture to load an image and return it as a texture. We pass over two parameters: the renderer that we're using and the filename of the image we want to load. It's possible to create a texture in SDL by first creating or loading a surface and then converting it into a texture, but this function does all the work for us in one call. Next, we have the blit function.

```pascal
procedure blit(Texture : PSDL_Texture; x, y : integer);
VAR dest : TSDL_Rect;
begin
  dest.x := x;
  dest.y := y;
  SDL_QueryTexture(Texture, NIL, NIL, @dest.w, @dest.h);
  SDL_RenderCopy(app.Renderer, Texture, NIL, @dest);
end;
```

The blit function simply draws the specified texture on screen at the specified x and y coordinates. The actual drawing code is performed by an SDL function called SDL_RenderCopy that takes four parameters. The first is our trusty renderer and the second is the texture we want to draw. The next two parameters are the rectangular regions of the src texture and the target renderer. We pass NULL as the src rectangle to tell SDL to copy the entire texture. The dest rectangle is the x and y coordinates where we want to draw our texture, with its width and height being the width and height of the source texture.

The reason for allowing us to specify the source and dest rectangles is because we might have a large texture (such as a sprite atlas) that we only want to copy a portion of. The dest width and height would allow us to resize the source texture at the destination if we wanted to. For now, we'll keep things 1:1.

We've added a new object to the TYPE definitions. This Entity struct is going to be used to represent the player. Right now, it just holds the x and y coordinates, and the texture that it will use.

```pascal
TYPE ...
    S_Entity = RECORD
                 x, y : integer;
                 Texture : PSDL_Texture;
               end;
VAR app      : S_App;
    player   : S_Entity;
    Event    : PSDL_EVENT;
    exitLoop : BOOLEAN;
```

Finally, let's turn our attention to the MAIN section. There have been a few changes here, but again nothing major or complicated.

```pascal
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
    blit(player.Texture, player.x, player.y);
    presentScene;
    SDL_Delay(16);
  end;
  cleanUp;
  DISPOSE(Event);
end.
```

We now have an Entity declaration called player (this variable is defined in VAR section). We assign the player its x and y coordinates and also call our loadTexture function to grab the texture to use. Finally, in the while loop, we call our blit function, passing the player's texture and coordinates. We put this draw call between the prepareScene and presentScene, so that it gets drawn after the scene has been cleared but not before the scene is presented. 

In the UTIL section we see a procedure errorMessage:

```pascal
procedure errorMessage(Message : PChar);
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,'Error Box',Message,NIL);
  HALT(1);
end;
```

To get an errormessage we pass over only one parameter: the message we will show in a message box window. After the window is displayed on the screen, we stop the program with HALT(1). 

