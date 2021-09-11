Part 1: Opening a window

##### Introduction

This first tutorial will explain how to open a window in SDL2. 
Compile the pascal source code to run the program.

A 1280 x 720 window will open, colored a light shade of blue. 
Close the window by clicking on the window's close button or make a mouseclick inside of the window.

##### Inspecting the code
```pascal
USES CRT, SDL2;
```
In the USES you find a call for the CRT and SDL2 units. The CRT-Unit is only used to clear the console-window to see the output from the program 
(error-messages). The SDL2-Unit is necessary for the following program.

We'll start by looking at the definitions:

```pascal
CONST SCREEN_WIDTH  = 1280;
      SCREEN_HEIGHT = 720;
```

The definitions of the screen resolution we want to use. This will also be our window size. 
Looking at the next lines we'll find a type definition:

```pascal
TYPE { "S_" short for "Struct" from "C" }
     S_App    = RECORD
                  Window   : PSDL_Window;
                  Renderer : PSDL_Renderer;
                end;
```

The S_App record will hold references to our renderer and window that we'll set up next. 
And now the definition of the variables we use:

```pascal
VAR app      : S_App;
    Event    : PSDL_EVENT;
    exitLoop : BOOLEAN;
```

App is a variable of the Type S_App;
Event is a Pointer Variable declared by SDL2 used for the input handling;
and Exitloop is Boolean (True / False), its used to exit the main (game-) loop. 

So the next step will be initSDL:

```pascal
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
```

We start by calling SDL_Init, passing over SDL_INIT_VIDEO to initialize SDL2's video subsystem.
We next create our window using SDL_CreateWindow.  SDL_WINDOWPOS_UNDEFINED tells SDL2 to let the OS position the window wherever it likes. 
Our width and height are defined by SCREEN_WIDTH and SCREEN_HEIGHT.  With this done, we next create the main renderer by calling SDL_CreateRenderer,  supplying the window we just created and the render flags that we want to use (in this case SDL_RENDERER_PRESENTVSYNC 
AND SDL_RENDERER_ACCELERATED). 
The -1 tells SDL2 to use the first graphics acceleration device it finds. Unless you're trying to do something very specific, you'll want to pass -1 to this call. 
For both the creation of the window and renderer we'll check to see if they were successful and exit with an error message if not.

Let's now look at some basic input handling.

```pascal
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
```

This function simply reads all the recent inputs (keyboard, mouse, etc) from SDL's queue until there are none left. 
We'll only handle one function right now - quitting. 
SDL_QUITEV: This event is sent when we close the SDL window by clicking on its close gadget.
SDL_MOUSEBUTTONDOWN: if you press a mousebutton inside of the SDL window, this will be recognized and the exitLoop variable will be set to TRUE.

Now let's look at the drawing code. It has just two functions:

```pascal
procedure prepareScene;
begin
  SDL_SetRenderDrawColor(app.Renderer, 96, 128, 255, 255);
  SDL_RenderClear(app.Renderer);
end;

procedure presentScene;
begin
  SDL_RenderPresent(app.Renderer);
end;
```

These functions deal with preparing to render: prepareScene, and displaying it: presentScene. In prepareScene, we set the color of SDL renderer using SDL_SetRenderDrawColor.  This function takes five parameters: the pointer to the renderer that we created in our S_App record and the RGBA of the color that we want to use. These values go from 0 to 255, with 0 being darkest and 255 being full. The final parameter is the alpha value. 0 is fully transparent whereas 255 is opaque. SDL_RenderPresent takes just one parameter - once again the renderer that we are using.

Finally, let's look at main part. This is where we'll find our main function and main program loop. As ever, there's not much to it:

```pascal
begin
  CLRSCR;
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
```

We first clear our console output window (linux) with CLRSCR and then call initSDL.  Afterwards we initialize the exitLoop variable with FALSE. 
The Event Pointer is created by calling NEW() and is used in the procedure doInput.
We enter our main loop now: its a WHILE loop. To exit the loop we use the variable exitLoop. 
For each frame we'll call prepareScene to setup rendering, collect and process the user input with doInput, display the scene with presentScene and 
then finally we call SDL_Delay with a value of 16 (milliseconds). This wait serves to limit our loop to around 62 frames per second and also prevent the application from running at full tilt and consuming far too much CPU time.

```pascal
procedure cleanUp;
begin
  SDL_DestroyRenderer(app.Renderer);
  SDL_DestroyWindow  (app.Window);
  SDL_Quit;
end;
```

After the the main loop is finished we call the procedure cleanUp to finish / close the Renderer and the Window. 
Last thing is to finish SDL2 by calling SDL_Quit and free the Event pointer by calling DISPOSE.

And there you have it - a simple application for opening a window in SDL2.
