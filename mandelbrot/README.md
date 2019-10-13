# Mandelbrot Set
The Mandelbrot Set is a fascinating program, and a personal 'Hello World' program for me to get familiar with a language.

## ASCII Renderer
### Example
```

                  **
                  **
               * *****
               *********
              **********
              **********
         *** ***********
        ****************
 **********************
        ****************
         *** ***********
              **********
              **********
               *********
               * *****
                  **
                  **
                  
```

## PNG Renderer
Using [Haskell Image Processing](http://hackage.haskell.org/package/hip), the Mandelbrot set simulation can be converted into a full colour PNG output!

This code wasn't a direct conversion - whereas the ASCII renderer is only a boolean, for a full colour renderer the number of iterations before escaping must also be tracked

### Colors
My method for coloring the Mandelbrot Set involves a relatively simple calculation of just scaling each RGB channel by a constant. These constants generally create a simple tint of the image, with no fancy gradients.

Benefits of this method include: simpler calculations and glowier visuals

A common way of coloring the Mandelbrot set includes taking a log2 of the number of iterations. This creates smooth gradients, but I personally prefer the banded style.

### Example
(Mandelbrot Set)[mandelbrot.png]