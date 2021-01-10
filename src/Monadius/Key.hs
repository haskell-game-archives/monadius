module Monadius.Key
  ( shotButton,
    missileButton,
    powerUpButton,
    upButton,
    downButton,
    leftButton,
    rightButton,
    selfDestructButton,
  ) where

import Graphics.UI.GLUT

-- Cuteness to add later
-- konamiCommand = [upButton,upButton,downButton,downButton,leftButton,rightButton,leftButton,rightButton,missileButton,shotButton]

downButton :: Key
downButton = SpecialKey KeyDown

leftButton :: Key
leftButton = SpecialKey KeyLeft

missileButton :: Key
missileButton = Char 'x'

powerUpButton :: Key
powerUpButton = Char 'c'

rightButton :: Key
rightButton = SpecialKey KeyRight

selfDestructButton :: Key
selfDestructButton = Char 'g'

shotButton :: Key
shotButton = Char 'z'

upButton :: Key
upButton = SpecialKey KeyUp