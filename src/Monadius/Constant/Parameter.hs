module Monadius.Constant.Parameter
  ( shieldMaxHp,
    shotSpeed,
    laserSpeed,
    laserBreadth,
    vicViperSize,
    squadSize,
    diamondBombSize,
    smallBacterianSize,
    hatchHeight,
  )
where

import Graphics.UI.GLUT

shieldMaxHp :: Int
shieldMaxHp = 16

shotSpeed :: GLdouble
shotSpeed = 25

laserSpeed :: GLdouble
laserSpeed = 60

laserBreadth :: GLdouble
laserBreadth = 20

vicViperSize :: GLdouble
vicViperSize = 6

squadSize :: Int
squadSize = 6

diamondBombSize :: GLdouble
diamondBombSize = 6

smallBacterianSize :: GLdouble
smallBacterianSize = 16

hatchHeight :: GLdouble
hatchHeight = 35
