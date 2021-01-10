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
    hatchHP,
    gaugeOfMissile,
    gaugeOfGLdouble,
    gaugeOfLaser,
    gaugeOfShield,
    stageClearTime,
    bacterianShotSpeedList,
    duckerShotWay,
    jumperShotFactor,
    grashiaShotSpeedFactor,
    flyerHitBack,
    particleHitBack,
    powerUpCapsuleHitBack,
    scrambleHatchHitBack,
    treasure,
    turnGearHitBack,
    duckerShotCount,
    flyerShotInterval,
    grashiaShotHalt,
    grashiaShotInterval,
    inceptorShotInterval,
    jumperShotWay,
    landRollShotInterval,
    scrambleHatchLaunchLimitAge,
    shieldPlacementMargin,
    shieldHitMargin
  )
where

import Graphics.UI.GLUT
import Monadius.Util

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

hatchHP :: Int
hatchHP = 15

gaugeOfMissile :: Int
gaugeOfMissile = 1

gaugeOfGLdouble :: Int
gaugeOfGLdouble = 2

gaugeOfLaser :: Int
gaugeOfLaser = 3

gaugeOfShield :: Int
gaugeOfShield = 5

stageClearTime :: Int
stageClearTime = 7800

-- these lists are game rank modifiers.
bacterianShotSpeedList :: [GLdouble]
bacterianShotSpeedList = [8, 4, 6, 8] ++ cycle [12, 8]

duckerShotWay :: [GLdouble]
duckerShotWay = [1, 1, 2, 1] ++ cycle [2, 2]

jumperShotFactor :: [GLdouble]
jumperShotFactor = [0.5, 0.5, 0.5, 0.5] ++ cycle [0.8, 0.5]

grashiaShotSpeedFactor :: [GLdouble]
grashiaShotSpeedFactor = [1, 1, 1, 1] ++ cycle [1, 0.6]

flyerHitBack :: [Bool]
flyerHitBack = [False, False, False] ++ repeat True

particleHitBack :: [Bool]
particleHitBack = True : repeat False

powerUpCapsuleHitBack :: [Bool]
powerUpCapsuleHitBack = [False, False, False, False] ++ cycle [False, True]

scrambleHatchHitBack :: [Bool]
scrambleHatchHitBack = [False, False, False, False] ++ cycle [False, True]

treasure :: [Bool]
treasure = [False, False, False, False] ++ cycle [False, True]

turnGearHitBack :: [Bool]
turnGearHitBack = [False, False, False] ++ repeat True

duckerShotCount :: [Int]
duckerShotCount = [2, 1, 1, 3] ++ repeat 2

flyerShotInterval :: [Int]
flyerShotInterval = [30, infinite, 30, 15] ++ cycle [15, 15]

grashiaShotHalt :: [Int]
grashiaShotHalt = [50, 100, 50, 50] ++ cycle [0, 0]

grashiaShotInterval :: [Int]
grashiaShotInterval = [30, 60, 30, 30] ++ cycle [15, 5]

inceptorShotInterval :: [Int]
inceptorShotInterval = [45, infinite, 60, 45] ++ cycle [45, 45]

jumperShotWay :: [Int]
jumperShotWay = [16, 4, 8, 16] ++ cycle [24, 32]

landRollShotInterval :: [Int]
landRollShotInterval = [60, 120, 60, 60] ++ cycle [30, 60]

scrambleHatchLaunchLimitAge :: [Int]
scrambleHatchLaunchLimitAge = [400, 200, 400, 400] ++ cycle [600, 400]

shieldPlacementMargin :: GLdouble
shieldPlacementMargin = 5

shieldHitMargin :: GLdouble
shieldHitMargin = 10