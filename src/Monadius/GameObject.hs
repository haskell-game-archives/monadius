{-# LANGUAGE LambdaCase #-}
module Monadius.GameObject
  ( freshDiamondBomb,
    freshFlyer,
    freshInterceptor,
    freshOption,
    freshPowerUpCapsule,
    freshPowerUpGauge,
    freshShield,
    freshStalk,
    freshStandardLaser,
    freshStandardMissile,
    freshStandardRailgun,
    freshTurnGear,
    freshTurnGearSquad,
    freshVicViper,
    freshDucker,
    freshScrambleHatch,
    freshVolcano,
    freshTable,
    freshGrashia,
    freshJumper,
    freshLandRoll,
    freshLandScapeGround,
    freshSabbathicAgent,
    freshScore,
    landScapeSensitive,
    isVicViper
  )
where

import qualified Data.Array as Array
import Data.Complex
import Monadius.Constant.Parameter
import Monadius.Game
import Monadius.Util
import Graphics.UI.GLUT hiding (position)

freshDiamondBomb :: GameObject
freshDiamondBomb =
  DiamondBomb
    { tag = Nothing,
      position = 0 :+ 0,
      velocity = 0 :+ 0,
      hp = 1,
      hitDisp = Circular (0 :+ 0) diamondBombSize,
      age = 0
    }

freshFlyer :: GameObject
freshFlyer =
  Flyer
    { tag = Nothing,
      position = 0 :+ 0,
      velocity = (-3) :+ 0,
      hitDisp = Circular 0 smallBacterianSize,
      hp = 1,
      age = 0,
      hasItem = False,
      mode = 0
    }

freshInterceptor :: GameObject
freshInterceptor =
  freshFlyer
    { mode = 1,
      velocity = 0 :+ 0
    }

freshOption :: GameObject
freshOption =
  Option
    { tag = Nothing,
      position = 0 :+ 0,
      hitDisp = Circular (0 :+ 0) 0,
      optionTag = 0,
      reloadTime = 0,
      weaponEnergy = 100
    }

freshPowerUpCapsule :: GameObject
freshPowerUpCapsule =
  PowerUpCapsule
    { tag = Nothing,
      hitDisp = Circular (0 :+ 0) 30,
      position = 0 :+ 0,
      hp = 1,
      age = 0
    }

freshPowerUpGauge :: GameObject
freshPowerUpGauge =
  PowerUpGauge
    { tag = Nothing,
      position = (-300) :+ (-240)
    }

freshShield :: GameObject
freshShield =
  Shield
    { tag = Nothing,
      position = 380 :+ 0,
      hitDisp = Circular (0 :+ 0) 0,
      hitDispLand = Circular (0 :+ 0) 0,
      hp = shieldMaxHp,
      settled = False,
      size = 0,
      placement = 0 :+ 0,
      angle = 0,
      omega = 0
    }

freshStalk :: GameObject
freshStalk =
  freshFlyer
    { mode = 10,
      velocity = (-2) :+ 0
    }

freshStandardLaser :: GameObject
freshStandardLaser =
  StandardLaser
    { tag = Nothing,
      position = 0 :+ 0,
      hitDisp = Rectangular (laserSpeed / (-2) :+ (- laserBreadth)) (laserSpeed / 2 :+ laserBreadth),
      hitDispLand = Rectangular (laserSpeed / (-2) :+ (- vicViperSize)) (laserSpeed / 2 :+ vicViperSize),
      velocity = laserSpeed :+ 0,
      hp = 1,
      parentTag = 0,
      age = 0
    }

freshStandardMissile :: GameObject
freshStandardMissile =
  StandardMissile
    { tag = Nothing,
      position = 0 :+ 0,
      hitDisp = Circular 0 7,
      velocity = 0 :+ 0,
      hp = 1,
      parentTag = 0,
      probe = Probe {tag = Nothing, position = 0 :+ 0, hitDisp = Circular (0 :+ (-5)) 12, hp = 1},
      mode = 0
    }

freshStandardRailgun :: GameObject
freshStandardRailgun =
  StandardRailgun
    { tag = Nothing,
      position = 0 :+ 0,
      hitDisp = Circular 0 12,
      hitDispLand = Circular (0 :+ 0) vicViperSize,
      velocity = shotSpeed :+ 0,
      hp = 1,
      parentTag = 0
    }

freshTurnGear :: GameObject
freshTurnGear =
  TurnGear
    { tag = Nothing,
      position = 0 :+ 0,
      velocity = 0 :+ 0,
      hp = 1,
      hitDisp = Circular (0 :+ 0) smallBacterianSize,
      age = 0,
      managerTag = 0,
      mode = 0
    }

freshTurnGearSquad :: GameObject
freshTurnGearSquad =
  SquadManager
    { tag = Nothing,
      position = 0 :+ 0,
      interval = 10,
      age = 0,
      bonusScore = squadSize,
      currentScore = 0,
      members = replicate squadSize freshTurnGear,
      items = [freshPowerUpCapsule]
    }

freshVicViper :: GameObject
freshVicViper =
  VicViper
    { tag = Nothing,
      position = 0 :+ 0,
      hitDisp = Circular (0 :+ 0) vicViperSize,
      hp = 1,
      trail = repeat $ 0 :+ 0,
      speed = 1,
      powerUpPointer = -1,
      powerUpLevels = Array.array (0, 5) [(x, 0) | x <- [0 .. 5]],
      reloadTime = 0,
      weaponEnergy = 100,
      ageAfterDeath = 0
    }

freshDucker :: GLdouble -> GameObject
freshDucker vg =
  Ducker
    { tag = Nothing,
      position = 0 :+ 0,
      velocity = 0 :+ 0,
      hitDisp = Circular (0 :+ 0) smallBacterianSize,
      hp = 1,
      age = 0,
      hasItem = False,
      gVelocity = 0 :+ (8 * vg),
      charge = 0,
      vgun = 0 :+ 0,
      touchedLand = False
    }

freshScrambleHatch :: GLdouble -> GameObject
freshScrambleHatch sign =
  ScrambleHatch
    { tag = Nothing,
      position = 0 :+ 0,
      hitDisp = regulate $ Rectangular ((-45) :+ 0) (45 :+ (hatchHeight * (- sign))),
      gravity = 0 :+ sign,
      hp = hatchHP,
      age = 0,
      launchProgram = cycle $ replicate 40 [] ++ (concat . replicate 6) ([freshInterceptor {velocity = 0 :+ (-6) * sign}] : replicate 9 []),
      gateAngle = 0
    }

freshVolcano :: GLdouble -> GameObject
freshVolcano grvty =
  LandScapeBlock
    { tag = Nothing,
      position = 0 :+ 0,
      velocity = 0 :+ 0,
      hitDisp =
        Shapes $ map (regulate . (\i -> Rectangular ((120 - 33 * i + 2 * i * i) :+ sign * 30 * i) ((33 * i - 2 * i * i - 120) :+ sign * 30 * (i + 1)))) [0 .. 4]
    }
  where
    sign = - grvty

freshTable :: GLdouble -> GameObject
freshTable grvty =
  LandScapeBlock
    { tag = Nothing,
      position = 0 :+ 0,
      velocity = 0 :+ 0,
      hitDisp =
        Shapes $
          map
            ( regulate
                . ( \i ->
                      Rectangular
                        ((-2 ** (i + 3) + shiftSinePi i) :+ sign * 30 * i)
                        ((2 ** (i + 3) + shiftSinePi i) :+ sign * 30 * (i + 1))
                  )
            )
            [0 .. 4]
    }
  where
    sign = - grvty

shiftSinePi :: (Floating a) => a -> a
shiftSinePi a = 5 * sin (a * 0.5 * pi)

freshGrashia :: GLdouble -> GameObject
freshGrashia sign =
  Grashia
    { tag = Nothing,
      position = 0 :+ 0,
      velocity = 0 :+ 0,
      hitDisp = Circular 0 smallBacterianSize,
      hp = 1,
      age = 0,
      hasItem = False,
      gravity = 0 :+ sign,
      gunVector = 0 :+ 0,
      mode = 0
    }

freshJumper :: GLdouble -> GameObject
freshJumper sign =
  Jumper
    { tag = Nothing,
      position = 0 :+ 0,
      velocity = 0 :+ 0,
      hitDisp = Circular 0 smallBacterianSize,
      hp = 1,
      age = 0,
      hasItem = False,
      gravity = 0 :+ 0.36 * sign,
      touchedLand = False,
      jumpCounter = 0
    }

freshLandRoll :: GLdouble -> GameObject
freshLandRoll sign = (freshGrashia sign) {mode = 1}

freshLandScapeGround :: GameObject
freshLandScapeGround =
  LandScapeBlock
    { tag = Nothing,
      position = 0 :+ 0,
      velocity = 0 :+ 0,
      hitDisp = Rectangular ((-158) :+ (-20)) (158 :+ 20)
    }

freshSabbathicAgent :: GameObject
freshSabbathicAgent =
  SabbathicAgent
    { tag = Nothing,
      fever = 1
    }

freshScore :: Int -> GameObject
freshScore point =
  ScoreFragment
    { tag = Nothing,
      score = point
    }

landScapeSensitive :: GameObject -> Bool
landScapeSensitive = \case
   StandardRailgun {} -> True -- these objects has hitDispLand
   StandardLaser {} -> True -- in addition to hitDisp
   Shield {} -> True
   _ -> False

isVicViper :: GameObject -> Bool
isVicViper = \case
  VicViper {} -> True
  _ -> False