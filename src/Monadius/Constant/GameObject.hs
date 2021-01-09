module Monadius.Constant.GameObject
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
  )
where

import qualified Data.Array as Array
import Data.Complex
import Monadius.Constant.Parameter
import Monadius.Game
import Monadius.Util

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
