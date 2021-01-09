module Monadius.Game
  ( Game (..),
    GameObject (..),
  )
where

import Data.Array
import Data.Complex
import Graphics.UI.GLUT hiding (DebugMessage, position)
import Graphics.UI.GLUT.Callbacks.Window
import Monadius.Util

-- | An abstracted game as a state machine.
class Game g where
  update :: [Key] -> g -> g
  render :: g -> IO ()
  isGameover :: g -> Bool

data GameObject -- objects that are actually rendered and moved.
  = VicViper
      { -- player's fighter.
        tag :: Maybe Int,
        position :: Complex GLdouble,
        hitDisp :: Shape,
        hp :: Int,
        trail :: [Complex GLdouble],
        speed :: GLdouble,
        powerUpPointer :: Int,
        powerUpLevels :: Array Int Int,
        reloadTime :: Int,
        weaponEnergy :: Int,
        ageAfterDeath :: Int
      }
  | Option
      { -- trailing support device.
        tag :: Maybe Int,
        position :: Complex GLdouble,
        hitDisp :: Shape,
        optionTag :: Int,
        reloadTime :: Int,
        weaponEnergy :: Int
      }
  | StandardMissile
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        hitDisp :: Shape,
        hp :: Int,
        mode :: Int,
        velocity :: Complex GLdouble,
        parentTag :: Int,
        probe :: GameObject -- missile that fly along the terrain
      }
  | Probe
      { -- this lets missile to fly along the terrain
        tag :: Maybe Int,
        position :: Complex GLdouble,
        hitDisp :: Shape,
        hp :: Int
      }
  | StandardRailgun
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        hitDisp :: Shape,
        hitDispLand :: Shape,
        hp :: Int,
        velocity :: Complex GLdouble,
        parentTag :: Int -- normal & double shot
      }
  | StandardLaser
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        hitDisp :: Shape,
        hitDispLand :: Shape,
        hp :: Int,
        velocity :: Complex GLdouble,
        parentTag :: Int,
        age :: Int -- long blue straight laser
      }
  | Shield
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        hitDisp :: Shape,
        hitDispLand :: Shape,
        hp :: Int,
        settled :: Bool,
        size :: GLdouble,
        placement :: Complex GLdouble,
        angle :: GLdouble,
        omega :: GLdouble -- solid state of Reek power that protects enemy atacks
      }
  | PowerUpCapsule
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        hitDisp :: Shape,
        hp :: Int,
        age :: Int
      }
  | PowerUpGauge
      { tag :: Maybe Int,
        position :: Complex GLdouble
      }
  | DiamondBomb
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        velocity :: Complex GLdouble,
        hitDisp :: Shape,
        hp :: Int,
        age :: Int -- Bacterian's most popular warhead
      }
  | TurnGear
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        velocity :: Complex GLdouble,
        hitDisp :: Shape,
        hp :: Int,
        age :: Int,
        mode :: Int,
        managerTag :: Int -- one of small Bacterian lifeforms, often seen in a squad.
      }
  | SquadManager
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        interval :: Int,
        age :: Int,
        bonusScore :: Int,
        currentScore :: Int,
        members :: [GameObject],
        items :: [GameObject]
      }
  | -- 1. generates objects contained in <members> with <interval>, one at each time.
    -- 2. sticks to one of the still-alive troop members.
    -- 3. counts up <currentScore> every time when one of the squad members are destroyed by lack of hp.
    -- 4. doesn't count up <currentScore> if a squad member are destroyed by scrolling out.
    -- 5. dies when all squad members were destroyed. at this time,
    --        releases <items> if <currentScore> >= <bonusScore>, or
    --        doesn't ,if not.
    Jumper
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        velocity :: Complex GLdouble,
        hitDisp :: Shape,
        hp :: Int,
        age :: Int,
        hasItem :: Bool,
        gravity :: Complex GLdouble,
        touchedLand :: Bool,
        jumpCounter :: Int
      } -- dangerous multi way mine dispenser.
  | Grashia
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        velocity :: Complex GLdouble,
        hitDisp :: Shape,
        hp :: Int,
        age :: Int,
        hasItem :: Bool,
        gravity :: Complex GLdouble,
        gunVector :: Complex GLdouble,
        mode :: Int
      } -- fixed antiaircraft cannon.
  | Ducker
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        velocity :: Complex GLdouble,
        hitDisp :: Shape,
        hp :: Int,
        age :: Int,
        hasItem :: Bool,
        gVelocity :: Complex GLdouble,
        charge :: Int,
        vgun :: Complex GLdouble,
        touchedLand :: Bool
      } -- 2-feet mobile land to air attack device.
  | Flyer
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        velocity :: Complex GLdouble,
        hitDisp :: Shape,
        hp :: Int,
        age :: Int,
        hasItem :: Bool,
        mode :: Int
      } -- Baterian's standard interceptor.
  | ScrambleHatch
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        gateAngle :: GLdouble,
        gravity :: Complex GLdouble,
        hitDisp :: Shape,
        hp :: Int,
        age :: Int,
        launchProgram :: [[GameObject]]
      } -- Where Baterian larvae spend last process of maturation.
  | LandScapeBlock
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        hitDisp :: Shape,
        velocity :: Complex GLdouble
      } -- landscape that just look like, and hit like its hitDisp.
  | Particle
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        velocity :: Complex GLdouble,
        size :: GLdouble,
        particleColor :: Color3 GLdouble,
        age :: Int,
        decayTime :: GLdouble,
        expireAge :: Int
      } -- multi purpose particles that vanishes after expireAge.
  | Star
      { tag :: Maybe Int,
        position :: Complex GLdouble,
        particleColor :: Color3 GLdouble
      } -- background decoration
  | SabbathicAgent
      { tag :: Maybe Int,
        fever :: Int
      } -- generates many flyers for additional fun if there are none of them
  | DebugMessage
      { tag :: Maybe Int,
        debugMessage :: String
      }
  | ScoreFragment
      { tag :: Maybe Int,
        score :: Int
      }
