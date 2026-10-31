{- |
Copyright: (c) 2026 D. Sunshine
SPDX-License-Identifier: MIT
Maintainer: D. Sunshine <david@sunshines.org>

See README for more info
-}

module GCode where 

import Text.Printf
import Linear.V2
import Linear.V3
import Control.Lens.Operators ((&))
import Control.Lens.Setter ((.~))
import Turtle
import Control.Monad.RWS.Lazy (MonadState(get))
import TurtleState

data Print = Print [GCode] [GCode] [GCode]

data Bed = Bed (V3 Double) (V3 Double)

data GCode = MoveXY Double (V2 Double)
  | MoveXYZ Double (V3 Double)
  | Message String
  | Model   String
  | Comment String
  | Relative
  | Absolute
  | Home
  | Milli
  | Origin
  | Off

instance Show GCode where
  show (MoveXY f (V2 x y))    = printf "G0 X%0.2f Y%0.2f F%0.2f" x y f
  show (MoveXYZ f (V3 x y z)) = printf "G0 X%0.2f Y%0.2f Z%0.2f F%0.2f" x y z f
  show (Message m)            = printf "M117 %s" m
  show (Model   m)            = printf "M862.3 P \"%s\" ; Printer model check" m
  show (Comment c)            = printf "; %s" c
  show Absolute               = "G90   ; Set all axes to absolute"
  show Relative               = "G91   ; Set all axes to relative"
  show Home                   = "G28 W ; Home all without mesh bed level"
  show Milli                  = "G21   ; Set units to millimeters"
  show Off                    = "M84   ; Disable motors"
  show Origin                 = "G92 X0 Y0 ; Set current position to origin"

defaultFeed :: Double
defaultFeed = 1000.0

penUp :: Double -> GCode
penUp d = moveXYZ $ V3 0.0 0.0 d

moveXY :: V2 Double -> GCode
moveXY = MoveXY defaultFeed

moveXYZ :: V3 Double -> GCode
moveXYZ = MoveXYZ defaultFeed

message :: String -> GCode
message = Message

model :: String -> GCode
model = Model

comment :: String -> GCode
comment = Comment

absolute :: GCode
absolute = Absolute

relative :: GCode
relative = Relative

home :: GCode
home = Home

milli :: GCode
milli = Milli

off :: GCode
off = Off

origin :: GCode
origin = Origin

trace :: [V3 Double] -> [GCode]
trace = map moveXYZ

initialP :: Bed -> Print
initialP b = Print header body footer
  where
    header = [model "MK3S", Milli, Absolute, Home, (moveXYZ $ offset b), toPaper b, Origin, Relative]
    body   = []
    footer = [penUp 100.0, Off]

offset :: Bed -> V3 Double
offset (Bed (V3 x1 y1 _) (V3 x2 _ z2)) = V3 (x1 + (x2 - x1) / 2) y1 z2

fromTurtle :: Bed -> Turtle Double () -> Print
fromTurtle b t = addAll p' xs
  where
    p' = initialP b
    t' = runTurtle t
    xs = trace $ path t'

addAll :: Print -> [GCode] -> Print
addAll (Print h b f) g = Print h (b <> g) f

prusa :: Bed
prusa = bed 39.5 66.0 5.8 253.0 212.0 10.8

bed :: Double -> Double -> Double -> Double -> Double -> Double -> Bed 
bed x1 y1 z1 x2 y2 z2 = Bed (V3 x1 y1 z1) (V3 x2 y2 z2)

toPaper :: Bed -> GCode
toPaper b = moveXYZ $ o & _z .~ (bottom b)
  where
    o = offset b

bottom :: Bed -> Double
bottom (Bed (V3 _ _ z1) _) = z1

instance Show Print where
  show (Print h b f) = unlines $ map show $ h <> b <> f
