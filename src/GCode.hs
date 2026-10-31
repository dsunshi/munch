{- |
Copyright: (c) 2026 D. Sunshine
SPDX-License-Identifier: MIT
Maintainer: D. Sunshine <david@sunshines.org>

See README for more info
-}

module GCode where 

import Text.Printf

data Print = Print [GCode] [GCode] [GCode]

data GCode = Up Double Double
  | Down Double Double
  | MoveXY Double Double Double
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
  show (Down z f) | z >= 0.0  = printf "G0 Z%0.2f F%0.2f" (-z) f
  show (Down z _) | otherwise = printf "; [WARNING] Suspicious down value: %0.2f" z
  show (Up z f) | z >= 0.0    = printf "G0 Z%0.2f F%0.2f" z f 
  show (Up z _) | otherwise   = printf "; [WARNING] Suspicious up value: %0.2f" z
  show (MoveXY x y f)         = printf "G0 X%0.2f Y%0.2f F%0.2f" x y f
  show (Message m)            = printf "M117 %s" m
  show (Model   m)            = printf "M862.3 P %s ; Printer model check" m
  show (Comment c)            = printf "; %s" c
  show Absolute               = "G90   ; Set all axes to absolute"
  show Relative               = "G91   ; Set all axes to relative"
  show Home                   = "G28 W ; Home all without mesh bed level"
  show Milli                  = "G21   ; Set units to millimeters"
  show Off                    = "M84   ; Disable motors"
  show Origin                 = "G92 X0 Y0 ; Set current position to origin"

defaultFeed :: Double
defaultFeed = 1000.0

down :: Double -> GCode
down = Down defaultFeed

up :: Double -> Double -> GCode
up = Up

moveXY :: Double -> Double -> Double -> GCode
moveXY = MoveXY

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

initial :: Print
initial = Print header body footer
  where
    header = []
    body   = []
    footer = []
