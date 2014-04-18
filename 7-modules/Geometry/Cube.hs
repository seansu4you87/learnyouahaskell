module Geometry.Cube
( volume
, area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume s = cuboidVolume s s s

area :: Float -> Float
area s = cuboidArea s s s
