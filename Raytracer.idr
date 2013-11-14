module Raytracer
import Vector
import PPM

%default total

data Point = P Float Float Float

(+) : Point -> Vector -> Point
(P x y z) + (V a b c) = P (x + a) (y + b) (z + c)

data Ray = R Point Vector





data Object : Type where
  Sphere : (center : Point) -> (radius : Float) -> Object

data Degrees = Deg Float
data Radians = Rad Float
data Pixels = Pix Int
-- bare floats represent measurements in the world

toRad : Degrees -> Radians
toRad (Deg d) = Rad $ d * (pi / 180)

tan : Radians -> Float
tan (Rad r) = tan r

record Scene : Type where
  MkScene : (objects : List Object) ->
            (lights : List Point) -> -- light sources
            (camera : Point) -> -- camera location
            (lookingAt : Point) ->
            (fieldOfView : Degrees) ->
            (background : Color) ->
            Scene

width : Scene -> Float
width s = tan (toRad (fieldOfView s)) * 2

height : Scene -> Float
height = width



--trace : Ray -> Scene -> Color
--