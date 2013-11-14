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

data Unit = Degrees | Radians | Pixels
data Dim : Unit -> Type where
  Deg : Float -> Dim Degrees
  Rad : Float -> Dim Radians
  Pix : Float -> Dim Pixels

-- bare floats represent measurements in the world

toRad : Dim Degrees -> Dim Radians
toRad (Deg d) = Rad $ d * (pi / 180)

tan : Dim Radians -> Float
tan (Rad r) = tan r

record Scene : Type where
  MkScene : (objects : List Object) ->
            (lights : List Point) -> -- light sources
            (camera : Point) -> -- camera location
            (lookingAt : Point) ->
            (fieldOfView : Dim Degrees) ->
            (background : Color) ->
            Scene

width : Scene -> Float
width s = tan (toRad (fieldOfView s)) * 2

height : Scene -> Float
height = width



--trace : Ray -> Scene -> Color
--