module Raytracer
import Units
import Vector
import PPM

%default total

data Point = P (Dim Meters) (Dim Meters) (Dim Meters)

data Ray = R Point Vector


(+) : Point -> Vector -> Point
(P (M x) (M y) (M z)) + (V a b c) = P (M (x + a)) (M (y + b)) (M (z + c))

data Object : Type where
  Sphere : (center : Point) -> (radius : Dim Meters) -> Object

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