module Raytracer
import Vector

%default total

data Point = P Float Float Float

(+) : Point -> Vector -> Point
(P x y z) + (V a b c) = P (x + a) (y + b) (z + c)

data Ray = R Point Vector

data Color = C Bits8 Bits8 Bits8


data PPM : Nat -> Nat -> Type where
  MkPPM: (x, y: Nat) ->
         (image : Vect x (Vect y Color)) ->
         PPM x y

data VectZipper : Nat -> Nat -> Type -> Type where
  VZ : (left : Vect n a) -> (right : Vect m a) -> VectZipper n m a

toVect : VectZipper n m a -> Vect (n + m) a
toVect (VZ l r) = reverse l ++ r

goLeft : VectZipper (S n) m a -> VectZipper n (S m) a
goLeft (VZ (x :: l) r) = VZ l (x :: r)

goRight : VectZipper n (S m) a -> VectZipper (S n) m a
goRight (VZ l (x :: r)) = VZ (x :: l) r

zipper : Vect n a -> VectZipper 0 n a
zipper v = VZ [] v

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