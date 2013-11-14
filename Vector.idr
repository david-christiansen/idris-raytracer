module Vector

%default total


data Vector = V Float Float Float

dot : Vector -> Vector -> Float
dot (V x y z) (V x' y' z') = x * x' + y * y' + z * z'

magnitude : Vector -> Float
magnitude v = v `dot` v

cross : Vector -> Vector -> Vector
cross (V x y z) (V x' y' z') = V (y * z' + z * y')
                                 (-(x * z' + z * x'))
                                 (x * y' + y * x')

zero : Vector
zero = V 0.0 0.0 0.0

unit : Fin 3 -> Vector
unit fZ           = V 1 0 0
unit (fS fZ)      = V 0 1 0
unit (fS (fS fZ)) = V 0 0 1
unit (fS (fS (fS i))) = FalseElim (fin0empty i)
  where fin0empty : Fin 0 -> _|_
        fin0empty fZ impossible
        fin0empty (fS _) impossible

(+) : Vector -> Vector -> Vector
(V x y z) + (V x' y' z') = V (x + x') (y + y') (z + z')

(*) : Vector -> Float -> Vector
(V x y z) * i = V (i*x) (i*y) (i*z)

neg : Vector -> Vector
neg v = v * -1.0