module PPM

%default total

record Color : Type where
  C : (red, green, blue : Bits8) -> Color

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

instance Cast Bits8 Int where
  cast = prim__zextB8_Int

toString : PPM n m -> String
toString (MkPPM x y image) = header ++ concatMap {t=Vect x} row image
  where header : String
        header = concat (the (List String)
                             [ "P3\n"
                             , show x, " "
                             , show y, "\n"
                             ])
        component : Bits8 -> String
        component b = show (cast {to=Int} b)
        pixel : Color -> String
        pixel (C r g b) = component r ++ " " ++
                          component g ++ " " ++
                          component b ++ "\n"
        row : Vect n Color -> String
        row = concatMap pixel