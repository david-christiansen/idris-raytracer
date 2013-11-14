module Units

%default total

data Unit = Degrees | Radians | Pixels | Meters
data Dim : Unit -> Type where
  Deg : Float -> Dim Degrees
  Rad : Float -> Dim Radians
  Pix : Float -> Dim Pixels
  M : Float -> Dim Meters