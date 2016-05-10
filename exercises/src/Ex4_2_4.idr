module Ex4_2_4

data PowerSource = Petrol | Pedal | Electricity

data Vehicule : PowerSource -> Type where
  Unicycle : Vehicule Pedal
  Bicycle : Vehicule Pedal
  ElectricCar : Vehicule Electricity
  MotorCycle : (fuel : Nat) -> Vehicule Petrol
  Car : (fuel : Nat) -> Vehicule Petrol
  Bus : (fuel : Nat) -> Vehicule Petrol

wheels : Vehicule power -> Nat
wheels Unicycle = 1
wheels Bicyle = 2
wheels ElectricCar = 4
wheels (MotorCycle _) = 2
wheels (Car _) = 4
wheels (Bus _) = 4

refuel : Vehicule Petrol -> Vehicule Petrol
refuel (MotorCycle _) = MotorCycle 30
refuel (Car _) = Car 100
refuel (Bus _) = Bus 200
refuel Unicycle impossible
refuel Bicycle impossible
refuel ElectricCar impossible

take : (m : Fin n) -> Vect n a -> Vect (finToNat m) a
take FZ v = Nil
take (FS pm) (x::xs) = x::(take pm xs)

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos v1 v2 =
  case integerToFin pos n of
       Nothing => Nothing
       (Just i) => Just (index i v1 + index i v2)

sumEntriesDo : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntriesDo {n} pos v1 v2 = do
  i <- integerToFin pos n
  return $ index i v1 + index i v2
