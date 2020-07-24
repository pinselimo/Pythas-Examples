module Wonderland.Types   (
   WonderState (..) --TODO export everythin?
 , WonderParameters (
 --Economy
   gamma, gamma0, eta, lambda, tau
 --Environment
 , chi, chi0, delta, rho, omega, kappa, epsilon
 --Env. Policies
 , my, phi
 --Population
 , alpha, alpha0, alpha1, alpha2
 , beta, beta0, beta1, theta 
 ) 
 , zeroToOne, nonNegative, StateVariable (getVar), safeZTO
 , ZeroToOne(), NonNegative()
 , setParams, PopParams(Pop), EcoParams(Eco), EnvParams(Env)
 , dreamParams, horrorParams, state1990, dream1992, horror1992
 , horrorWithAbatement, horrorWithTaxes
 , list, last, add, new
) where

import Prelude hiding (last)

defaultState :: ZeroToOne -> WonderState
defaultState = WS (nonNegative 1)
                  (zeroToOne 1)
                  (nonNegative 1)
                    

state1990 :: WonderState
state1990 = WS (nonNegative 1)
               (zeroToOne 0.98)
               (nonNegative 1)
               (zeroToOne 1)

-- TYPES
class StateVariable a where
    getVar :: a -> Double
    
instance StateVariable NonNegative where
    getVar = getNonNegative
    
instance StateVariable ZeroToOne where
    getVar = getZeroToOne

newtype NonNegative = NonNegative {
     getNonNegative :: Double
     } deriving (Show, Eq, Ord)

nonNegative :: Double -> NonNegative
nonNegative d | d < 0     = NonNegative (- d)
              | otherwise = NonNegative d

newtype ZeroToOne = ZeroToOne {
    getZeroToOne :: Double
    } deriving (Show, Eq, Ord)
  
  
zeroToOne :: Double -> ZeroToOne
zeroToOne d | d < 0     = ZeroToOne 0.0
            | d > 1     = ZeroToOne 1.0
            | otherwise = ZeroToOne d

safeZTO :: Double -> ZeroToOne
safeZTO d | d <  0    = ZeroToOne 0.0
          | d >= 1    = ZeroToOne 0.9999999999999999 --least difference to 1
          | otherwise = ZeroToOne d
{-
zeroToOne :: Double -> ZeroToOne
zeroToOne d | valid     = ZeroToOne d
            | otherwise = error "Value out of bounds"
            where valid = d <= 1 && d >= 0
-}
data WonderState = WS {
     perCapitaOutput     :: NonNegative  --y(t) / I
   , naturalCapitalStock :: ZeroToOne    --z(t) / NK
   --, netPerCapitaOutput :: Double             / NI
   --, crudeBirthRate :: Double --b(y,z)        / BR
   --, crudeDeathRate :: Double --d(y,z)        / DR
   , population :: NonNegative           --x(t) / N
   --, pollutionFlow :: Double             --f(x,y,p) / PF
   , pollutionPerUnitOutput :: ZeroToOne --p(t) / T
   --, perCapitaPollutionControlExpenditures :: Double --c(y,z) / PC
   } deriving (Eq)
   
instance Show WonderState where
   show ws =      f (perCapitaOutput ws)      ++
             s ++ f (naturalCapitalStock ws)  ++
             s ++ f (population ws)           ++
             s ++ f (pollutionPerUnitOutput ws)
           where f :: StateVariable a => a -> String
                 f = show . getVar
                 s = ";"
              

data WonderParameters = WonderParameters { 
   --Economy
     gamma  :: Double --y
   , gamma0 :: Double --y
   , eta    :: Double --y
   , lambda :: Double --y
   , tau    :: Double --y Tax Rate
   --Environment
   , chi     :: Double --p
   , chi0    :: Double --p
   , delta   :: Double --z
   , rho     :: Double --z
   , omega   :: Double --z
   , kappa   :: Double --f
   , epsilon :: Double --f
   --Env. Policies
   , my      :: Double --c
   , phi     :: Double --c
   --Population
   , alpha  :: Double --d
   , alpha0 :: Double --d
   , alpha1 :: Double --d
   , alpha2 :: Double --d
   , beta   :: Double --b
   , beta0  :: Double --b
   , beta1  :: Double --b
   , theta  :: Double --d
   } deriving (Show, Eq)
   
newtype DList a = DList {
    getList :: [a] -> [a]
    }

app :: DList a -> DList a -> DList a
app xs ys = DList $ getList xs . getList ys

fromList :: [a] -> DList a
fromList xs = DList (xs ++)

toList :: DList a -> [a]
toList (DList xs) = xs []

empty :: DList a
empty = DList id

--not really needed
cons :: a -> DList a -> DList a
cons x (DList xs) = DList $ (x :) . xs

listAdd :: DList a -> a -> DList a
listAdd (DList xs) x = DList $ xs . ([x] ++)

newtype Store a = Store {
    getStore :: (DList a, a)
    }
    
new :: a -> Store a
new x = Store (fromList [x], x)
    
list :: Store a -> [a]
list (Store (dl, _)) = toList dl

last :: Store a -> a
last (Store (_, l)) = l

add :: Store a -> a -> Store a
add (Store (dl, _)) x = Store (listAdd dl x, x)
-- GENERATORS

setParams :: EcoParams -> EnvParams -> PopParams -> WonderParameters
setParams eco env pop = WonderParameters 
                        (ecoG eco) (ecoG0 eco) (ecoE eco) 
                        (ecoL eco) (ecoT  eco)
                        (envC env) (envC0 env) (envD env)
                        (envR env) (envO env)  (envK env)
                        (envE env) (envM env)  (envP env)
                        (popA pop) (popA0 pop) (popA1 pop) (popA2 pop) 
                        (popB pop) (popB0 pop) (popB1 pop) (popT  pop)

--TODO maybe export to another module to rename extractor funcs
data EcoParams = Eco {
     ecoG  :: Double
   , ecoG0 :: Double
   , ecoE :: Double
   , ecoL :: Double
   , ecoT :: Double
   } deriving (Show, Eq)

data EnvParams = Env {
     envC  :: Double
   , envC0 :: Double
   , envD :: Double
   , envR :: Double
   , envO :: Double
   , envK :: Double
   , envE :: Double
   , envM :: Double
   , envP :: Double
   } deriving (Show, Eq)

data PopParams = Pop {
     popA  :: Double
   , popA0 :: Double
   , popA1 :: Double
   , popA2 :: Double
   , popB  :: Double
   , popB0 :: Double
   , popB1 :: Double
   , popT  :: Double
   } deriving (Show, Eq)

-- EXAMPLE PARAMETERS

dreamParams :: WonderParameters
dreamParams =  setParams dreamEconomy dreamEnvironment dreamPopulation
 where dreamEconomy     = Eco 0.04 0 0.04 2.0 0     ----- -\
       dreamEnvironment = Env 0.04 0 1.0  0.2 0.1 0 0 0 0 
                          -- -> 0's are defaults
       dreamPopulation  = Pop 0.09 10.0 2.5 2.0 0.8 40.0 1.375 15.0

horrorParams :: WonderParameters
horrorParams = dreamParams { chi = 0.01 }

params1992NoAbatement :: WonderParameters
params1992NoAbatement = horrorParams { beta = 0.08, beta0 = 35, beta1 = 1.5
                         , alpha0 = 20, alpha1 = 1.5, alpha2 = 29}

horror1992 :: WonderParameters
horror1992 = params1992NoAbatement { kappa = 1, epsilon = 0.02
                                   , my = 2.0, phi = 0.5 }
                                   
dream1992 :: WonderParameters
dream1992 = params1992NoAbatement { kappa = 1, epsilon = 0.02
                                   , my = 2.0, phi = 0.5
                                   , chi = 0.04 }

horrorWithAbatement :: WonderParameters
horrorWithAbatement = horrorParams { kappa = 1
                                   , epsilon = 1
                                   , my = 1
                                   , phi = 1 }

horrorWithTaxes :: Double -> WonderParameters
horrorWithTaxes taxRate = horrorParams { chi0 = chi horrorParams / 2
                                       , gamma0 = 0.5
                                       , tau = taxRate }
          
