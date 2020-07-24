module Wonderland.Funcs
 where

import Wonderland.Types hiding (WonderState(..))
import qualified Wonderland.Types as T (WonderState(..))

type WonderFunc a = WonderParameters -> T.WonderState -> a

------- POPULATION ---------

--x
population :: WonderFunc NonNegative
population ps st = nonNegative $ pop * (1 + populationGrowth o c ps)
    where o   = getVar $ T.perCapitaOutput st
          c   = getVar $ T.naturalCapitalStock st
          pop = getVar $ T.population st

--Endegenous population growth / n
populationGrowth :: Double -> Double -> WonderParameters -> Double
populationGrowth o c p = (birthRate o p - deathRate o c p) / 1000

--crude birth rate / b
birthRate :: Double -> WonderParameters -> Double
birthRate o ps = b0 * (b1 - e // (1 + e) )
 where e  =  exp $ beta ps * o
       b0 = beta0 ps
       b1 = beta1 ps

--crude death rate / d
deathRate :: Double -> Double -> WonderParameters -> Double
deathRate o c ps = a0 * (1 + a2 * (1 - c) ** t) * ( a1 - e // (1 + e) )
 where e   = exp $ alpha ps * o
       a0  = alpha0 ps
       a1  = alpha1 ps
       a2  = alpha2 ps
       t   = theta  ps

---------- ECONOMY ------------

--y
perCapitaOutput :: WonderFunc NonNegative
perCapitaOutput ps st
    = nonNegative  $ y * (1 + g - (g + e) * (1 - z) ** l - g0 * t / (1 - t))
    where y = getVar $ T.perCapitaOutput st
          z = getVar $ T.naturalCapitalStock st
          g = gamma  ps
          e = eta    ps
          l = lambda ps
          t = tau    ps
          g0 = gamma0 ps

--netto output
--Never actually used
netPerCapitaOutput :: WonderFunc Double
netPerCapitaOutput p st = o - pc
    where o  = getVar $ T.perCapitaOutput st
          z  = getVar $ T.naturalCapitalStock st
          pc = perCapitaPollutionControlExpenditures p o z

----------- ENVIRONMENT --------
       
--p  -> Exogenous
pollutionPerUnitOutput :: WonderFunc ZeroToOne
pollutionPerUnitOutput ps st = zeroToOne $ p * (1 - c - c0 * (t/(1+t)))
 where p  = getVar $ T.pollutionPerUnitOutput st
       c  = chi  ps
       c0 = chi0 ps
       t  = tau  ps

--z
------- ORIGINAL -------

naturalCapitalStock :: WonderFunc ZeroToOne
naturalCapitalStock ps st =  
   let
          g :: Double --g
          g  = (z /* (1 - z)) * exp (d * z ** r - o * f)
             where r = rho   ps
                   o = omega ps
                   d = delta ps
                   f = pollutionFlow ps st
                   
                   z = getVar $ T.naturalCapitalStock st
                   
    -- z is turned from 1.0 to 0.9999999 before calculation
    --   so it can't get stuck on 1.0 (see Types.hs -> safeZTO)
                   zSafe = getVar $ safeZTO z
   
   in zeroToOne $ g // (1 + g)

----- TEST --------
-- naturalCapitalStock :: WonderFunc ZeroToOne
-- naturalCapitalStock ps st =  
--    let
--           z :: Double --z
--           z = getVar $ T.naturalCapitalStock st
--           zSafe = getVar $ safeZTO z
--           g :: Double --g
--           g  = exp (d * z ** r - o * f)
--              where r = rho   ps
--                    o = omega ps
--                    d = delta ps
--                    f = pollutionFlow ps st
                   
                   
                   
--     -- z is turned from 1.0 to 0.9999999 before calculation
--     --   so it can't get stuck on 1.0 (see Types.hs -> safeZTO)
--                    zSafe = getVar $ safeZTO z
--    --in zeroToOne $ zSafe*g/(1-z+z*g) -- from discrete
--    --in zeroToOne $ z + z * g/(1-z+z*g) - z -- no use of zSafe needed in this variant WTF!? TODO!
--    --in zeroToOne $ z + z * g - z * (1-z+z*g)  -- should be equal to from analytical
--    in zeroToOne $ z + z*g - z + z*z - z*z*g -- from analytical 

--Implementation of 1992
--includes pollution abatement
pollutionFlow :: WonderParameters -> T.WonderState -> Double
pollutionFlow ps st = 
     let
       x  = getVar $ T.population st
       y  = getVar $ T.perCapitaOutput st
       z  = getVar $ T.naturalCapitalStock st
       p  = getVar $ T.pollutionPerUnitOutput st
       e  = exp $ epsilon ps
                      * perCapitaPollutionControlExpenditures ps y z
                      * x
     in 
       
       x * y * p - k * (e // (1+e))
       
       where k  = kappa ps -- switch for pollution control
             

------------ ENVIRONMENTAL POLICY ------

--c (y,z)
perCapitaPollutionControlExpenditures :: WonderParameters ->
                                         Double -> Double -> Double
perCapitaPollutionControlExpenditures ps y z =
    p * (1 - z) ** m * y
    where p = phi ps
          m = my  ps

------------ PLOTTING -------------

--r
realGrowthRate :: WonderParameters -> T.WonderState -> Double
realGrowthRate ps st = (y' - y) / (g * y)
 where y  = getVar $ T.perCapitaOutput st
       y' = getVar $ perCapitaOutput ps st
       g  = gamma ps

--(//) :: (Eq a, Fractional a) => a -> a -> a
--(//) :: RealFloat a => a -> a -> a
a // b | infinite a && infinite b = 1
       | otherwise                = a / b
infixl 7 //

a /* b | b == 0    = fromIntegral (maxBound :: Int)
    -- | b == 0    = exp 50 --just some guessed big value
       | otherwise = a / b

--infinite :: (Eq a, Fractional a) => a -> Bool
--infinite d = d == (1/0)

infinite = isInfinite 

a :: Double
a = 1.0e-31
