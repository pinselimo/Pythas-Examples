module Wonderland.IFace (
 listResult,
 result
 ) 
 where

import Wonderland.Types (WonderState(..), WonderParameters(..), 
              horrorParams, dreamParams, state1990, getVar,
              dream1992, horror1992)
import qualified Wonderland.Types as T (last, list, add, new)
import qualified Wonderland.Funcs as F (population, perCapitaOutput,
                             pollutionPerUnitOutput,
                             naturalCapitalStock)

getStep :: WonderParameters -> WonderState -> WonderState
getStep ps st = 
             let 
              pop  = F.population ps st
              pco  = F.perCapitaOutput ps st
              ppuo = F.pollutionPerUnitOutput ps st
              ncs  = F.naturalCapitalStock ps st
             in
                 st { population             = pop
                    , perCapitaOutput        = pco
                    , pollutionPerUnitOutput = ppuo
                    , naturalCapitalStock    = ncs
                    }
                    
loop :: (WonderState -> WonderState) -> WonderState -> Int -> [WonderState]
loop f st count = T.list $ loop' (T.new st) count f
   where
    loop' xs c f  | c > 0     = loop' (T.add xs $ f st) (c - 1) f
                  | otherwise = xs
                  where st    = T.last xs

loopList :: (WonderState -> WonderState) -> WonderState -> Int -> [WonderState]
loopList f st count =  loop' [st] count f
   where
    loop' xs@(st:_) c f | c > 0     = loop' (f st : xs) (c - 1) f
                        | otherwise = xs

end :: (WonderState -> WonderState) -> WonderState -> Int -> WonderState
end f = f' 
    where f' :: WonderState -> Int -> WonderState
          f' s 0 = s
          f' s c = f' (f s) (c-1)

listResult :: WonderParameters -> WonderState -> Int -> [WonderState]
listResult = loop . getStep

result :: WonderParameters -> WonderState -> Int -> WonderState
result = end . getStep
