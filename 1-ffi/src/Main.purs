module Main where

import Prelude (Unit, negate, ($), (<$>), (#), (<#>), bind, (>>>))
import Data.Int (toNumber)
import Data.Tuple
import Data.Maybe (Maybe(..))
import Data.Array (zip, (..), length)

import Control.Monad.Eff (Eff)

import DOM (DOM)
import DyGraphs (DyData(Array2D, CSV), defaultDyOpts, newDyGraph, RunDyGraph)

import Util.DOM (tryWithNode, setText)

series1 :: Array Number
series1 = [ 106.1616296,98.92188981,96.47125453,96.46867633,101.455799,102.2368421,114.709233,107.0379752,101.3964413,103.9022896,92.38084853,105.2355647,87.15531588,119.241544,111.0962582,97.41282225 ]
series2 :: Array Number
series2 = [ 61.87022328,112.2162175,55.26382685,89.23769414,88.73319089,111.9717193,66.11845016,124.9046659,107.2487974,64.06293035,76.29163772,43.25612068,82.22805411,53.40558767,79.04625267,55.79522371 ]
--plotData :: DyData
--plotData = Array2D $ [ [0.0,  0.0 ,  0.0 ,  0.0]
--                     , [0.1,  0.1 , -0.2 ,  0.3]
--                     , [0.2, -0.1 ,  0.2 ,  0.3]
--                     , [0.3,  0.15, -0.25, -0.3]
--                     ]

toDyGraph :: Array Number -> Array Number -> DyData
toDyGraph s1 s2 = zip (1 .. length s1) (zip s1 s2)
  <#> (\(Tuple i (Tuple e1 e2)) -> [toNumber i, e1, e2])
   #  Array2D
--toDyGraph s1 s2 = Array2D $ (\(Tuple i (Tuple e1 e2)) -> [toNumber i, e1, e2]) <$> zip (1 .. length s1) (zip s1 s2)

main :: forall eff. Eff ( dom :: DOM, runDyGraph :: RunDyGraph | eff ) Unit
main = do
  tryWithNode "#graph" $ \d -> newDyGraph d (toDyGraph series1 series2) defaultDyOpts
      { title = Just "Data"
      , xlabel = Just "Value"
      , ylabel = Just "Day"
--      , showRoller = Just true
--      , errorBars = Just true
--      , stepPlot = Just true
--      , clickCallback = Just $ \e x pts -> do
--          tryWithNode "#msg" $ setText "Canvas clicked!"
--      , highlightCallback = Just $ \e x pts r s -> do
--          tryWithNode "#msg" $ setText "Point highlighted!"
      }
