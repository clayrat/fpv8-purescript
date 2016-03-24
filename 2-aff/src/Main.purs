module Main where

import Prelude (Unit, ($), (#), (<#>), (++), (<>), (>>>), (>>=), bind, id, const, show, pure, (+), (-), (/), (>), negate)
import Data.Int (round)
import Data.Array (catMaybes, partition, replicate, length, zip)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(), fromMaybe)
import Data.Either (either)
import Data.Foldable (foldl)
import Data.String (joinWith)
import Data.StrMap (lookup)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff (launchAff, liftEff')

import Network.HTTP.Affjax as AJ
import Data.Argonaut.Core (Json, toObject, toArray, toNumber)
import DOM (DOM)
import DOM.File.Types (Blob)

import Util.DOM

type Point = { x :: Int, y :: Int, s :: Int }

emptyPoint :: Point
emptyPoint = {x:0, y:0, s:0}

extractPoints :: Json -> Array Point
extractPoints j = (do
  obj <- toObject j
  arr <- lookup "features" obj
  toArray arr) # fromMaybe [] <#> extractPoint # catMaybes
  where
  extractPoint :: Json -> Maybe Point
  extractPoint j = do
    obj <- toObject j
    prop <- lookup "properties" obj
    obj2 <- toObject prop
    x <- lookup "long_x" obj2 >>= toNumber
    y <- lookup "lat_y" obj2 >>= toNumber
    s <- lookup "elevation" obj2 >>= toNumber
    pure $ { x: (180 + round x) / 4, y: (90 + round y) / 2, s: round s }

splitPoints :: Array Point -> Tuple (Array Point) (Array Point)
splitPoints ps = Tuple high low
  where
  p2 = partition (\p -> p.s > 0) ps
  high = p2.yes <#> (\p -> p {s = p.s / 100})
  low = (p2.no <#> (\p -> p {s = negate p.s})) <> replicate (length p2.yes - length p2.no) emptyPoint

renderPoints :: forall e. String -> String -> Tuple (Array Point) (Array Point) -> AJ.Affjax e Blob
renderPoints n1 n2 a12 = AJ.get $ "http://chart.googleapis.com/chart?chs=547x547&cht=s&chxt=x,y&chd=t:"
                                  ++ strPoints a12 ++ "&chco=FF0000|0000FF&chdl=" ++ n1 ++ "|" ++ n2
  where
  strPoints :: Tuple (Array Point) (Array Point) -> String
  strPoints (Tuple a1 a2) = zip a1 a2
    # foldl (\t (Tuple p1 p2) -> {xs: t.xs<>[p1.x,p2.x], ys: t.ys<>[p1.y,p2.y], ss: t.ss<>[p1.s,p2.s]}) {xs: [], ys: [], ss: []}
    # \t -> [t.xs, t.ys, t.ss] <#> str # joinWith "|"
  str a = a <#> show # joinWith ","

main :: forall eff. Eff ( err :: EXCEPTION, ajax :: AJ.AJAX, dom :: DOM | eff ) Unit
main = launchAff $ do
  d <- getData
  r <- d.response # extractPoints >>> splitPoints >>> renderPoints "Elevation" "Depression"
  urlE <- liftEff' $ blob2url r.response
  liftEff' $ tryWithNode "#chart" $ setSrc $ either (const "bad") id urlE
  where
  getData :: forall e. AJ.Affjax e Json
  getData = AJ.get "https://d2ad6b4ur7yvpq.cloudfront.net/naturalearth-3.3.0/ne_50m_geography_regions_elevation_points.geojson"
