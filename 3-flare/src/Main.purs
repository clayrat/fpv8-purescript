module Main where

import Prelude (Unit, (<$>), (<#>), (<*>), (#), ($), bind, show, (-), (+), (*), (/), (++), compare)
import Data.Int (toNumber)
import Data.Maybe.Unsafe (fromJust)
import Data.Array ((..), zip, length, sortBy, unzip)
import Data.Tuple (Tuple(..), fst, snd)
import Data.String (take)
import Data.Foldable (maximum, mconcat)
import Control.Monad.Eff (Eff)
import Signal.Channel (CHANNEL)

import DOM (DOM)
import Text.Smolder.HTML5.SVG as S
import Text.Smolder.Markup (Markup() ,(!), text)

import Flare (UI(), boolean)
import Flare.Smolder (runFlareHTML)

names :: Array String
names = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"]

values :: Array Number
values = [0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02288, 0.02015, 0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749, 0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758, 0.00978, 0.02360, 0.00150, 0.01974, 0.00074]

m :: Number
m = fromJust $ maximum values

xaxis :: Array String -> Markup
xaxis ns = S.g ! S.class' "x axis" ! S.transform "translate(0,450)" $ do
  zip (0 .. length ns) ns <#> drawTick # mconcat
  where
  drawTick (Tuple i s) = S.g ! S.class' "tick" ! S.transform ("translate(" ++ (show $ 25.5 + toNumber (i*34)) ++ ",0)") ! S.style "opacity: 1;" $ do
    S.line ! S.y2 "6" ! S.x2 "0"
    S.text' ! S.dy ".71em" ! S.y "9" ! S.x "0" ! S.style "text-anchor: middle;" $ text s

yaxis :: Number -> Int -> Markup
yaxis top is = S.g ! S.class' "y axis" $ do
  (0 .. is) <#> drawTick # mconcat
  where
  drawTick i = S.g ! S.class' "tick" ! S.transform ("translate(0," ++ (show $ 450.0 - (toNumber i)*35.4274917336) ++ ")") ! S.style "opacity: 1;" $ do
    S.line ! S.x2 "-6" ! S.y2 "0"
    S.text' ! S.dy ".32em" ! S.x "-12" ! S.y "0" ! S.style "text-anchor: end;" $ text $ take 5 $ show $ (toNumber i) * top / (toNumber is)

barchart :: Boolean -> Boolean -> Markup
barchart sort abs = S.svg ! S.width "960" ! S.height "500" $ do
  S.g ! S.transform "translate(40,20)" $ do
    xaxis ns
    yaxis top 13
    zip (0 .. length vs) vs <#> drawBar # mconcat
  where
  top = if abs then 1.0 else m
  ratio = if abs then 450.0 * m else 450.0 / m
  drawBar (Tuple i v) = S.rect ! S.class' "bar" ! S.width (show 31) ! S.x (show (10+i*34)) ! S.y (show (450.0 - ratio * v)) ! S.height (show (ratio * v))
  nv = if sort then
        zip names values # sortBy (\(Tuple _ v1) (Tuple _ v2) -> compare v2 v1) # unzip
       else Tuple names values
  ns = fst nv
  vs = snd nv

ui :: forall eff. UI eff Markup
ui = barchart <$> boolean "sorted" false <*> boolean "absolute" false

main :: forall eff. Eff(dom :: DOM, channel :: CHANNEL | eff) Unit
main = runFlareHTML "controls" "output" ui
