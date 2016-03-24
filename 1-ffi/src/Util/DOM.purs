module Util.DOM ( tryWithNode, setText ) where

import Prelude (Unit, unit, ($), (++), void, bind, pure)

import Data.Maybe (Maybe(..))
import Data.Function (Fn3, runFn3)

import Control.Monad.Eff (Eff)
import Debug.Trace (trace)

import DOM (DOM)
import DOM.Node.Types (Node())

tryWithNode :: forall eff a. String -> (Node -> Eff ( dom :: DOM | eff ) a ) -> Eff ( dom :: DOM | eff ) Unit
tryWithNode divName fun = do
  maybeDiv <- querySelector divName
  case maybeDiv of
    Just div -> void $ fun div
    Nothing -> pure $ trace ("Cannot find node " ++ divName) (\_ -> unit)

foreign import querySelectorImpl :: forall eff r. Fn3 r (Node -> r) String (Eff (dom :: DOM | eff) r)

querySelector :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe Node)
querySelector s = runFn3 querySelectorImpl Nothing Just s

foreign import setText :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node
