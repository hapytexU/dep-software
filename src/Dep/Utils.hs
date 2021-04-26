module Dep.Utils (
    toList'
  , applyExp, applyExp'
  ) where

import Language.Haskell.TH.Lib(appE, conE)
import Language.Haskell.TH.Syntax(Exp, Lift(lift), Name, Q)

toList' :: Foldable f => f a -> [a] -> [a]
toList' = flip (foldr (:))

applyExp :: Foldable f => Name -> f (Q Exp) -> Q Exp
applyExp = foldl appE . conE

applyExp' :: (Lift a, Foldable f) => Name -> f a -> Q Exp
applyExp' = foldl ((. lift) . appE) . conE

