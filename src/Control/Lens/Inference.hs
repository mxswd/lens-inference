{-# LANGUAGE KindSignatures, TemplateHaskell, ConstraintKinds, TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
module Control.Lens.Inference where

import Control.Lens
import Language.Haskell.TH

import Control.Applicative (Applicative)
import GHC.Exts (Constraint)
import Data.List (find)
import Control.Monad (forM, join)

class IsInferable a b f where
  inferLens :: (b -> f b) -> a -> f a

-- Remove in GHC 7.10
-- type family LensFunctor a (f :: * -> *) :: Constraint

-- operators
infixr 4 %~?
(%~?) :: IsInferable a b Identity => (b -> b) -> a -> a
(%~?) = (%~) inferLens

infixr 4 ^.?
(^.?) :: IsInferable a b (Const b) => a -> b
(^.?) x = (^.) x inferLens

infixr 4 .~?
(.~?) :: IsInferable a b Identity => b -> a -> a
(.~?) = (.~) inferLens

-- the magic
makeInferableLenses :: Name -> Q [Dec]
makeInferableLenses nm = do
  ls <- makeLenses nm
  r <- reify nm
  case r of
    (TyConI (DataD _ n _ cs _)) -> do
      ts <- fmap join $ mapM tysOf cs
      let cx = case ts of
                [_] -> ''Functor
                _   -> ''Applicative
      
      tyF <- newName "f"
      -- tyInst <- tySynInstD ''LensFunctor (tySynEqn [conT n, varT tyF] (appT (conT cx) (varT tyF)))
      
      inst <- forM ts $ \t -> do
        f <- newName "f"
        instanceD (cxt [classP ''Functor [varT f]])
                  (appT (appT (appT (conT ''IsInferable) (conT n)) (return t)) (varT f))
                  [fromN t ls]
      return (ls ++ inst)
    _ -> fail "only inferable on constructors"

  where
    tysOf :: Con -> Q [Type]
    tysOf (NormalC _ xs) = return $ map snd xs
    tysOf (RecC _ xs) = return $ map (\(_, _, x) -> x) xs
    tysOf _ = fail "only supports normal or record constructors"
  
    fromN :: Type -> [Dec] -> Q Dec
    fromN a ls = case d of
        Just (SigD n _) -> funD 'inferLens [clause [] (normalB (varE n)) []]
        _ -> fail "makeLenses' derived something I can't use"
      where
        d = flip find ls $ \x -> case x of
          (SigD _ (ForallT [] [] (AppT _ t))) -> case t of
            (AppT _ t') -> t' == a -- isos
            _           -> t  == a -- normal lens
          _ -> False
