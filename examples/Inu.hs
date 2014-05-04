{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Lens
import Control.Lens.Inference

newtype Breed = Breed { unBreed :: String }
  deriving Show

data Colour = White | Red | Sesame
  deriving Show

newtype Age = Age { unAge :: Int }
  deriving (Show, Num)

data Inu = Inu { _breed :: Breed, _colour :: Colour, _age :: Age }
  deriving Show
makeInferableLenses ''Inu

kabosu :: Inu
kabosu = Inu (Breed "Shiba Inu") Red 6

kabosu_breed :: Breed
-- kabosu_breed = kabosu ^. breed
-- kabosu_breed = kabosu ^. inferLens
kabosu_breed = (^.?) kabosu

name :: Inu -> String
-- name x = "Kawaii " ++ unBreed (x ^. breed)
-- name x = "Kawaii " ++ unBreed (x ^. inferLens)
name x = "Kawaii " ++ unBreed ((^.?) x)

birthday :: Age -> Age
birthday (Age x) = Age (x + 1)

inu_birthday :: Inu -> Inu
-- inu_birthday = age %~ birthday
-- inu_birthday = inferLens %~ birthday
inu_birthday = (%~?) birthday

--
data Inko = Inko { _inkoAge :: Age }
  deriving Show
makeInferableLenses ''Inko

inkoChan = Inko 4

older :: IsInferable a Age Identity => a -> a
older x = birthday %~? x

main = do
  print kabosu
  print $ birthday %~? kabosu
  print inkoChan
  print $ birthday %~? inkoChan
