-- File auto generated by purescript-bridge! --
module GHC.Base where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Prim (Array)

import Prelude

data NonEmpty a =
    Cons a (Array a)

derive instance genericNonEmpty :: Generic a ra => Generic (NonEmpty a) _
