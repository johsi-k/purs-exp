module Main
  ( Item(..)
  , Label(..)
  , RPMT(..)
  , aesonEncoding
  , example
  , itemToJson
  , labelFromJson
  , labelToJson
  , main
  )
  where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Decode.Generic (genericDecodeJsonWith)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJsonWith)
import Data.Argonaut.Types.Generic as Gen
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- import Unsafe.Coerce (unsafeCoerce)

-- Item type and instances
data Item a = Leaf a
            | All (Maybe (Label a)) (Array (Item a))
            | Any (Maybe (Label a)) (Array (Item a))
            | Not (Item a)

derive instance eqItem :: (Eq a) => Eq (Item a)
derive instance genericItem :: Generic (Item a) _

instance showItem :: (Show a) => Show (Item a) where
  show eta = genericShow eta

instance encodeJsonItem :: EncodeJson a => EncodeJson (Item a) where
  encodeJson a = genericEncodeJsonWith aesonEncoding a
instance decodeJsonItem :: DecodeJson a => DecodeJson (Item a) where
  decodeJson a = genericDecodeJsonWith aesonEncoding a


-- Label type and instances
data Label a = Pre a
             | PrePost a a

derive instance eqLabel :: (Eq a) => Eq (Label a)
derive instance genericLabel :: Generic (Label a) _

instance showLabel :: (Show a) => Show (Label a) where
  show = genericShow

instance encodeJsonLabel :: EncodeJson a => EncodeJson (Label a) where
  encodeJson a = genericEncodeJsonWith aesonEncoding a
instance decodeJsonLabel :: DecodeJson a => DecodeJson (Label a) where
  decodeJson a = genericDecodeJsonWith aesonEncoding a


-- RPMT type and instances
data RPMT = RPMT (Array String)
          | RPMTDummy

derive instance eqRPMT :: Eq RPMT
derive instance genericRPMT :: Generic RPMT _
instance showRPMT :: Show RPMT where
  show = genericShow

instance encodeJsonRPMT :: EncodeJson RPMT where
  encodeJson = genericEncodeJsonWith aesonEncoding
instance decodeJsonRPMT :: DecodeJson RPMT where
  decodeJson = genericDecodeJsonWith aesonEncoding


-- modified default encoding for aeson compatibility
aesonEncoding :: Gen.Encoding
aesonEncoding =
  { tagKey: "tag"
  , valuesKey: "contents"
  , unwrapSingleArguments: true
  }


-- toy items
example :: Item RPMT
example =  All Nothing
           [ Leaf ( RPMT [ "a" ] )
           , Any Nothing
             [ Leaf ( RPMT [ "b" ] )
             , Leaf ( RPMT [ "c" ] )
             ]
           ]

-- examplenot :: Item RPMT
-- examplenot =  All Nothing
--               [ Leaf ( RPMT [ "a" ] )
--               , Any Nothing
--                 [ Leaf ( RPMT [ "b" ] )
--                 , Not
--                   ( Leaf ( RPMT [ "c" ] )
--                   )
--                 ]
--               ]


labelToJson :: Label String -> Json
labelToJson = encodeJson

labelFromJson :: Json -> Either JsonDecodeError (Label String)
labelFromJson = decodeJson

itemToJson :: Item RPMT -> Json
itemToJson = encodeJson

itemFromJson :: Json -> Either JsonDecodeError (Item RPMT)
-- itemFromJson = unsafeCoerce unit :: forall a. a
itemFromJson = decodeJson


main :: Effect Unit
main = do
  let fp = "/Users/johsi/purs-exp/src/input.json"

  str <- readTextFile UTF8 fp
  let decoded = itemFromJson =<< parseJson str
  log $ show decoded

  -- log $ show $ decoded == Right example

  -- toy example to JSON
  -- log $ stringify $ itemToJson example

  -- round trip with toy example
  -- let str = stringify $ itemToJson $ example
  -- log $ show $ itemFromJson =<< parseJson str

-- type reference
-- log :: String -> Effect Unit
-- readTextFile :: Encoding -> FilePath -> Effect String

-- parseJson :: String -> Either JsonDecodeError Json
-- itemFromJson :: Json -> Either JsonDecodeError (Item RPMT)
-- =<< :: (a -> m b) -> m a -> m b
-- =<< :: (Json -> Either JsonDecodeError (Item RPMT))
--     -> Either JsonDecodeError Json
--     -> Either JsonDecodeError (Item RPMT)
