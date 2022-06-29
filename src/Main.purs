module Main
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
import Data.Either (Either(..), fromRight)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- import Unsafe.Coerce (unsafeCoerce)


-- Item type and instances
data Item lbl a
  = Leaf a
  | All lbl (Array (Item lbl a))
  | Any lbl (Array (Item lbl a))
  | Not (Item lbl a)

type ItemJSONStr = Item (Label String) String

derive instance eqItem :: (Eq lbl, Eq a) => Eq (Item lbl a)
derive instance genericItem :: Generic (Item lbl a) _

instance showItem :: (Show lbl, Show a) => Show (Item lbl a) where
  show eta = genericShow eta

instance encodeJsonItem :: (EncodeJson lbl, EncodeJson a) => EncodeJson (Item lbl a) where
  encodeJson a = genericEncodeJsonWith aesonEncoding a
instance decodeJsonItem :: (DecodeJson lbl, DecodeJson a) => DecodeJson (Item lbl a) where
  decodeJson a = genericDecodeJsonWith aesonEncoding a


-- Label type and instances
data Label a
  = Pre a
  | PrePost a a

derive instance eqLabel :: (Eq a) => Eq (Label a)
derive instance genericLabel :: Generic (Label a) _

instance showLabel :: (Show a) => Show (Label a) where
  show = genericShow

instance encodeJsonLabel :: EncodeJson a => EncodeJson (Label a) where
  encodeJson a = genericEncodeJsonWith aesonEncoding a
instance decodeJsonLabel :: DecodeJson a => DecodeJson (Label a) where
  decodeJson a = genericDecodeJsonWith aesonEncoding a


-- modified default encoding for aeson compatibility
aesonEncoding :: Gen.Encoding
aesonEncoding =
  { tagKey: "tag"
  , valuesKey: "contents"
  , unwrapSingleArguments: true
  }


-- toy example
inline1m :: ItemJSONStr
inline1m
  = Any
  ( PrePost "any unauthorised" "of personal data" )
  [ Leaf "access"
  , Leaf "use"
  , Leaf "disclosure"
  , Leaf "copying"
  , Leaf "modification"
  , Leaf "disposal"
  ]


-- labelToJson :: Label String -> Json
-- labelToJson = encodeJson

-- labelFromJson :: Json -> Either JsonDecodeError (Label String)
-- labelFromJson = decodeJson

-- itemToJson :: Item String -> Json
-- itemToJson = encodeJson

-- itemFromJson :: Json -> Either JsonDecodeError (Item String)
-- -- itemFromJson = unsafeCoerce unit :: forall a. a
-- itemFromJson = decodeJson

itemFromJson :: Json -> Either JsonDecodeError ItemJSONStr
itemFromJson = decodeJson


main :: Effect Unit
main = do
  let fp = "/Users/johsi/purs-exp/src/inline1m-hask.json"

  str <- readTextFile UTF8 fp
  let decoded = itemFromJson =<< parseJson str
  log $ show decoded

  -- log $ show $ decoded == Right inline1m

  -- toy example to JSON
  -- log $ stringify $ itemToJson inline1m

  -- round trip with toy example
  -- let str = stringify $ itemToJson $ inline1m
  -- log $ show $ itemFromJson =<< parseJson str

-- type reference
-- log :: String -> Effect Unit
-- readTextFile :: Encoding -> FilePath -> Effect String

-- parseJson :: String -> Either JsonDecodeError Json
-- itemFromJson :: Json -> Either JsonDecodeError ItemJSONStr
-- =<< :: (a -> m b) -> m a -> m b
-- =<< :: (Json -> Either JsonDecodeError ItemJSONStr)
--     -> Either JsonDecodeError Json
--     -> Either JsonDecodeError ItemJSONStr
