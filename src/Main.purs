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


-- BinExpr type and instances
data BinExpr a b =
    BELeaf a
  | BEAll b (Array (BinExpr a b))
  | BEAny b (Array (BinExpr a b))
  | BENot (BinExpr a b)

derive instance eqBinExpr :: (Eq a, Eq b) => Eq (BinExpr a b)
derive instance genericBinExpr :: Generic (BinExpr a b) _
instance showBinExpr :: (Show a, Show b) => Show (BinExpr a b) where
  show eta = genericShow eta

instance encodeJsonBinExpr :: (EncodeJson a, EncodeJson b) => EncodeJson (BinExpr a b) where
  encodeJson a = genericEncodeJsonWith aesonEncoding a
instance decodeJsonBinExpr :: (DecodeJson a, DecodeJson b) => DecodeJson (BinExpr a b) where
  decodeJson a = genericDecodeJsonWith aesonEncoding a


-- Item type and instances
-- Maybe (Label a) has been changed to (Label a) for compatiblity with vue-pure-pdpa
data Item a = Leaf a
            | All (Label a) (Array (Item a))
            | Any (Label a) (Array (Item a))
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


-- modified default encoding for aeson compatibility
aesonEncoding :: Gen.Encoding
aesonEncoding =
  { tagKey: "tag"
  , valuesKey: "contents"
  , unwrapSingleArguments: true
  }


-- toy example
exampleBinExpr :: BinExpr String String
exampleBinExpr =  BEAll ""
           [ BELeaf "a"
           , BEAny ""
             [ BELeaf "b"
             , BELeaf "c"
             ]
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


binExprToJson :: BinExpr String String -> Json
binExprToJson = encodeJson

binExprFromJson :: Json -> Either JsonDecodeError (BinExpr String String)
binExprFromJson = decodeJson

binExprToItem :: BinExpr String String -> Item String
binExprToItem (BELeaf a) = Leaf a
binExprToItem (BEAll a binexprs) = All (Pre a) (map binExprToItem binexprs)
binExprToItem (BEAny a binexprs) = Any (Pre a) (map binExprToItem binexprs)
binExprToItem (BENot binexpr) = Not (binExprToItem binexpr)


main :: Effect Unit
main = do
  let fp = "/Users/johsi/purs-exp/src/binexpr-hask.json"

  str <- readTextFile UTF8 fp
  let decoded = binExprFromJson =<< parseJson str
  let be = fromRight (BELeaf "") decoded

  log $ show $ binExprToItem be
  -- (All (Pre "") [(Leaf "a"),(Any (Pre "") [(Leaf "b"),(Leaf "c")])])

  -- log $ show $ decoded == Right exampleBinExpr

  -- toy example to JSON
  -- log $ stringify $ binExprToJson exampleBinExpr

  -- round trip with toy example
  -- let str = stringify $ binExprToJson $ exampleBinExpr
  -- log $ show $ binExprFromJson =<< parseJson str

-- type reference
-- log :: String -> Effect Unit
-- readTextFile :: Encoding -> FilePath -> Effect String

-- parseJson :: String -> Either JsonDecodeError Json
-- binExprFromJson :: Json -> Either JsonDecodeError (BinExpr String String)
-- =<< :: (a -> m b) -> m a -> m b
-- =<< :: (Json -> Either JsonDecodeError (BinExpr String String))
--     -> Either JsonDecodeError Json
--     -> Either JsonDecodeError (BinExpr String String)
