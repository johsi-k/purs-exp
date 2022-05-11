module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Error (JsonDecodeError)
-- import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode.Parser (parseJson)

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)

import Data.Either (Either)

import Unsafe.Coerce (unsafeCoerce)

data Item a = Leaf a
            | All (Label a) (Array (Item a))
            | Any (Label a) (Array (Item a))
            | Not (Item a)

derive instance eqItem :: (Eq a) => Eq (Item a)
derive instance genericItem :: Generic (Item a) _

instance showItem :: (Show a) => Show (Item a) where
  show eta = genericShow eta
instance decodeJsonItem :: DecodeJson a => DecodeJson (Item a) where
  decodeJson a = genericDecodeJson a


data Label a = Pre a
             | PrePost a a

derive instance eqLabel :: (Eq a) => Eq (Label a)
derive instance genericLabel :: Generic (Label a) _

instance showLabel :: (Show a) => Show (Label a) where
  show = genericShow
instance decodeJsonLabel :: DecodeJson a => DecodeJson (Label a) where
  decodeJson a = genericDecodeJson a


itemFromJson :: Json -> Either JsonDecodeError (Item String)
-- itemFromJson = unsafeCoerce unit :: forall a. a
itemFromJson = decodeJson

-- someObject :: Either String Json
-- someObject = jsonParser
--   """
--   { people: [{ name: "John" }, { name: "Jane" }] };
--   """

main :: Effect Unit
main = do
  -- log =<< readTextFile UTF8 "input.json"
  str <- readTextFile UTF8 "/Users/johsi/purs-exp/src/input.json"
  log $ show $ userFromJson =<< parseJson str

-- log :: String -> Effect Unit
-- readTextFile :: Encoding -> FilePath -> Effect String

-- parseJson :: String -> Either JsonDecodeError Json
-- userFromJson :: Json -> Either JsonDecodeError (Item String)
-- =<< :: (a -> m b) -> m a -> m b
-- =<< :: (Json -> Either JsonDecodeError (Item String))
--     -> Either JsonDecodeError Json
--     -> Either JsonDecodeError (Item String)


-- main :: Effect Unit
-- main = do
--   log "🍝"
