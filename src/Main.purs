module Main
  where

import AnyAll.Types (Item'(..), Label)
import LS.Types (RelationalPredicate(..))
import Prelude (Unit, bind, show, ($), (=<<))

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)


-- toy items
-- prettierEx :: Item' RelationalPredicate RelationalPredicate
-- prettierEx =  All
--                { itemLbl : Nothing
--                , itemsAll :
--                  [ Leaf
--                    ( RPMT [ "a" ] )
--                  , Any
--                    { itemLbl : Nothing
--                    , itemsAny :
--                      [ Leaf ( RPMT [ "b" ] )
--                      , Leaf ( RPMT [ "c" ] )
--                      ]
--                    }
--                  ]
--                }

example :: Item' RelationalPredicate RelationalPredicate
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

itemToJson :: Item' RelationalPredicate RelationalPredicate -> Json
itemToJson = encodeJson

itemFromJson :: Json -> Either JsonDecodeError (Item' RelationalPredicate RelationalPredicate)
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
  -- let str = stringify $ itemToJson $ prettierEx
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
