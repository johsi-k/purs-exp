-- File auto generated by purescript-bridge! --
module LS.Types
  where

import Prelude

import AnyAll.Types (Item', Label, aesonEncoding)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJsonWith)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJsonWith)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Prim (Array, String)

data RelationalPredicate =
    RPParamText (NonEmptyList (Tuple (NonEmptyList String) (Maybe TypeSig)))
  | RPMT (Array String)
  | RPConstraint (Array String) RPRel (Array String)
  | RPBoolStructR (Array String) RPRel (Item' (Label String) RelationalPredicate)

derive instance genericRelationalPredicate :: Generic RelationalPredicate _

instance showRP :: Show RelationalPredicate where
  show a = genericShow a

-- from docs: instances for a recursive type cannot be written in point-free style, as that would likely cause a stack overflow during execution
instance encodeJsonRP :: EncodeJson RelationalPredicate where
  encodeJson a = genericEncodeJsonWith aesonEncoding a
instance decodeJsonRP :: DecodeJson RelationalPredicate where
  decodeJson a = genericDecodeJsonWith aesonEncoding a


data TypeSig =
    SimpleType ParamType String
  | InlineEnum ParamType (NonEmptyList (Tuple (NonEmptyList String) (Maybe TypeSig)))

derive instance genericTypeSig :: Generic TypeSig _

instance showTypeSig :: Show TypeSig where
  show a = genericShow a

instance encodeJsonTypeSig :: EncodeJson TypeSig where
  encodeJson a = genericEncodeJsonWith aesonEncoding a
instance decodeJsonTypeSig :: DecodeJson TypeSig where
  decodeJson a = genericDecodeJsonWith aesonEncoding a


data RPRel =
    RPis
  | RPeq
  | RPlt
  | RPlte
  | RPgt
  | RPgte
  | RPelem
  | RPnotElem

derive instance genericRPRel :: Generic RPRel _

instance showRPRel :: Show RPRel where
 show = genericShow

instance encodeJsonRPRel :: EncodeJson RPRel where
  encodeJson = genericEncodeJsonWith aesonEncoding
instance decodeJsonRPRel :: DecodeJson RPRel where
  decodeJson = genericDecodeJsonWith aesonEncoding


data ParamType =
    TOne
  | TOptional
  | TList0
  | TList1

derive instance genericParamType :: Generic ParamType _

instance showParamType :: Show ParamType where
  show = genericShow

instance encodeJsonParamType :: EncodeJson ParamType where
  encodeJson = genericEncodeJsonWith aesonEncoding
instance decodeJsonParamType :: DecodeJson ParamType where
  decodeJson = genericDecodeJsonWith aesonEncoding
