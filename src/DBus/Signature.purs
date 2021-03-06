module DBus.Signature
  ( Signature, Variant, class IsVariant, toVariant, fromVariant, class IsValue, typeOf, class IsKey
  , MethodDesc, Method, class AutoMethod, types, typeCode, autoMethod, DBusType
  ) where

import Prelude
import Data.String as String
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import Data.Array as Array
import Data.Foreign (toForeign, isArray)
import Data.Foreign as F
import Data.Decimal (isInteger)
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (traverse)
import Data.Tuple.Native (T4, t4)
import Data.Monoid (class Monoid)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1)
import Type.Proxy (Proxy (..))
import Unsafe.Coerce (unsafeCoerce)


data DBusType
  = DBusBoolean
  | DBusInt32
  | DBusDouble
  | DBusString
  -- | DBusSignature
  -- | TypeObjectPath
  -- | TypeVariant
  | DBusArray DBusType
  | DBusDictionary DBusType DBusType
  | DBusStructure (Array DBusType)

newtype Signature = Signature String

instance semigroupSignature :: Semigroup Signature where
  append (Signature x) (Signature y) = Signature (x <> y)

instance monoidSignature :: Monoid Signature where
  mempty = Signature ""


typeCode :: DBusType -> Signature
typeCode t = Signature $ case t of
  DBusBoolean -> "b"
  DBusInt32 -> "i"
  DBusDouble -> "d"
  -- TypeUnixFd -> "h"
  DBusString -> "s"
  -- DBusSignature -> "g"
  -- TypeObjectPath -> "o"
  -- TypeVariant -> "v"
  DBusArray x -> case typeCode x of
    Signature s -> "a" <> s
  DBusDictionary k v -> case Tuple (typeCode k) (typeCode v) of
    Tuple (Signature k') (Signature v') -> "a{" <> k' <> v' <> "}"
  DBusStructure xs -> String.joinWith "" $ ["("] <> map (getSignature <<< typeCode) xs <> [")"]
  where
    getSignature (Signature x) = x


foreign import data Variant :: Type


class IsVariant a where
  toVariant :: a -> Variant
  fromVariant :: Variant -> Either String a

instance isVariantBoolean :: IsVariant Boolean where
  toVariant = unsafeCoerce
  fromVariant x
    | F.typeOf (toForeign x) == "boolean" = Right (unsafeCoerce x)
    | otherwise                           = Left "Not a boolean"

instance isVariantInt :: IsVariant Int where
  toVariant = unsafeCoerce
  fromVariant x
    | F.typeOf (toForeign x) == "number" && isInteger (unsafeCoerce x) = Right (unsafeCoerce x)
    | otherwise                                                        = Left "Not a Int or isInteger"

instance isVariantNumber :: IsVariant Number where
  toVariant = unsafeCoerce
  fromVariant x
    | F.typeOf (toForeign x) == "number" && not (isInteger $ unsafeCoerce x) = Right (unsafeCoerce x)
    | otherwise                                                              = Left "Not a Number or not isInteger"

instance isVariantString :: IsVariant String where
  toVariant = unsafeCoerce
  fromVariant x
    | F.typeOf (toForeign x) == "string" = Right (unsafeCoerce x)
    | otherwise                          = Left "Not a string"

instance isVariantArray :: IsVariant a => IsVariant (Array a) where
  toVariant = unsafeCoerce
  fromVariant x
    | isArray (toForeign x) = Right (unsafeCoerce x)
    | otherwise             = Left "Not an array"

instance isVariantTuple :: (IsVariant a, IsVariant b) => IsVariant (Tuple a b) where
  toVariant (Tuple x y) = unsafeCoerce [toVariant x, toVariant y]
  fromVariant xs
    | isArray (toForeign xs) = do
        {head:h1,tail} <- case Array.head (unsafeCoerce xs) of
                            Nothing -> Left "no first element"
                            Just x -> pure x
        {head:h2} <- case Array.head tail of
                       Nothing -> Left "no second element"
                       Just x -> pure x
        x <- fromVariant h1
        y <- fromVariant h2
        pure (Tuple x y)
    | otherwise = Left "Not an array - tuple"

instance isVariantMap :: (IsKey k, IsVariant a, Ord k) => IsVariant (Map k a) where
  toVariant xs = unsafeCoerce (map (\(Tuple k v) -> unsafeCoerce [toVariant k, toVariant v]
                                   ) (Map.toUnfoldable xs) :: Array Variant)
  fromVariant xs
    | isArray (toForeign xs) = do
        kvs <-
           traverse (\kv -> if (isArray (toForeign kv))
                              then do {head:k',tail} <- case Array.head (unsafeCoerce kv) of
                                                          Nothing -> Left "no first element"
                                                          Just x -> pure x
                                      {head:v'} <- case Array.head tail of
                                                     Nothing -> Left "no second element"
                                                     Just x -> pure x
                                      k <- fromVariant k'
                                      v <- fromVariant v'
                                      pure (Tuple k v)
                              else Left "Not an array - map elems"
                    ) (unsafeCoerce xs :: Array Variant)
        pure (Map.fromFoldable kvs)
    | otherwise = Left "Not an array - map"

class IsVariant a <= IsValue a where
  typeOf :: Proxy a -> DBusType

instance isValueBoolean :: IsValue Boolean where
  typeOf Proxy = DBusBoolean

instance isValueInt :: IsValue Int where
  typeOf Proxy = DBusInt32

instance isValueNumber :: IsValue Number where
  typeOf Proxy = DBusDouble

instance isValueString :: IsValue String where
  typeOf Proxy = DBusString

instance isValueArray :: IsValue a => IsValue (Array a) where
  typeOf Proxy = DBusArray (typeOf (Proxy :: Proxy a))

instance isValueMap :: (IsKey k, IsValue a, Ord k) => IsValue (Map k a) where
  typeOf Proxy = DBusDictionary (typeOf (Proxy :: Proxy k)) (typeOf (Proxy :: Proxy a))

instance isValueTuple :: (IsValue a, IsValue b) => IsValue (Tuple a b) where
  typeOf Proxy = DBusStructure [typeOf (Proxy :: Proxy a), typeOf (Proxy :: Proxy b)]


class (IsValue k, Ord k) <= IsKey k

instance isKeyBoolean :: IsKey Boolean
instance isKeyInt :: IsKey Int
instance isKeyNumber :: IsKey Number
instance isKeyString :: IsKey String


class AutoMethod a where
  types :: Proxy a -> Tuple (Array Signature) Signature

instance autoMethodEff :: IsValue a => AutoMethod (Eff eff a) where
  types Proxy = Tuple [] $ typeCode $ typeOf (Proxy :: Proxy a)

instance autoMethodEffFn1 :: (IsValue a, IsValue z) => AutoMethod (EffFn1 eff a z) where
  types Proxy = Tuple [typeCode $ typeOf (Proxy :: Proxy a)] (typeCode $ typeOf (Proxy :: Proxy z))



condenseArity :: Array Signature -> Signature
condenseArity xs = case Array.uncons xs of
  Nothing -> Signature ""
  Just {head,tail}
    | Array.length tail == 0 -> head
    | otherwise -> Signature $ String.joinWith "" (map getSignature xs)
  where
    getSignature (Signature x) = x


foreign import data Method :: Type

type MethodDesc =
  { desc :: T4 Signature Signature String String
  , func :: Method
  }


autoMethod :: forall fn. AutoMethod fn => {inputDesc :: String, outputDesc :: String} -> fn -> MethodDesc
autoMethod {inputDesc,outputDesc} fn = case types (Proxy :: Proxy fn) of
  Tuple args result ->
    { desc: t4 (condenseArity args) result inputDesc outputDesc
    , func: unsafeCoerce fn
    }
