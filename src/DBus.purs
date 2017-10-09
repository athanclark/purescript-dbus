module DBus
  ( connectSession, getService, getInterface, call, on, signalDesc, SignalDesc, exportInterface
  , InterfaceName (..), ObjectPath (..), MemberName (..), BusName (..), DBUS, Client, Service, Interface, InterfaceDesc
  , nil, arg
  , module Sig
  ) where

import DBus.Signature (Variant, Signature, MethodDesc, class IsVariant, class IsValue, typeOf, typeCode, fromVariant, toVariant)
import DBus.Signature as Sig

import Prelude
import Data.Nullable (Nullable, toMaybe)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Foreign as F
import Data.StrMap (StrMap)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import Data.Monoid (mempty)
import Data.Array as Array
import Data.Traversable (traverse_)
import Type.Proxy (Proxy (..))
import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn4, EffFn3, EffFn8, runEffFn3, runEffFn4, runEffFn8, mkEffFn1, mkEffFn2)
import Control.Monad.Eff.Exception (Error, error)



newtype InterfaceName = InterfaceName String

newtype ObjectPath = ObjectPath String

newtype MemberName = MemberName String

newtype BusName = BusName String


foreign import data DBUS :: Effect

foreign import data Client :: Type

foreign import connectSessionImpl :: forall eff. Eff (dbus :: DBUS | eff) (Nullable Client)


connectSession :: forall eff. Eff (dbus :: DBUS | eff) (Maybe Client)
connectSession = toMaybe <$> connectSessionImpl


foreign import data Service :: Type

foreign import getServiceImpl :: Fn2 Client BusName (Nullable Service)


getService :: Client -> BusName -> Maybe Service
getService c b = toMaybe (runFn2 getServiceImpl c b)


foreign import data Interface :: Type

foreign import getInterfaceImpl :: forall eff
                                 . EffFn4 (dbus :: DBUS | eff)
                                     Service
                                     ObjectPath
                                     InterfaceName
                                     (EffFn2 (dbus :: DBUS | eff) (Nullable Error) Interface Unit)
                                     Unit


getInterface :: forall eff
              . Service -> ObjectPath -> InterfaceName
             -> Aff (dbus :: DBUS | eff) Interface
getInterface s o i =
  makeAff \evoke -> do
    runEffFn4 getInterfaceImpl s o i $ mkEffFn2 \mE i' -> case toMaybe mE of
      Nothing -> evoke (Right i')
      Just e  -> evoke (Left e)
    pure nonCanceler


foreign import callImpl :: forall eff
                         . EffFn8 (dbus :: DBUS | eff)
                             Client
                             BusName
                             ObjectPath
                             InterfaceName
                             MemberName
                             Signature
                             (Array Variant)
                             (EffFn2 (dbus :: DBUS | eff) (Nullable (Array Error)) Variant Unit) -- FIXME do tuples apply extra args?
                             Unit


nil :: Tuple Signature (Array Variant)
nil = Tuple mempty []

arg :: forall a. IsValue a => Tuple Signature (Array Variant) -> a -> Tuple Signature (Array Variant)
arg (Tuple s xs) x =
  let s' = typeCode (typeOf (Proxy :: Proxy a))
  in  Tuple (s <> s') (xs <> [toVariant x])


call :: forall eff result
      . IsVariant result
     => Client -> BusName -> ObjectPath -> InterfaceName -> MemberName -> Tuple Signature (Array Variant)
     -> Aff (dbus :: DBUS | eff) result
call c b o i m@(MemberName m') (Tuple s xs) =
  makeAff \evoke -> do
    runEffFn8 callImpl c b o i m s xs $ mkEffFn2 \mE v ->
      case toMaybe mE of
        Nothing -> case fromVariant v of
          Nothing -> evoke $ Left $ error $ "Could not marshall return variant into type: " <> F.typeOf (F.toForeign v)
          Just r -> evoke (Right r)
        Just es -> traverse_ (\e -> evoke (Left e)) es
    pure nonCanceler


foreign import onImpl :: forall eff
                       . EffFn3 (dbus :: DBUS | eff) Interface MemberName (EffFn1 (dbus :: DBUS | eff) Variant Unit) Unit

on :: forall eff a
    . IsVariant a
   => Interface -> MemberName
   -> (a -> Eff (dbus :: DBUS | eff) Unit)
   -> Eff (dbus :: DBUS | eff) Unit
on i m f =
  runEffFn3 onImpl i m $ mkEffFn1 \v -> case fromVariant v of
    Nothing -> pure unit
    Just a -> f a


foreign import data SignalDesc :: Type

foreign import signalDescImpl :: Fn2 Signature String SignalDesc

signalDesc :: Signature -> String -> SignalDesc
signalDesc = runFn2 signalDescImpl


type InterfaceDesc =
  { name :: InterfaceName
  , methods :: StrMap MethodDesc
  , signals :: StrMap SignalDesc
  , properties :: StrMap Unit
  }


foreign import exportInterfaceImpl :: forall eff
                                    . EffFn3 (dbus :: DBUS | eff) Service ObjectPath InterfaceDesc Unit


exportInterface :: forall eff. Service -> ObjectPath -> InterfaceDesc -> Eff (dbus :: DBUS | eff) Unit
exportInterface = runEffFn3 exportInterfaceImpl

