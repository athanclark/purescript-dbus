module DBus
  ( connectSession, getService, getInterface, call, on, signalDesc, SignalDesc, exportInterface
  , InterfaceName (..), ObjectPath (..), MemberName (..), BusName (..), DBUS, Client, Service, Interface, InterfaceDesc
  , module Sig
  ) where

import DBus.Signature (Variant, Signature, MethodDesc, class IsValue, class IsVariant, fromVariant)
import DBus.Signature as Sig

import Prelude
import Data.Nullable (Nullable, toMaybe)
import Data.Function.Uncurried (Fn2, Fn4, runFn2, runFn4)
import Data.StrMap (StrMap)
import Data.Maybe (Maybe (..))
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn4, EffFn3, runEffFn3, runEffFn4, mkEffFn1, mkEffFn2)
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
  makeAff \onError onSuccess -> runEffFn4 getInterfaceImpl s o i $ mkEffFn2 \mE i' -> case toMaybe mE of
    Nothing -> onSuccess i'
    Just e  -> onError e


foreign import callImpl :: forall eff
                         . Fn4
                             Interface
                             MemberName
                             (Array Variant)
                             (EffFn2 (dbus :: DBUS | eff) (Nullable Error) Variant Unit) -- FIXME do tuples apply extra args?
                             Boolean



call :: forall eff result
      . IsVariant result
     => Interface -> MemberName -> Array Variant
     -> Aff (dbus :: DBUS | eff) result
call i m@(MemberName m') vs =
  makeAff \onError onSuccess ->
    let result = runFn4 callImpl i m vs $ mkEffFn2 \e r -> case fromVariant r of
                   Nothing -> onError $ error $ "Could not marshall return variant into type"
                   Just r' -> onSuccess r'
    in  if result
           then pure unit
           else onError $ error $ "Member name " <> m' <> " does not exist in interface."


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

