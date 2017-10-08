module DBus
  ( connectSession, getService, getInterface, call, signalDesc, SignalDesc, exportInterface
  , InterfaceName (..), ObjectPath (..), MemberName (..), BusName (..), DBUS, Client, Service, Interface, InterfaceDesc
  , module Sig
  ) where

import DBus.Signature (Variant, Signature, MethodDesc, class IsVariant, fromVariant)
import DBus.Signature as Sig

import Prelude
import Data.Nullable (Nullable, toMaybe)
import Data.Function.Uncurried (Fn2, Fn4, runFn2, runFn4)
import Data.StrMap (StrMap)
import Data.Maybe (Maybe (..))
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn4, EffFn3, runEffFn3, runEffFn4, mkEffFn2)
import Control.Monad.Eff.Exception (Error)



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
             -> (Maybe Error -> Interface -> Eff (dbus :: DBUS | eff) Unit)
             -> Eff (dbus :: DBUS | eff) Unit
getInterface s o i f = runEffFn4 getInterfaceImpl s o i (mkEffFn2 \e i' -> f (toMaybe e) i')


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
     -> (Maybe Error -> result -> Eff (dbus :: DBUS | eff) Unit)
     -> Boolean
call i m vs f = runFn4 callImpl i m vs $ mkEffFn2 \e r -> case fromVariant r of
  Nothing -> pure unit
  Just r' -> f (toMaybe e) r'


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

