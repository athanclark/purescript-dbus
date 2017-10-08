module Test.Main where

import DBus

import Prelude
import Data.Maybe (Maybe (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)

main :: forall e. Eff (console :: CONSOLE, dbus :: DBUS | e) Unit
main = do
  mClient <- connectSession
  case mClient of
    Nothing -> error "no client!"
    Just client -> case getService client (BusName "com.moneroworld.Monerodo") of
      Nothing -> error "no service!"
      Just service ->
        getInterface service (ObjectPath "/") (InterfaceName "com.moneroworld.FooInterface") $ \mError interface ->
          case mError of
            Nothing ->
              if ( call interface (MemberName "Ayoo") [toVariant "foo"] $ \mError result ->
                      case mError of
                        Just e -> error $ "no member! " <> show e
                        Nothing -> log $ "success! " <> result
                  )
                  then log "technically should work"
                  else error "no member Ayoo!"
            Just e -> error $ "no interface! " <> show e
