module Test.Main where

import DBus

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, error, errorShow)

main :: forall e. Eff (console :: CONSOLE, dbus :: DBUS | e) Unit
main = do
  mClient <- connectSession
  case mClient of
    Nothing -> error "no client!"
    Just client -> case getService client (BusName "com.moneroworld.Monerodo") of
      Nothing -> error "no service!"
      Just service -> void $ runAff resolve $ do
        interface <- getInterface service (ObjectPath "/") (InterfaceName "com.moneroworld.FooInterface")
        liftEff $ on interface (MemberName "Bacon") $ \x -> log x
        result <- call client (BusName "com.moneroworld.Monerodo") (ObjectPath "/") (InterfaceName "com.moneroworld.FooInterface") (MemberName "Ayoo") (nil `arg` "foo")
        liftEff $ log $ "success! " <> result
  where
    resolve (Left e) = errorShow e
    resolve _        = pure unit
