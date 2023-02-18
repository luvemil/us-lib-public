{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Storage where

import Data.Aeson as Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Polysemy
import Polysemy.Error
import Polysemy.Output

data Storage v m a where
    SaveData :: v -> Storage v m ()

makeSem ''Storage

runAesonStorageToFile :: forall v r a. (Member (Embed IO) r, ToJSON v) => FilePath -> Sem (Storage v ': r) a -> Sem r a
runAesonStorageToFile fp = interpret $ \case
    SaveData d -> do
        let postsS = Aeson.encode d
        embed $ BSL.writeFile fp postsS

runStorageToOutput :: Member (Output v) r => Sem (Storage v ': r) a -> Sem r a
runStorageToOutput = interpret $ \case
    SaveData d -> do
        output d

runStorageToStorage :: (v -> v') -> Sem (Storage v ': r) a -> Sem (Storage v' ': r) a
runStorageToStorage fn = reinterpret $ \case
    SaveData d -> saveData $ fn d

runStorageToStorageH :: (v -> Sem (Storage v' ': r) v') -> Sem (Storage v ': r) a -> Sem (Storage v' ': r) a
runStorageToStorageH fn = reinterpret $ \case
    SaveData d -> do
        v <- fn d
        saveData v

runStorageToIO :: Members '[Embed IO, Error e] r => (v -> IO (Either e ())) -> Sem (Storage v ': r) a -> Sem r a
runStorageToIO save = interpret $ \case
    SaveData d -> do
        res <- embed $ save d
        case res of
            Right _ -> pure ()
            Left e -> throw e