{-# language
    DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveTraversable
  , DerivingStrategies
  , LambdaCase
#-}

-- | This module provides a simple tree structure, 'Fs', which
--   is suitable for modelling static file systems, i.e. those 
--   without symlinks and ignoring references.
--
--   Later on, the type will be able to handle these more complicated
--   file systems.
module Fs
  ( Fs(..)
  , read
  , readFilter
  , write
  ) where

import Control.Monad (forM)
import Data.Foldable (for_)
import Data.Text (Text)
import GHC.Generics (Generic, Generic1)
import Path (Path, Rel, File, Dir)
import Prelude hiding (read)
import qualified Data.Text.IO as TIO
import qualified Path.IO as Path
import qualified Path.Internal as UnsafePath

-- | A tree representing a file system. The type constructor
--   'Fs' is parameterised by the type of its contents.
data Fs a
  = File (Path Rel File) a
    -- ^ A relative file path, and the contents therein.
  | Dir (Path Rel Dir) [Fs a]
    -- ^ A relative directory. The branches are more 'Fs'
    --   objects.
  deriving stock (Eq, Show)
  deriving stock (Generic, Generic1)
  deriving stock (Functor, Foldable, Traversable)

-- | 'write' an 'Fs' to the file system.
write :: Fs Text -> IO ()
write = \case
  File path content -> do
    TIO.writeFile (UnsafePath.toFilePath path) content
  Dir path children -> do
    Path.createDirIfMissing False path
    Path.withCurrentDir path $ for_ children write

-- | 'read' an 'Fs' from the file system.
read :: Path Rel Dir -> IO (Fs Text)
read = readFilter (const True) (const True)

-- | 'readFilter' is like 'read', but you can filter out
--   files and directories you'd like to ignore via simple
--   predicates.
readFilter :: ()
  => (Path Rel Dir -> Bool)
  -> (Path Rel File -> Bool)
  -> Path Rel Dir
  -> IO (Fs Text)
readFilter p_dir p_file = go
  where
   sift ~(ds, fs) = (filter p_dir ds, filter p_file fs)
   go d = do
     Path.withCurrentDir d $ do
       listed <- sift <$> Path.listDirRel (UnsafePath.Path ".")
       contents <- forM (snd listed) $ \path -> do
         content <- TIO.readFile (UnsafePath.toFilePath path)
         pure (File path content)
       dirs <- forM (fst listed) go
       pure (Dir d (contents ++ dirs))
