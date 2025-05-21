{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions to work with cabal files and cabal extension maps.
-}

{-# LANGUAGE CPP #-}
#ifdef MIN_VERSION_directory_ospath_streaming
{-# LANGUAGE QuasiQuotes #-}
#endif

module Stan.Cabal
    ( createCabalExtensionsMap
    , usedCabalFiles

    , mergeParsedExtensions
    ) where

import Relude.Extra.Tuple (toSnd)

import Colourista (errorMessage, infoMessage, warningMessage)
import Control.Exception (catch)
import Extensions (CabalException, ExtensionsError (..), ExtensionsResult, ParsedExtensions (..),
                   mergeAnyExtensions, parseCabalFileExtensions)
import System.Directory (doesFileExist, makeRelativeToCurrentDirectory)

import Stan.Hie.Compat (HieFile (..))

import qualified Data.Map.Strict as Map
#ifndef MIN_VERSION_directory_ospath_streaming
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Directory as FilePath
import qualified System.FilePath as FilePath
#else
import qualified System.OsPath as OsPath
import qualified System.Directory.OsPath as OsPath
import qualified System.Directory.OsPath.Streaming as OPS
import qualified System.Directory.OsPath.Types as OPS
import qualified Data.Set as S
#endif


{- | Gets the list of @.cabal@ file paths that were used in the project.
-}
usedCabalFiles :: [FilePath] -> IO [FilePath]
usedCabalFiles fs = do
    cabals <- case fs of
        []    -> findCabalFiles
        files -> pure files
    mapM makeRelativeToCurrentDirectory cabals

{- | From a given path to cabal files and 'HieFile's create the map from modules
(that are in .cabal file) to the resulting parsed extensions for each.
-}
createCabalExtensionsMap
    :: Bool  -- ^ Do print into terminal?
    -> [FilePath]  -- ^ @.cabal@ files
    -> [HieFile]
    -> IO (Map FilePath (Either ExtensionsError ParsedExtensions))
createCabalExtensionsMap isLoud cabalPath hies = case cabalPath of
    -- if cabal files are not specified via CLI option
    -- try to find cabal files in current directory
    [] -> findCabalFiles >>= \case
        -- if cabal file is not found, pass the empty map instead
        [] -> do
            when isLoud $ do
                warningMessage ".cabal file not found in the current directory."
                infoMessage " 💡 Try using --cabal-file-path option to specify the path to the .cabal file.\n"
            pure mempty
        -- else concat map for each @.cabal@ file.
        cabals -> mconcat <$> mapM getExtensionsWithCabal cabals
    -- if cabal file specified via CLI option
    cabals -> fmap mconcat $ forM (ordNub cabals) $ \cabal ->
        ifM (doesFileExist cabal)
        {- then -} (getExtensionsWithCabal cabal)
        {- else -} (errorMessage (".cabal file does not exist: " <> toText cabal) >> exitFailure)
  where
    getExtensionsWithCabal
        :: FilePath
        -> IO (Map FilePath (Either ExtensionsError ParsedExtensions))
    getExtensionsWithCabal cabal = do
        when isLoud $ infoMessage $ "Using the following .cabal file: " <> toText cabal <> "\n"
        (Right <<$>> parseCabalFileExtensions cabal)
            `catch` handleCabalErr
      where
        handleCabalErr
            :: CabalException
            -> IO (Map FilePath (Either ExtensionsError ParsedExtensions))
        handleCabalErr err = do
            when isLoud $ errorMessage "Error when parsing cabal file. Stan will continue without information from .cabal file"
            pure $ Map.fromList $
                map (toSnd (const $ Left $ CabalError err) . hie_hs_file) hies

{- | Recursively find all @.cabal@ files in the current directory and its
subdirectories. It returns maximum 1 @.cabal@ file from each directory.
-}
findCabalFiles :: IO [FilePath]
findCabalFiles =
#ifndef MIN_VERSION_directory_ospath_streaming
    findCabalFilesFilePath
#else
    findCabalFilesStreaming
#endif

#ifndef MIN_VERSION_directory_ospath_streaming
findCabalFilesFilePath :: IO [FilePath]
findCabalFilesFilePath = do
    dir <- FilePath.getCurrentDirectory
    curDirCabal <- findCabalFileFilePath dir
    dirs <- getSubdirsRecursiveFilePath dir
    subDirsCabals <- mapM findCabalFileFilePath dirs
    pure $ catMaybes $ curDirCabal : subDirsCabals
  where
    -- | Find a @.cabal@ file in the given directory.
    -- TODO: better error handling in stan.
    findCabalFileFilePath :: FilePath -> IO (Maybe FilePath)
    findCabalFileFilePath dir = do
        dirContent <- FilePath.listDirectory dir
        let cabalFiles = filter isCabal dirContent
        pure $ case cabalFiles of
            []            -> Nothing
            cabalFile : _ -> Just $ dir FilePath.</> cabalFile
      where
        isCabal :: FilePath -> Bool
        isCabal p = FilePath.takeExtension p == ".cabal"

    getSubdirsRecursiveFilePath :: FilePath -> IO [FilePath]
    getSubdirsRecursiveFilePath fp = do
        all' <- filter nonGenDir <$> FilePath.listDirectory fp
        dirs <- filterM FilePath.doesDirectoryExist (mkRel <$> all')
        case dirs of
            [] -> pure []
            ds -> do
                -- unsafeInterleaveIO is required here for performance reasons
                next <- unsafeInterleaveIO $ foldMapA getSubdirsRecursiveFilePath ds
                pure $ dirs ++ next
      where
        nonGenDir :: FilePath -> Bool
        nonGenDir d =
               d /= "dist"
            && d /= "dist-newstyle"
            && d /= ".stack-work"
            && d /= ".git"

        mkRel :: FilePath -> FilePath
        mkRel = (fp FilePath.</>)

#else
-- Fix for https://github.com/haskell/haskell-language-server/issues/4515
findCabalFilesStreaming :: IO [FilePath]
findCabalFilesStreaming = do
    setRef <- newIORef S.empty -- stores the directories where we already found 1 cabal file
    root <- OsPath.getCurrentDirectory
    traverse OsPath.decodeFS =<<
        OPS.listContentsRecFold
           Nothing -- Depth limit
            (\_ _ (OPS.Relative _dirRelPath) (OPS.Basename dirBasename)  _symlinkType _consDirToList traverseThisSubdir rest ->
                 if visitCurrSubdirPred dirBasename -- if this condition is satisfied
                 then traverseThisSubdir rest -- True -> then this subdir will be traversed
                 else rest -- False -> else, this subdir will not be traversed
                ) -- how to fold this directory and its children, given its path
            (\_ _ (OPS.Relative path) (OPS.Basename fileBasename) _ft -> do
                  let parentDir = OsPath.takeDirectory path
                  set <- readIORef setRef
                  if not (S.member parentDir set) && collectPred fileBasename -- if this condition is satisfied
                  then do
                        writeIORef setRef $  S.insert parentDir set -- we add the parentDir of this file, to prevent adding more than 1 .cabal file
                        pure (Just path) -- True -> then this path will be added to the results (because @path@ is already relative, we no longer need @mkRel@)
                  else pure Nothing -- False -> else, this path wont be added
                )
            (Identity root) -- (f a), list of roots to search in
  where
    visitCurrSubdirPred :: OsPath.OsPath -> Bool
    visitCurrSubdirPred d =
           d /= [OsPath.osp|dist|]
        && d /= [OsPath.osp|dist-newstyle|]
        && d /= [OsPath.osp|.stack-work|]
        && d /= [OsPath.osp|.git|]

    collectPred :: OsPath.OsPath -> Bool
    collectPred p =
        OsPath.takeExtension p == [OsPath.osp|.cabal|]
#endif

mergeParsedExtensions
    :: Either ExtensionsError ParsedExtensions
    -> Either ExtensionsError ParsedExtensions
    -> ExtensionsResult
mergeParsedExtensions (Left err) _                = Left err
mergeParsedExtensions _ (Left err)                = Left err
mergeParsedExtensions (Right exts1) (Right exts2) = mergeAnyExtensions exts1 exts2
