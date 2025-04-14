{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions to work with cabal files and cabal extension maps.
-}

{-# LANGUAGE QuasiQuotes #-}

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
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory,
                         makeRelativeToCurrentDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO.Unsafe (unsafeInterleaveIO)

import Stan.Hie.Compat (HieFile (..))

import qualified Data.Map.Strict as Map
import qualified System.OsPath as OsPath
import qualified System.Directory.OsPath as OsPath


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
findCabalFiles = do
    dir <- getCurrentDirectory
    curDirCabal <- findCabalFileDir dir
    dirs <- getSubdirsRecursive dir
    subDirsCabals <- mapM findCabalFileDir dirs
    pure $ catMaybes $ curDirCabal : subDirsCabals

-- | Find a @.cabal@ file in the given directory.
-- TODO: better error handling in stan.
findCabalFileDir :: FilePath -> IO (Maybe FilePath)
findCabalFileDir dir = do
    dirContent <- listDirectory dir
    let cabalFiles = filter isCabal dirContent
    pure $ case cabalFiles of
        []            -> Nothing
        cabalFile : _ -> Just $ dir </> cabalFile
  where
    isCabal :: FilePath -> Bool
    isCabal p = takeExtension p == ".cabal"

getSubdirsRecursive :: FilePath -> IO [FilePath]
getSubdirsRecursive fp = do
    f <- OsPath.encodeFS fp
    res <- getSubdirsRecursiveOs f
    traverse OsPath.decodeFS res

getSubdirsRecursiveOs :: OsPath.OsPath -> IO [OsPath.OsPath]
getSubdirsRecursiveOs fp = do
    all' <- filter nonGenDir <$> OsPath.listDirectory fp
    dirs <- filterM OsPath.doesDirectoryExist (mkRel <$> all')
    case dirs of
        [] -> pure []
        ds -> do
            -- unsafeInterleaveIO is required here for performance reasons
            next <- unsafeInterleaveIO $ foldMapA getSubdirsRecursiveOs ds
            pure $ dirs ++ next
  where
    nonGenDir :: OsPath.OsPath -> Bool
    nonGenDir d =
           d /= [OsPath.osp|dist|]
        && d /= [OsPath.osp|dist-newstyle|]
        && d /= [OsPath.osp|.stack-work|]
        && d /= [OsPath.osp|.git|]

    mkRel :: OsPath.OsPath -> OsPath.OsPath
    mkRel = (fp OsPath.</>)

mergeParsedExtensions
    :: Either ExtensionsError ParsedExtensions
    -> Either ExtensionsError ParsedExtensions
    -> ExtensionsResult
mergeParsedExtensions (Left err) _                = Left err
mergeParsedExtensions _ (Left err)                = Left err
mergeParsedExtensions (Right exts1) (Right exts2) = mergeAnyExtensions exts1 exts2
