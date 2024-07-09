module Target.PlutusTx where

import Data.Foldable (forM_, for_)
import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as List
import qualified Data.Text as Text

import qualified PlutusTx as Tx
import qualified PlutusTx.Maybe as Maybe
import qualified PlutusTx.Prelude as Tx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx (UnsafeFromData(unsafeFromBuiltinData))

-- Place for future imports
--
--
--
--
--
--
--
--
--
--
--
--

assocMap :: AssocMap.Map k v
assocMap = AssocMap.unsafeFromList mempty

unsafeFromBuiltinData :: Integer
unsafeFromBuiltinData =
  Tx.unsafeFromBuiltinData (error "we don't care")

fromMaybe01 :: Integer
fromMaybe01 =
  Maybe.fromMaybe 2 (Just 1)
