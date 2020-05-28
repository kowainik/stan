{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Environment variables for @stan@.
-}

module Stan.EnvVars
    ( EnvVars (..)
    , getEnvVars
    ) where

import System.Environment (lookupEnv)
import Trial (TaggedTrial, Trial, fiasco, withTag)


data EnvVars = EnvVars
    { envVarsUseDefaultConfigFile :: !(TaggedTrial Text Bool)
    }

getEnvVars :: IO EnvVars
getEnvVars = do
    envVarsUseDefaultConfigFile <- getEnvVar "STAN_USE_DEFAULT_CONFIG" $ \case
        "True"  -> pure True
        "False" -> pure False
        _ -> fiasco "Incorrect value is set for STAN_USE_DEFAULT_CONFIG"
    pure EnvVars{..}
  where
    getEnvVar :: String -> (String -> Trial Text a) -> IO (TaggedTrial Text a)
    getEnvVar var act = lookupEnv var >>= \val -> pure $ withTag "EnvVar" $ case val of
        Nothing -> fiasco $ "No " <> toText var <> " Env Variable is set"

        Just s  -> act s
