
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import           Data.List (stripPrefix)
import           Data.Maybe (fromMaybe)
import qualified Network.Wai.Application.Static as W
import qualified WaiAppStatic.Types as W
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai as W
import           Network.Mime (MimeType)
import           System.Environment (getArgs, lookupEnv)
import           Text.Read (readMaybe)

main :: IO ()
main = do
    [dir] <- getArgs
    port <- fromMaybe 8008 . (readMaybe =<<) <$> lookupEnv "PORT"
    putStrLn $ "Running on port " <> show port <> "..."
    app <- getStaticApp dir
    W.runSettings (W.setPort port $ W.setTimeout 3600 W.defaultSettings) app

getStaticApp :: FilePath -> IO W.Application
getStaticApp dir = do
    pure $ W.staticApp settings
    where
        charset :: B.ByteString
        charset = "utf8"
        settings =
            let def = W.defaultWebAppSettings dir
            in def
                { W.ssAddTrailingSlash = True
                , W.ssGetMimeType = setCharset (W.ssGetMimeType def)
                , W.ssMaxAge = W.NoCache
                }
        setCharset :: (W.File -> IO MimeType) -> W.File -> IO MimeType
        setCharset getMime file =
            let setMimeCharset mime =
                    if B.isPrefixOf "text/" mime
                        then mime <> ";charset=" <> charset
                        else mime
            in setMimeCharset <$> getMime file

