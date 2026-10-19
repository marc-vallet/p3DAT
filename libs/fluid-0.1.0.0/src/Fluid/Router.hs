
{-# LANGUAGE OverloadedStrings #-}

module Fluid.Router
where
import           Fluid hiding (LocalURI)
import           Fluid.Query.Event
import           Fluid.Subscription

import           Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Text.Encoding as T
import           Network.HTTP.Types.URI
import           Web.Event.Event
import           Web.Event.EventTarget
import           Web.HTML.Event.PopStateEvent
import           Web.HTML.History
import           Web.HTML.Location
import           Web.HTML.Window


-- * LocalURI type and functions

data LocalURI = LocalURI
    { luriPath :: [Text]
    , luriQuery :: [(Text, Maybe Text)]
    , luriFragment :: Maybe Text
    }
    deriving (Eq, Show)

localURIToText :: LocalURI -> Text
localURIToText luri = "/" <> path <> qs <> frag
    where
        path = pack $ LB8.unpack $ B.toLazyByteString $
          encodePathSegmentsRelative $ luriPath luri
        qs = case luriQuery luri of
            [] -> ""
            ps -> pack $ B8.unpack $ renderQuery True (queryTextToQuery ps)
        frag = case luriFragment luri of
            Nothing -> ""
            Just f -> "#" <> f


pushLocalURI :: LocalURI -> IO ()
pushLocalURI luri = do
  let his = history
  pushState "" (localURIToText luri) his
  -- Dispatch a syntetic popstate event
  threadDelay 1000 -- dalays 1 millisecond
  pev <- newPopStateEvent "popstate"
  win <- window
  _ <- dispatchEvent pev win
  pure ()

getLocalURI :: IO LocalURI
getLocalURI = do
  let loc = location
  path <- B8.pack . unpack <$> getPathname loc
  search <- B8.pack . unpack <$> getSearch loc
  hash <- B8.pack . unpack <$> getHash loc
  pure $ LocalURI
          { luriPath = decodePathSegments path
          , luriQuery = parseQueryText search
          , luriFragment = case B.uncons hash of
                  Just (35, f) -> Just $ T.decodeUtf8 f
                  _ -> Nothing
          }


localURIListener :: (LocalURI -> a) -> IO (Emitter a)
localURIListener act = do
  let callback _ = do
        luri <- getLocalURI
        pure $ Just $ act luri
  win <- window
  pure $ eventListener' (EventType "popstate") (toEventTarget win) callback


-- * Router class and functions

class Router route where
    toRoute :: LocalURI -> Maybe route
    fromRoute :: route -> LocalURI

routeToText :: Router route => route -> Text
routeToText = localURIToText . fromRoute


pushRoute :: Router r => r -> IO ()
pushRoute =
  pushLocalURI . fromRoute


routeListener :: Router r => (r -> a) -> (LocalURI -> a) -> IO (Emitter a)
routeListener success error =
  let f luri = maybe (error luri) success $ toRoute luri
  in localURIListener f

