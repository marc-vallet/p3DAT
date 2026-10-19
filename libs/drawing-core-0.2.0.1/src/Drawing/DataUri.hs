
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Drawing.DataUri
  ( dataUri, dataUriFromFile
#ifdef EMBEDING
  , embedAsUri, embedAsImage
#endif
  )
where
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

#ifdef EMBEDING
import Drawing

import qualified Data.ByteString.Unsafe as B
import Language.Haskell.TH.Syntax
import System.IO.Unsafe
#endif

dataUri :: T.Text -> B.ByteString -> T.Text
dataUri contentType content =
    let payload = T.decodeUtf8 $ B64.encode content
    in "data:" <> contentType <> ";base64," <> payload

dataUriFromFile :: T.Text -> FilePath -> IO T.Text
dataUriFromFile contentType fileName =
    dataUri contentType <$> B.readFile fileName

#ifdef EMBEDING
-- | 'embedAsUri contentType fileName' is a quoted expression of type 'T.Text'
embedAsUri :: T.Text -> FilePath -> Q Exp
embedAsUri contentType fileName = do
  qAddDependentFile fileName
  uri <- runIO $ dataUriFromFile contentType fileName
  let uri_len = T.length uri
      uri_words = B.unpack $ T.encodeUtf8 uri
  [e|
    T.decodeUtf8 $ unsafePerformIO $
      B.unsafePackAddressLen
        $(return $ LitE $ IntegerL $ fromIntegral uri_len)
        $(return $ LitE $ StringPrimL uri_words)
    |]

-- | 'embedAsImage contentType fileName w h' is a quoted expression of type 'Drawing'
embedAsImage :: T.Text -> FilePath -> Double -> Double -> Q Exp
embedAsImage contentType fileName w h =
  [e| image $(embedAsUri contentType fileName) w h |]
#endif

