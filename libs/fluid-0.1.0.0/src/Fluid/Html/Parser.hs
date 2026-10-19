
module Fluid.Html.Parser
    ( parseText, parseText'
    )
where
import Fluid
import Fluid.Html
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Data.Tree
import Text.HTML.Parser
import Text.HTML.Tree

parseText :: Text -> Either Text [Html w action]
parseText txt = do -- Monad (Either Text)
    forest <- parseText' txt
    convertForest forest

parseText' :: Text -> Either Text (Forest Token)
parseText' txt =
    let toks = parseTokens txt
    in case tokensToForest toks of
         Right h -> Right h
         Left err -> Left $ pack $ show err


convertForest :: Forest Token -> Either Text [Html w action]
convertForest ts = catMaybes <$> mapM convertTree ts

convertTree :: Tree Token -> Either Text (Maybe (Html w action))
convertTree (Node (TagOpen n as) ts) | n `notElem` nonClosing =
    Just . nodeHtml n (convertAttr <$> as) <$> convertForest ts
convertTree (Node t []) =
    case t of
        TagOpen n as -> Right $ Just $ nodeHtml n (convertAttr <$> as) []
        TagSelfClose n as -> Right $ Just $ nodeHtml n (convertAttr <$> as) []
        ContentText t -> Right $ Just $ text t
        ContentChar c -> Right $ Just $ text $ pack [c]
        Comment _ -> Right $ Nothing
        _ -> Left $ "convertTree: invalid node: " <> pack (show t)
convertTree _ =
    Left "convertTree: leaf node with children"

convertAttr :: Attr -> Prop action
convertAttr (Attr n v) =
    textProp n v

