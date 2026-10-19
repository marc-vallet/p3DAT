
{-# LANGUAGE CPP                        #-}

module HVDom.Utils
where
import Control.Monad
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text (Text)
import Web.DOM.Node as DOM

insertChildIx :: Int -> Node -> Node -> IO ()
#ifdef GHCJS_NEW
insertChildIx ix child parent =
  js_insert_child_ix ix child parent

foreign import javascript unsafe
  "js_insert_child_ix"
  js_insert_child_ix :: Int -> Node -> Node -> IO ()

#else
insertChildIx = undefined
#endif


diffWithIxE :: [b] -> [c]
            -> (Int -> b -> c -> IO (Maybe d)) -> (Int -> b -> IO (Maybe d)) -> (Int -> c -> IO (Maybe d))
            -> IO [d]
diffWithIxE u v onThese onThis onThat = reverse . catMaybes <$> go 0 u v []
  where
    go _ [] [] acc = pure acc
    go i (x : xs) [] acc = do
      void $ onThis i x
      go (i + 1) xs [] acc
    go i [] (y : ys) acc = do
      val <- onThat i y
      go (i + 1) [] ys (val : acc)
    go i (x : xs) (y : ys) acc = do
      val <- onThese i x y
      go (i + 1) xs ys (val : acc)

diffWithKeyAndIxE
  :: M.Map Text a
  -> [b]
  -> (b -> Text)
  -> (Text -> Int -> a -> b -> IO c)
  -> (Text -> a -> IO d)
  -> (Text -> Int -> b -> IO c)
  -> IO (M.Map Text c)
diffWithKeyAndIxE o1 as fk f1 f2 f3 = do
  o2 <- foldM go M.empty (zip [0 ..] as)
  traverse_ (uncurry f2) (M.toAscList (M.difference o1 o2))
  pure o2
  where
    go acc (i, a) = do
      let k = fk a
      val <- case M.lookup k o1 of
        Just v -> f1 k i v a
        Nothing -> f3 k i a
      pure $ M.insert k val acc

strMapWithIxE :: [a] -> (a -> Text) -> (Text -> Int -> a -> IO b) -> IO (M.Map Text b)
strMapWithIxE = strMapWithIxE' . zip [0 ..]
  where
    strMapWithIxE' :: [(Int, a)] -> (a -> Text) -> (Text -> Int -> a -> IO b) -> IO (M.Map Text b)
    strMapWithIxE' [] _ _ = pure mempty
    strMapWithIxE' ((i, x) : xs) f g = do
      val <- g (f x) i x
      m <- strMapWithIxE' xs f g
      pure $ M.insert (f x) val m

