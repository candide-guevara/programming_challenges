import Data.Time.Clock
import Control.Monad.Writer
import qualified Text.Printf as T

data DList a = DList { innerL :: [a] -> [a] }

dList :: a -> DList a
dList l = DList (\x -> [l] ++ x)

instance Semigroup (DList a) where
  (<>) (DList x) (DList y) = DList (x . y)
instance Monoid (DList a) where
  mempty = DList id

slow_append :: Int -> Writer [String] ()
slow_append 0 = return ()
slow_append i = do
  slow_append (i-1)
  tell [show i]

fast_append :: Int -> Writer (DList String) ()
fast_append 0 = return ()
fast_append i = do
  fast_append (i-1)
  tell $ dList $ show i

get_log :: Writer [String] () -> [String]
get_log w = snd $ runWriter w

discard :: Writer [String] () -> String
discard w = last $ snd $ runWriter w

discard2 :: Writer (DList String) () -> String
discard2 w = last $ (innerL $ snd $ runWriter w) []

main = do
  print $ discard $ slow_append 1000
  print $ discard2 $ fast_append 1000
  t0 <- getCurrentTime
  print $ discard2 $ fast_append 10000
  t1 <- getCurrentTime
  print $ discard2 $ fast_append 20000
  t2 <- getCurrentTime
  let d1 = diffUTCTime t1 t0
  let d2 = diffUTCTime t2 t1
  T.printf "d1 = %s\n" (show d1)
  T.printf "d2 = %s\n" (show d2)


