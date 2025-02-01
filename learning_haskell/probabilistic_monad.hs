import qualified Control.Monad as C
import qualified Data.Map.Strict as M
import qualified Data.Ratio as R
import qualified Text.Printf as T

newtype PList a = PList { pList :: [(a, Rational)] }

instance Functor PList where
  fmap = C.liftM

instance Applicative PList where
  pure  = return
  (<*>) = C.ap

instance Monad PList where
  return a = PList [(a,1)]
  m >>= f = PList $ concat $ map f' $ pList m
    where f' (a,p) = map (\(a',p') -> (a',p*p')) (pList $ f a)

coin_toss :: Int -> PList Int
coin_toss x = PList [(x, 1 R.% 2), (x + 1, 1 R.% 2)]

dice_roll :: Int -> PList Int
dice_roll x = PList $ map (\x' -> (x+x',1 R.% 6)) [1..6]

multi_rand_event :: t -> Int -> (t -> PList t) -> PList t
multi_rand_event initial rounds event = f initial
  where f = foldr (C.<=<) return (replicate rounds event)

lookup_default :: Ord k => k -> (M.Map k v) -> v -> v
lookup_default k m d = case M.lookup k m of
  (Just x) -> x
  otherwise -> d

to_histo :: PList Int -> M.Map Int Rational
to_histo (PList []) = M.fromList []
to_histo (PList ((a,p):xs)) = let
  m = to_histo (PList xs)
  p' = lookup_default a m 0
  in M.insert a (p'+p) m

main = do
  let raw_results = multi_rand_event 0 8 dice_roll
  let histo = M.toList $ to_histo raw_results
  print histo

