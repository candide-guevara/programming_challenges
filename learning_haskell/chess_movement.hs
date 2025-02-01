import Data.Set

data Pos = Pos Int Int deriving (Eq, Ord, Show)
data Mov = Mov Int Int deriving Show

add :: Pos -> Mov -> Pos
add (Pos a b) (Mov x y) = Pos (a Prelude.+ x) (b Prelude.+ y)

in_bounds :: Pos -> Bool
in_bounds (Pos x y) = (elem x [0..7]) && (elem y [0..7])

type Gen = Pos -> Set Pos
gen_horse :: Gen
gen_horse p = let moves = [Mov 2 1, Mov 1 2, Mov (-1) 2, Mov (-2) 1, Mov (-2) (-1), Mov (-1) (-2), Mov 1 (-2), Mov 2 (-1)]
  in fromList [add p m | m <- moves, in_bounds $ add p m]

trace_gen :: Gen -> [Pos] -> Set [Pos]
trace_gen g ps = let raw_hist = Data.Set.map (:ps) (g $ head ps)
  in Data.Set.filter (\l -> (length l) == (length $ fromList l)) raw_hist

--instance Monad Set where
--  return = singleton
--  fail _ = empty
(>>=) :: Ord b => Set a -> (a -> Set b) -> Set b
(>>=) s f = let l = toList s in Prelude.foldl union empty (Prelude.map f l)

reachability :: Gen -> Int -> Pos -> Set Pos
reachability g 0 p = singleton p
reachability g t p = (g p) Main.>>= reachability g (t-1)

reach_trace :: Gen -> Int -> [Pos] -> Set [Pos]
reach_trace g 0 p = singleton p
reach_trace g t p = (trace_gen g p) Main.>>= reach_trace g (t-1)

path_to_from :: Gen -> Int -> Pos -> Pos -> Set [Pos]
path_to_from g t i f = Data.Set.filter (\l -> f == (head l)) $ reach_trace g t [i]

main = do
  print $ reachability gen_horse 2 (Pos 4 4)
  print $ reachability gen_horse 2 (Pos 0 0)
  print $ reach_trace gen_horse 2 [Pos 0 0]
  print $ path_to_from gen_horse 2 (Pos 3 3) (Pos 0 0)

