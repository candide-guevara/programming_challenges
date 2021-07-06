import qualified Data.List
import qualified Data.Maybe
import qualified BasePrelude as Base
import qualified Text.Printf as Text

side_k = 9
area_k = side_k * side_k
empty_k = '0'
all_syms_k = "123456798"
all_syms_with_empty_k = empty_k:all_syms_k
type Symbol = Char
type Symbols = [Symbol]

data Score = Score { score_syms::Symbols, score_r::Int, score_y::Int } deriving Show
data Board = Board { raw::[Symbols] } deriving (Eq, Ord)
instance Show Board where
  show b = let show_row = foldr (\c a -> c:" " ++ a) ""
               rows = map show_row (raw b)
           in  foldr (\r a -> ' ':r ++ "\n" ++ a) "" rows

empty_row_k   = [empty_k,empty_k,empty_k,empty_k,empty_k,empty_k,empty_k,empty_k,empty_k]
empty_board_k = Board [empty_row_k,empty_row_k,empty_row_k,empty_row_k,empty_row_k,empty_row_k,empty_row_k,empty_row_k,empty_row_k]

from_string :: String -> Board
from_string s = let nums = filter (\x -> elem x all_syms_with_empty_k) s
                    (rows,_) = split_rows nums side_k
                in Base.assert (length nums == area_k) (Board rows)

split_rows :: Symbols -> Int -> ([Symbols], Symbols)
split_rows nums len | (length nums) > len = let (row,rest)   = splitAt len nums
                                                (tail,rest') = split_rows rest len
                                            in  (row:tail, rest')
                    | otherwise           = ([nums], [])

at :: Board -> Int -> Int -> Symbol
at b i j = raw b !! i !! j

-- https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
set :: Board -> Int -> Int -> Symbol -> Board
set b i j s = let (l_row,r_row) = splitAt j (row b i)
                  new_row       = l_row ++ (s:(tail r_row)) 
                  (top_rows,bottom_rows) = splitAt i (raw b)
              in  Board (top_rows ++ (new_row:(tail bottom_rows)))

row :: Board -> Int -> Symbols
row b i = raw b !! i

col :: Board -> Int -> Symbols
col b i = let s = (length $ raw b) - 1
          in map (\r -> raw b !! r !! i) [0..s]

block :: Board -> Int -> Int -> Symbols
block b i j = let m = floor $ sqrt $ fromIntegral $ length $ raw b
                  mi = m * (quot i m)
                  mj = m * (quot j m)
                  rf i' = map (\r -> raw b !! i' !! r) [mj..(mj+2)]
              in [mi..(mi+2)] >>= rf

coords :: Board -> [(Int, Int)]
coords b = let side_idx = [0..(side_k-1)]
           in  [(,)] <*> side_idx <*> side_idx

board_fold :: Board -> (Board -> Int -> Int -> t) -> [t]
board_fold b f = map (\(r, c) -> f b r c) (coords b)

board_full :: Board -> Bool
board_full b = let is_not_empty b_ r c = (at b_ r c) /= empty_k
               in  and $ board_fold b is_not_empty

sym_substraction :: Symbols -> Symbols -> Symbols
sym_substraction op1 op2 = filter (\x -> not $ elem x op2) op1

candidates_at :: Board -> Int -> Int -> Symbols
candidates_at b r c = let missing_in_row =   sym_substraction all_syms_k (row b r)
                          missing_in_col =   sym_substraction missing_in_row (col b c)
                          missing_in_block = sym_substraction missing_in_col (block b r c)
                      in  missing_in_block

best_candidate :: Board -> Maybe Score
best_candidate b = let scores = candidate_helper b
                       comparator s1 s2 = compare (length $ score_syms s1) (length $ score_syms s2)
                   in  Data.Maybe.listToMaybe $ Data.List.sortBy comparator scores

candidate_helper :: Board -> [Score]
candidate_helper b = let score_at b_ r c | (at b_ r c) == empty_k = Score (candidates_at b_ r c) r c
                                         | otherwise              = Score [] r c
                         unfiltered_candidates = board_fold b score_at
                     in  filter (\s -> (score_syms s) /= []) unfiltered_candidates

solver :: Board -> Maybe Board
solver b = solver_helper b (best_candidate b)

solver_helper :: Board -> (Maybe Score) -> Maybe Board
solver_helper b Nothing | board_full b = Just b
                        | otherwise    = Nothing
solver_helper b (Just (Score [] r c)) = Base.assert False (Just b)
solver_helper b (Just (Score candidates r c)) = let all_sols = map (solver . (set b r c)) candidates
                                                    good_sols = Data.Maybe.catMaybes all_sols
                                                in  Data.Maybe.listToMaybe good_sols

---------------------------------------------------------------------------------------------------

b1 = from_string "530070000 600195000 098000060 800060003 400803001 700020006 060000280 000419005 000080079"
b_done = from_string "534678912 672195348 198342567 859761423 426853791 713924856 961537284 287419635 345286179"
b_almost_done = from_string "534678912 672195348 198342567 859761403 426853791 713924856 961537284 287419635 345286179"
bad_b1 = set b1 1 1 '9'

main = do
  move1@(Just (Score syms1 r1 c1)) <- return $ best_candidate b1
  b2 <- return $ set b1 r1 c1 (head syms1)
  move2@(Just (Score syms2 r2 c2)) <- return $ best_candidate b2
  move3@(Just (Score syms3 r3 c3)) <- return $ best_candidate b_almost_done
  b3 <- return $ set b_almost_done r3 c3 (head syms3)
  print b1
  --print bad_b1
  --print move1
  --print b2
  --print move2
  --print move3
  --print b3
  print $ Data.Maybe.fromMaybe empty_board_k $ solver b1
  -- print $ Data.Maybe.fromMaybe empty_board_k $ solver bad_b1
  --print $ Data.Maybe.fromMaybe empty_board_k $ solver b_almost_done
  --print $ Data.Maybe.fromMaybe empty_board_k $ solver b_done

