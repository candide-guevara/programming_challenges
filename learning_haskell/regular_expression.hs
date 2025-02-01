import Control.Exception as C
import Data.List as L
import Data.Maybe as M
import Debug.Trace as D
import System.Environment
import Text.Printf as T

data Op = Or | And | Star deriving (Show, Eq, Ord)
data RxTree = Empty | Atom Char | RxTree { nodeop::Op, subexp::Bool, t_left::RxTree, t_right:: RxTree } deriving Show
data ParseState = ParseState { tree::RxTree, left::String } deriving Show
data Match = Match { from::Int, to::Int } deriving Show

parse_rx :: String -> RxTree
parse_rx str = let state = ParseState Empty str
                   final_state = parse_rx_helper state
                in tree final_state

parse_rx_helper :: ParseState -> ParseState
parse_rx_helper state@(ParseState t []) = state
parse_rx_helper (ParseState t (c:left)) | c == '('  = parse_rx_helper . merge_subexpr $ ParseState t left
                                        | c == ')'  = ParseState t left
                                        | c == '|'  = parse_rx_helper $ ParseState (RxTree Or False t Empty) left
                                        | c == '*'  = parse_rx_helper $ ParseState (push_star_down t) left
                                        | otherwise = parse_rx_helper $ ParseState (append_atom t c) left

merge_subexpr :: ParseState -> ParseState
merge_subexpr (ParseState t0 ini) = let (ParseState t1 left) = parse_rx_helper $ ParseState Empty ini
                                        t1' = decorate_as_subexpr t1
                                     in (ParseState (merge_trees t0 t1') left)

decorate_as_subexpr :: RxTree -> RxTree
decorate_as_subexpr (RxTree op exp left right) = RxTree op True left right
decorate_as_subexpr t = t

merge_trees :: RxTree -> RxTree -> RxTree
merge_trees Empty t = t
merge_trees (RxTree op False t0 Empty) t1 = RxTree op False t0 t1 
merge_trees t0 t1 = RxTree And False t0 t1 

append_atom :: RxTree -> Char -> RxTree
append_atom Empty c     = Atom c
append_atom (Atom c0) c = RxTree And False (Atom c0) (Atom c)
append_atom tree@(RxTree op exp left right) c | And > op && (not exp) = RxTree op False left (append_atom right c)
                                              | otherwise = RxTree And False tree (Atom c)

push_star_down :: RxTree -> RxTree
push_star_down (Atom c) = RxTree Star False (Atom c) (Atom '*')
push_star_down tree@(RxTree op exp left right) | Star > op && (not exp) = RxTree op False left (push_star_down right)
                                               | otherwise = RxTree Star False tree (Atom '*')

-----------------------------------------------------------------------------------------------------------------------

search :: String -> RxTree -> Maybe Match
search str tree = head_maybe $ search_helper str tree

search_helper :: String -> RxTree -> [Match]
--search_helper str tree  | D.traceShow (str, tree) False = undefined
search_helper str Empty = map (\(i,s) -> Match i i) (zip [0..] str)
search_helper ""  _     = []
search_helper str (Atom c) = map (\(i,s) -> Match i (i+1)) $ filter (\(i,s) -> s == c) (zip [0..] str)
search_helper str (RxTree Or   _ t0 t1) = let left_matches  = search_helper str t0
                                              right_matches = search_helper str t1
                                           in sort_matches left_matches right_matches
search_helper str (RxTree And  _ t0 t1) = let search_after_match = \m -> search_helper_contiguous (substr m str) t1
                                              left_matches   = search_helper str t0
                                              right_matches  = map search_after_match left_matches
                                              merged_matches = map (uncurry fold_merge) (zip left_matches right_matches)
                                           in mconcat merged_matches
search_helper str (RxTree Star _ t0 t1) = let all_pos = search_helper str Empty
                                              chain_matches = map (\m -> search_first_recursive m str t0) all_pos
                                           in chain_matches

search_helper_contiguous :: String -> RxTree -> [Match]
search_helper_contiguous str tree = let matches = search_helper str tree
                                     in takeWhile (\m -> from m == 0) matches

search_first_recursive :: Match -> String -> RxTree -> Match
--search_first_recursive m str tree | D.traceShow (m, str, tree) False = undefined
search_first_recursive m []  _    = m
search_first_recursive m str tree = let left = substr m str
                                        first_m = fmap (merge_matches m) (head_maybe $ search_helper_contiguous left tree)
                                        recurse_m = fmap search_first_recursive first_m
                                        final_m = recurse_m <*> (Just str) <*> (Just tree)
                                     in M.fromMaybe m final_m

sort_matches :: [Match] -> [Match] -> [Match]
--sort_matches ls rs = L.sortOn from (ls ++ rs)
sort_matches ls []         = ls
sort_matches [] rs         = rs
sort_matches (l:ls) (r:rs) = case (from l) <= (from r) of
                             True  -> l : (sort_matches ls (r:rs)) 
                             False -> r : (sort_matches (l:ls) rs) 

merge_matches :: Match -> Match -> Match
merge_matches m1 m2 | (from m2) == 0 = let combined_to = (to m1) + (to m2)
                                        in Match (from m1) combined_to
                    | otherwise      = m1

fold_merge :: Match -> [Match] -> [Match]
fold_merge _ []      = []
fold_merge m (m0:ms) | C.assert (from m0 == 0) False = undefined
fold_merge m (m0:ms) = (merge_matches m m0) : (fold_merge m ms)

substr :: Match -> String -> String
substr m str = drop (to m) str

head_maybe :: [a] -> Maybe a
head_maybe [] = Nothing
head_maybe l  = Just $ head l

-----------------------------------------------------------------------------------------------------------------------

show_tree :: RxTree -> String
show_tree t = show_helper t ""

show_helper :: RxTree -> String -> String
show_helper Empty indent             = indent ++ "Empty"
show_helper (Atom c) indent          = indent ++ "Atom " ++ (show c)
show_helper (RxTree op exp t0 t1) indent = let new_indent = indent ++ "  "
                                               header = indent ++ "RxTree " ++ (show op) ++ " subexpr=" ++ (show exp) ++ "\n"
                                               t0_str = (show_helper t0 new_indent) ++ "\n"
                                               t1_str = (show_helper t1 new_indent)
                                            in header ++ t0_str ++ t1_str

show_match :: String -> (Maybe Match) -> String
show_match _ Nothing    = "<no_match>"
show_match str (Just m) = let width = (to m) - (from m)
                              fragment = (take width) $ (drop $ from m) str
                           in case fragment == "" of
                              True      -> "<empty_match>"
                              otherwise -> fragment

test_parse_rx :: IO [()]
test_parse_rx = let cases = [ "", "abc", "a|b", "ab|cd", "(ab)|(cd)", "a|bc|d", "()", "(a)", "((a))", "(a)(b)", "(ab)(cd)", "a(b|c)", "ab(cd(e|f)g)h",
                              "(ab|)c",
                              "a*", "ab*", "(ab)*", "a(bc)*", "abc*" ]
                    regexes = map parse_rx cases
                    print_pair = \str rx -> T.printf "###### '%s' ######\n%v\n\n" str (show_tree rx)
                    prints = zipWith print_pair cases regexes
                 in sequence prints

test_search_rx :: IO [()]
test_search_rx = let strings = ["", "a", "a", "ab", "ab", "ab", "xabx", "xbx", "aaa", "abc" , "aab", "xaxaax", "abc"     , "xabbaabx", "xabybccdfhy" ]
                     rx_strs = ["", "a", "b", "yx", "ax", "ab", "ab",   "a|b", "a*" , "ac*" , "a*" , "a*"    , "(b|c|a)*", "aaa*b"   , "a*b(g|c*d(e|f))h" ]
                     expects = ["", "a", "",  "",   "",   "ab", "ab",   "b",   "aaa", "a"   , "aa" , ""      , "abc"     , "aab"     , "bccdfh" ]
                     regexes = map parse_rx rx_strs
                     results = map (uncurry search) (zip strings regexes)
                     print_one_test = \str rx m_m expect -> T.printf "###### '%s' =~ /%s/ ######\n'%s' (expected: '%s')\n\n" str rx (show_match str m_m) expect
                     prints = L.zipWith4 print_one_test strings rx_strs results expects
                  in sequence prints

-----------------------------------------------------------------------------------------------------------------------

main = do 
  --(rx_str:str:_) <- getArgs
  print $ (show $ search_helper "xabc" (parse_rx "(x|y|)(c|a)"))
  -- test_parse_rx
  --test_search_rx
  T.printf "done\n"

