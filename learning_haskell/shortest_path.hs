import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import System.Environment
import qualified Text.Printf as T

data Edge = Edge { to :: Int, weight :: Int } deriving Show
data Path = Path { nodes :: [Int], score :: Int } deriving Show
type Road = M.Map Int [Edge]
type Cache = M.Map Int Int

parse_roads :: String -> String -> String -> Road
parse_roads up down trav = let
  (fup,ups,lup)       = decompose_ints up
  (fdown,downs,ldown) = decompose_ints down
  (ftrav:travs)       = map read (words trav)
  road0               = init_road fup fdown ftrav
  road1               = ifoldl add_road_section road0 (zip3 ups downs travs)
  last_node           = M.size road1
  road2               = add_edge road1 (last_node - 2) (Edge last_node lup)
  road3               = add_edge road2 (last_node - 1) (Edge last_node ldown)
  in road3

decompose_ints:: String -> (Int, [Int], Int)
decompose_ints s = let
  i:ints = map read (words s)
  in (i, init ints, last ints)

init_road :: Int -> Int -> Int -> Road
init_road u d t = let
  road0 = M.fromList []
  road1 = M.insert 0 [(Edge 1 u), (Edge 2 d)] road0
  road2 = M.insert 1 [Edge 2 t] road1
  road3 = M.insert 2 [Edge 1 t] road2
  in road3

data Wrap t = Wrap t Int
ifoldl :: (acc -> Int -> elt -> acc) -> acc -> [elt] -> acc
ifoldl f a es = let
  ini = Wrap a 0
  wrapped_res = foldl (\(Wrap x y) e -> Wrap (f x y e) (y + 1)) ini es
  Wrap res _ = wrapped_res
  in res

add_road_section :: Road -> Int -> (Int, Int, Int) -> Road
add_road_section road idx (u,d,t) = let
  n0 = 2 * idx + 1
  n1 = n0 + 1
  s0 = road
  s1 = add_edge s0 n0 (Edge (n0 + 2) u)
  s2 = add_edge s1 n1 (Edge (n1 + 2) d)
  s3 = add_edge s2 (n0 + 2) (Edge (n1 + 2) t)
  s4 = add_edge s3 (n1 + 2) (Edge (n0 + 2) t)
  in s4

add_edge :: Road -> Int -> Edge -> Road
add_edge g from (Edge to cost) = M.insert from (get_value from g) g
  where get_value k m = case (M.lookup k m) of
          Just v  -> (Edge to cost):v
          Nothing -> [Edge to cost]

showRoad :: Road -> String
showRoad = showRoad' . M.toList
  where showRoad' [] = ""
        showRoad' ((k, es):is) = let
          line = T.printf "%d -> %s\n" k (foldl (\a (Edge to cost) -> a ++ T.printf "%d/%d, " to cost) "" es)
          in line ++ showRoad' is

shortest_path :: Road -> Path
shortest_path road = let 
  ini_cache             = M.fromList [] :: Cache
  ini_path              = Path [0] 0
  (res_cache, res_path) = shortest_path' ini_cache ini_path 
  in res_path
  where shortest_path' cache path = let
          Path ns tcost    = path
          edges            = fromMaybe [] (M.lookup (head ns) road)
          paths            = [Path (to:ns) (cost + tcost) | Edge to cost <- edges]
          (paths',cache')  = prune_paths_update_cache paths cache
          (fcache, fpaths) = foldl (\(c,ps) p -> let (c',p') = shortest_path' c p in (c',p':ps)) (cache',[]) paths'
          mpath            = foldl choose_max path fpaths
          in (fcache, mpath)

prune_paths_update_cache :: [Path] -> Cache -> ([Path], Cache)
prune_paths_update_cache [] cache = ([], cache)
prune_paths_update_cache (path:paths) cache = let
  (paths', cache') = prune_paths_update_cache paths cache
  Path (to:_) cost = path
  cache_cost       = fromMaybe (maxBound :: Int) (M.lookup to cache')
  cache''          = if cache_cost > cost then M.insert to cost cache' else cache'
  paths''          = if cache_cost > cost then path:paths' else paths'
  in (paths'', cache'')

choose_max :: Path -> Path -> Path
choose_max apath path = let
  Path ato acost = apath
  Path to cost   = path
  better_new     = (to > ato) || ((to == ato) && (cost < acost))
  in if better_new then path else apath

main = do
  (up_lane:down_lane:trav_lane:_) <- getArgs
  graph <- return $ parse_roads up_lane down_lane trav_lane
  putStrLn $ showRoad graph
  print $ shortest_path graph

