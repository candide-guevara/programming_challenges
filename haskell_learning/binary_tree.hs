import Text.Printf as T

data Node v = Empty | Node { value::v, left::(Node v), right::(Node v) } deriving Show

consChild :: v -> (Node v)
consChild v = Node v Empty Empty

addValue :: Ord v => (Node v) -> v -> Node v
addValue Empty i = consChild i
addValue (Node v l r) i | v <= i    = Node v l (addValue r i)
                        | otherwise = Node v (addValue l i) r

buildTree :: Ord v => (Node v) -> [v] -> Node v
buildTree root is = foldr (\i a -> addValue a i) root is

dFirst :: (Node v) -> [v]
dFirst Empty = []
dFirst (Node v l r) = v:(dFirst l) ++ (dFirst r)

dFirst' :: (Node v) -> [v]
dFirst' Empty = []
dFirst' (Node v l r) = (dFirst' l) ++ v:(dFirst' r)

bFirst  :: (Node v) -> [v]
bFirst n = bFirst' [n] []

bFirst' :: [Node v] -> [v] -> [v]
bFirst' [] l = l
bFirst' (n:ns) ll = case n of
  Empty        -> bFirst' ns ll
  (Node v l r) -> bFirst' (ns ++ [l,r]) (ll ++ [v])

isThere :: Ord v => (Node v) -> v -> Bool
isThere Empty i = False
isThere (Node v l r) i | v == i    = True
                       | v <= i    = isThere r i
                       | otherwise = isThere l i

root = consChild 5
tree = buildTree root [7,1,2,9,3,6,7,8]
ordered_nodes = dFirst' tree
breadth_nodes = bFirst  tree

main = do 
  T.printf "breadth_nodes = %v (expect: [5,3,8,2,7,9,1,6,7])\n" (show breadth_nodes)
  T.printf "depth_nodes = %v (expect: [1,2,3,5,6,7,7,8,9])\n" (show ordered_nodes)
  T.printf "isThere 6 = %v\n"  (show $ isThere tree 6)
  T.printf "isThere 66 = %v\n" (show $ isThere tree 66)

