import Text.Printf as T

data Node v = Node { value::v, left::(Maybe (Node v)), right::(Maybe (Node v)) } deriving Show

consChild :: v -> Maybe (Node v)
consChild v = Just $ Node v Nothing Nothing

addValue :: Ord v => (Node v) -> v -> Node v
addValue n i | (value n) <= i = 
                case (right n) of
                  Nothing -> Node (value n) (left n) (consChild i)
                  Just c  -> Node (value n) (left n) (Just $ addValue c i)
             | otherwise     =
               case (left n) of
                 Nothing -> Node (value n) (consChild i) (right n) 
                 Just c  -> Node (value n) (Just $ addValue c i) (right n)

buildTree :: Ord v => (Node v) -> [v] -> Node v
buildTree root is = foldr (flip addValue) root is

dFirst :: (Node v) -> [v]
dFirst (Node v Nothing Nothing) = [v]
dFirst (Node v (Just l) Nothing) = v:(dFirst l)
dFirst (Node v Nothing (Just r)) = v:(dFirst r)
dFirst (Node v (Just l) (Just r)) = v:(dFirst l) ++ (dFirst r)

dFirst' :: (Node v) -> [v]
dFirst' (Node v Nothing Nothing) = [v]
dFirst' (Node v (Just l) Nothing) = (dFirst' l) ++ [v]
dFirst' (Node v Nothing (Just r)) = v:(dFirst' r)
dFirst' (Node v (Just l) (Just r)) = (dFirst' l) ++ v:(dFirst' r)

bFirst  :: (Node v) -> [v]
bFirst' :: [Node v] -> [v] -> [v]
bFirst n = bFirst' [n] []
bFirst' [] l = l
bFirst' (n:ns) ll = case n of
  Node v Nothing Nothing   -> bFirst' ns (ll ++ [v])
  Node v (Just l) Nothing  -> bFirst' (ns ++ [l]) (ll ++ [v])
  Node v Nothing (Just r)  -> bFirst' (ns ++ [r]) (ll ++ [v])
  Node v (Just l) (Just r) -> bFirst' (ns ++ [l,r]) (ll ++ [v])

isThere :: Ord v => (Node v) -> v -> Bool
isThere n i | (value n) == i = True
            | (value n) >= i = case (left n) of
              Just c -> isThere c i
              _      -> False
            | otherwise      = case (right n) of
              Just c -> isThere c i
              _      -> False

root = Node 5 Nothing Nothing
tree = buildTree root [7,1,2,9,3,6,7,8]
ordered_nodes = dFirst' tree
breadth_nodes = bFirst tree

main = do 
  print breadth_nodes
  print ordered_nodes
  T.printf "isThere 6 = %v\n" (show $ isThere tree 6)
  T.printf "isThere 66 = %v\n" (show $ isThere tree 66)

