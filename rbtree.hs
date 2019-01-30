-- Joel Devey

-- Color can be red (R) or black (B)
data Color = R | B deriving (Eq, Show)

-- Tree can be Empty or a Node
data Tree a = Empty | Node (Tree a) a (Tree a) Color deriving Show

-- Auxiliary function used by insert; places a value in a branch
aux :: (Ord p) => p -> Tree p -> Tree p
aux p Empty = Node Empty p Empty R
aux p (Node l v r c)
 | p < v = fix (aux p l) v r c
 -- Do not change the node if the inserted value is the same
 | p == v = Node l v r c
 | p > v = fix l v (aux p r) c

-- Inserts a value. First we use aux to place it into the correct position, then we paint
-- the root black
insert :: (Ord a) => a -> Tree a -> Tree a
insert p n = paintItBlack (aux p n)

-- Fixes a node by balancing it according to the axioms for RB trees
-- The first four cases take a black grandparent, red parent, and red node, and transform
-- them into a red parent with two black children (rotation)
-- If the node's parent is black, then we can just insert it as red without swapping or
-- changing colors.
fix :: Tree a -> a -> Tree a -> Color -> Tree a
-- Left left case
fix (Node (Node ll lv lr R) v rl R) rv rr B = Node (Node ll lv lr B) v (Node rl rv rr B) R
-- Right left case
fix ll lv (Node (Node lr v rl R) rv rr R) B = Node (Node ll lv lr B) v (Node rl rv rr B) R
-- Left right case
fix (Node ll lv (Node lr v rl R) R) rv rr B = Node (Node ll lv lr B) v (Node rl rv rr B) R
-- Right right case
fix ll lv (Node lr v (Node rl rv rr R) R) B = Node (Node ll lv lr B) v (Node rl rv rr B) R
-- Black parent case, just insert the red node
fix l v r c = Node l v r c

-- Take any node and paint it black
paintItBlack (Node l v r c) = Node l v r B

-- depth first search for finding the maximum depth of a tree
dfs :: Tree a -> Integer -> Integer
dfs Empty n = n - 1
dfs (Node l v r c) n = max (dfs l (n + 1)) (dfs r (n + 1))

-- height function, takes any node in a tree and use dfs function
height :: Tree a -> Integer
height a = dfs a 1

