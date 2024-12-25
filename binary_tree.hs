data BinTree a =
    Nil |
    Node {
        left :: BinTree a,
        right :: BinTree a,
        value :: a,
        count :: Int
    }


instance Show a => Show (BinTree a) where
    show = show0 0 where
        show0 _ Nil = "Nil"
        show0 lvl Node{left=l, right=r, value=v, count=cnt} =
            "Node (v = " ++ show v ++ ", count = " ++ show cnt ++ ")\n" ++
            replicate lvl '\t' ++ "l=" ++ show0 (lvl + 1) l ++ "\n" ++
            replicate lvl '\t' ++ "r=" ++ show0 (lvl + 1) r ++ "\n"



insert :: Ord a => BinTree a -> a -> BinTree a
insert Nil x = Node {left = Nil, right = Nil, value = x, count = 1}
insert Node{left=l, right=r, value=v, count=cnt} x
    | x == v    = Node {left = l, right = r, value = v, count = cnt + 1}
    | x < v     = Node {left = insert l x, right = r, value = v, count = cnt}
    | otherwise = Node {left = l, right = insert r x, value = v, count = cnt}

fromList :: Ord a => [a] -> BinTree a
fromList = foldl insert Nil



findMin :: Ord a => BinTree a -> Maybe a
findMin Nil = Nothing
findMin Node{left=Nil, value=v} = Just v
findMin Node{left=l} = findMin l

findMax :: Ord a => BinTree a -> Maybe a
findMax Nil = Nothing
findMax Node{right=Nil, value=v} = Just v
findMax Node{right=r} = findMax r



treeSort :: Ord a => BinTree a -> [a]
treeSort Nil = []
treeSort Node{left=l, right=r, value=v, count=cnt} =
    treeSort l ++ replicate cnt v ++ treeSort r



findAny :: Ord a => (a -> Bool) -> BinTree a -> Maybe a
findAny _ Nil = Nothing
findAny p Node{left=l, right=r, value=v}
    | p v       = Just v
    | otherwise = case findAny p l of
        Just res -> Just res
        Nothing  -> findAny p r



isSearchTree :: Ord a => BinTree a -> Bool
isSearchTree = isSearchTree' Nothing Nothing where
    isSearchTree' _ _ Nil = True
    isSearchTree' minVal maxVal Node{left=l, right=r, value=v} =
        maybe True (v >) minVal &&
        maybe True (v <) maxVal &&
        isSearchTree' minVal (Just v) l &&
        isSearchTree' (Just v) maxVal r



testTree :: BinTree Int
testTree = Node {
    left  = Node {
        left  = Node {left  = Nil, right = Nil, value = 1, count = 1},
        right = Node {
            left  = Node {
                left = Nil, right = Nil, value = 4, count = 1},
            right = Node {
                left = Nil, right = Nil, value = 7, count = 1},
            value = 6, count = 1
        },
        value = 3, count = 1
    },
    right = Node {
        left  = Nil,
        right = Node {
            left  = Node {
                left = Nil, right = Nil, value = 13, count = 1},
            right = Nil,
            value = 14, count = 1
        },
        value = 10, count = 1
    },
    value = 8, count = 1
}

