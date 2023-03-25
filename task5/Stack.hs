module Stack where

newtype Stack a = Stack [a]
    deriving (Show, Eq)


push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pushN :: [a] -> Stack a -> Stack a
pushN x (Stack xs) = Stack (x ++ xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack [])     = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack xs) = Just $ head xs


popN :: Int -> Stack a -> (Maybe [a], Stack a)
popN n (Stack xs) = (Just $ take n xs, Stack $ drop n xs)

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _          = False

makeStack :: Stack a
makeStack = Stack []