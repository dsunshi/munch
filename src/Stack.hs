
module Stack where

data Stack a = Stack [a]

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack [])     = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _          = False
