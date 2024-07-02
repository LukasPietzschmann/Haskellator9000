module Math.SiConverter.Internal.Utils.Stack (Stack, isEmpty, pop, push, top) where

newtype Stack a = Stack [a]

instance Functor Stack where
    fmap f (Stack xs) = Stack $ fmap f xs

instance Applicative Stack where
    pure x = Stack [x]
    Stack fs <*> Stack xs = Stack $ fs <*> xs

instance Semigroup (Stack a) where
    Stack xs <> Stack ys = Stack $ xs <> ys

instance Monoid (Stack a) where
    mempty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (a, Stack a)
pop (Stack (x:xs)) = (x, Stack xs)
pop _              = error "empty stack"

top :: Stack a -> a
top (Stack (x:_)) = x
top _             = error "empty stack"

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _          = False
