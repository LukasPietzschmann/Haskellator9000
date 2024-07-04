module Math.SiConverter.Internal.Utils.Stack (
      Stack
    , isEmpty
    , mapTop
    , pop
    , push
    , top
    ) where

-- | A simple stack implementation
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

instance Foldable Stack where
    foldr f z (Stack xs) = foldr f z xs

-- | Map a function over the top element of the stack
mapTop :: (a -> a) -- ^ The function to map
       -> Stack a  -- ^ The stack to map over
       -> Stack a  -- ^ The new stack, with the top element mapped
mapTop f (Stack (x:xs)) = Stack (f x:xs)
mapTop _ s              = s

-- | Push an element onto the stack
push :: a       -- ^ The element to push
     -> Stack a -- ^ The stack to push onto
     -> Stack a -- ^ The new stack, containing the element
push x (Stack xs) = Stack (x:xs)

-- | Pop an element from the stack. If the stack is empty, an error is thrown.
pop :: Stack a      -- ^ The stack to pop from
    -> (a, Stack a) -- ^ The popped element and the stack without the element
pop (Stack (x:xs)) = (x, Stack xs)
pop _              = error "empty stack"

-- | Get the top element of the stack. If the stack is empty, an error is thrown.
top :: Stack a -- ^ The stack to get the top element from
    -> a       -- ^ The top element
top (Stack (x:_)) = x
top _             = error "empty stack"

-- | Check if the stack is empty
isEmpty :: Stack a -- ^ The stack to check
        -> Bool    -- ^ 'True' if the stack is empty, 'False' otherwise
isEmpty (Stack []) = True
isEmpty _          = False
