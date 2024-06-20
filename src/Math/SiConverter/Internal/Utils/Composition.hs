module Math.SiConverter.Internal.Utils.Composition ((.:)) where

-- | Compose two functions. @f .: g@ is similar to @f . g@
-- except that @g@ will be fed /two/ arguments instead of one
-- before handing its result to @f@.
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
