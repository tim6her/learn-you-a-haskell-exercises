import Control.Applicative
import Data.Monoid
import Control.Exception

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show, Eq)

-- Make the list a Functor
instance Functor List where
    fmap f (Value x xs) = Value (f x) $ fmap f xs
    fmap _ Empty = Empty

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty other = other
combineLists other Empty = other
combineLists (Value a as) b = Value a $ combineLists as b

-- Make our list a Monoid
instance Monoid (List a) where
    mempty = Empty
    mappend xs ys = combineLists xs ys

-- Make our list an Applicative
instance Applicative List where
        pure x = Value x Empty
        Empty <*> _ = Empty
        _ <*> Empty = Empty
        (Value f fs) <*> ys = mappend (fmap f ys) (fs <*> ys)

-- Make sure that the List obeys the laws for Applicative and Monoid
main = do
    let phi = pure (*2) :: List (Int -> Int)
        psi = foldr Value Empty [(*6), (^2), flip div 2] :: List (Int -> Int)
        f = (^3) :: Int -> Int
        g = (*9) :: Int -> Int
        u = foldr Value Empty [0..9]
        v = foldr Value Empty [(-3)..6]
        w = foldr Value Empty [7..19]
        x = 42
        y = 12
        tests = [
                -- Functor laws
                fmap id u == id u,
                fmap (f . g) u == fmap f (fmap g u),
                -- Applicative laws
                (pure f <*> u) == fmap f u,
                (pure id <*> v) == v,
                (pure (.) <*> phi <*> psi <*> u) == (phi <*> (psi <*> u)),
                (pure f <*> pure x) == (pure (f x) :: List Int),
                (phi <*> pure y) == (pure ($ y) <*> phi),
                -- Monoid laws
                mempty `mappend` u == u,
                u `mappend` mempty == u,
                (u `mappend` v) `mappend` w == u `mappend` (v `mappend` w)
                ]
    return $ assert (all id tests) ()


-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty
u = foldr Value Empty [0..9]
v = foldr Value Empty [(-3)..6]
w = foldr Value Empty [7..19]

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)
l1 = plusTwo <$> twoValueList

-- Use <$> and <*> on the lists with a binary function
l2 = (*) <$> u <*> v

-- Create some lists of binary functions
phi = pure (*2) :: List (Int -> Int)
psi = foldr Value Empty [(*6), (^2), flip div 2] :: List (Int -> Int)

-- Use <*> on the binary functions list and the number lists--
l3 = phi <*> u
l4 = psi <*> v