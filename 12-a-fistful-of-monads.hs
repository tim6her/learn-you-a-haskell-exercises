{-
 - Create a type called Validation
 - The type constructor takes one parameter
 - There are two Values: 
 -   Success, which takes that parameter and
 -   Fail String, which represents a failure, and a reason for that failure
 -}
data Validation a = Success a | Fail String deriving (Show, Eq)

-- Make the Validation a Monad
instance Functor Validation where
  fmap f (Success x) = Success (f x)
  fmap _ (Fail msg) = Fail msg

instance Applicative Validation where
  pure = Success
  (Success f) <*> (Success x) = Success (f x)
  (Fail msg) <*> _ = Fail msg
  _ <*> (Fail msg) = Fail msg

instance Monad Validation where
    return = Success
    (Success x) >>= f = f x
    (Fail msg) >>= _ = Fail msg 
    fail = Fail

{-
 - Create a function, positiveCheck, which takes a number and returns a successful Validation if it's positive, 
 - and a failed Validation with a String message if not.
 -}
positiveCheck :: (Num a, Ord a, Show a) => a -> Validation a
positiveCheck x
    | x > 0 = Success x
    | otherwise = Fail $ show x ++ " is not positiv"

{-
 - Create a function, evenCheck, which returns a successful Validation if it's even,
 - and a failed Validation with a string message if it's odd
 -}
evenCheck :: (Integral a, Show a)  =>  a -> Validation a
evenCheck x
    | even x = Success x
    | otherwise = Fail $ show x ++ " is not even" 

{-
 - Write a function which uses positiveCheck and evenCheck to make sure a number is both positive and even
 -}
positiveAndEvenCheck :: (Num a, Ord a, Integral a, Show a) => a -> Validation a
positiveAndEvenCheck x = do
    x' <- positiveCheck x
    x'' <- evenCheck x'
    return x''
