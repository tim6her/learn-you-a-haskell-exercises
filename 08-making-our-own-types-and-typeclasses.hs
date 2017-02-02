{-
 - We are going to create some types for a deck of cards
 - The cards need to have an ordering, based on the standard ranking http://en.wikipedia.org/wiki/Standard_52-card_deck#Rank_and_color
 - We are assuming Aces are high.
 - Therefore, the following statements should be true:
 -    (Card Ace Spades) > (Card King Spades)
 -    (Card Two Clubs) < (Card Three Clubs)
 -
 - We are going to provide our own implementation of the Show typeclass for the Card type.
 - When displaying the Card instance in GHCI, or calling show (Card digit suit), the String which should be displayed is "The <Digit> of <Suit>"
 -
 - Uncomment the following declarations to complete the implementation, and provide an implementation for instance Show Card
 -}

data Suit = Clubs | Diamonds | Hearts | Spades
     deriving (Eq, Ord, Show, Read, Bounded, Enum)
data FDigit = Jack | Queen | King | Ace
     deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Digit = NumDigit Int | FaceDigit FDigit
     deriving (Eq, Ord)
data Card = Card {suit :: Suit, digit :: Digit}
     deriving (Eq, Ord)

numDigit :: Int -> Digit
numDigit x
    | 1 >= x || x > 10 = error $ "Digit " ++ show x ++ " not in range 2 to 10"
    | otherwise = NumDigit x

instance Show Card where
    show card = "The " ++ (show $ digit card) ++ " of " ++ (show $ suit card)

instance Show Digit where
    show (NumDigit x) = show x
    show (FaceDigit x) = show x

-- We should be able to provide a function which returns the higher ranked card:
betterCard :: Card -> Card -> Card
betterCard = max 

-- Here is a new Typeclass, which represents some kind of playing hand in a game.
-- It returns True for a "winning hand", depending on the rules for the type of class we are playing with
class Hand a where
    play :: [a] -> Bool

-- Implement Hand for Card, where play returns true if the list contains the Ace of Spades
instance Hand Card where
    play c = (Card Spades $ FaceDigit Ace) `elem` c

-- Create a new Coin type
data Coin = Tails | Heads
     deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Implement Hand for Coin, where play returns true if there are ten heads in a row in the list
instance Hand Coin where
    play all@(c:cs) = ((take 10 all) == replicate 10 Heads) || play cs

-- Have a play with implementing Hand for some other types, for instance Int and Bool

instance Hand Int where
    play c = any (/= 0) c

instance Hand Bool where
    play c = any id c