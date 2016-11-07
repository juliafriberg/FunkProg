module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random    

-- PART A

-- Task 3.1
{-
size hand2
= size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
= 1 + size (Add (Card Jack Spades) Empty)
= 1 + 1 + size Empty
= 1 + 1 + 0
= 2

-}

-- Task 4: Functions

-- Returns an empty hand.
empty :: Hand
empty = Empty

-- Tests if the hand returned from empty is actually empty
prop_empty :: Bool
prop_empty = size empty == 0

-- Calculates the value of a hand according to the rules mentioned in 
-- Task 2 in the lab assignment. 
value :: Hand -> Integer
value hand 
    | val <= 21 = val
    | otherwise = val - 10 * numberOfAces hand 
    where val = valueCards hand

-- Calculates the value of all cards in a hand, according to their card value
valueCards :: Hand -> Integer
valueCards Empty = 0
valueCards (Add card hand) = valueCard card + valueCards hand 

-- Returns the value for a specified rank. Ace is always 11.
valueRank :: Rank -> Integer
valueRank rank = 
    case rank of 
        Ace -> 11
        King -> 10
        Queen -> 10
        Jack -> 10
        Numeric n -> n

-- Returns the value of a card according to its rank.
valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

-- Returns the number of aces in the hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty           = 0
numberOfAces (Add (Card Ace _) hand)  = 1 + numberOfAces hand
numberOfAces (Add _ hand) = numberOfAces hand

-- Returns true if the hand is over 21 and false otherwise.
gameOver :: Hand -> Bool
gameOver hand = value hand > 21 

-- Returns the winner (Bank or Guest) according to the rules mentioned in 
-- Task 2 in the lab assignment.
winner :: Hand -> Hand -> Player
winner guestHand bankHand 
    | gameOver guestHand = Bank
    | gameOver bankHand = Guest
    | value guestHand > value bankHand = Guest
    | otherwise = Bank




-- PART B
-- These functions are a part of part B and are therefore not implemented yet

(<+) :: Hand -> Hand -> Hand
(<+) = undefined

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf = undefined


fullDeck :: Hand
fullDeck = undefined

draw :: Hand -> Hand -> (Hand,Hand)
draw = undefined
--error "draw: The deck is empty." when deck is empty

first :: (a, b) -> a
first (x,y) = x

playBank :: Hand -> Hand
playBank = undefined
{-playBank' deck bankHand 
where (deck′,bankHand′) = draw deck bankHand-}

shuffle' :: StdGen -> Hand -> Hand
shuffle' = undefined

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle' g h


belongsTo :: Card -> Hand -> Bool
belongsTo = undefined
{-
c `belongsTo` Empty = False
c `belongsTo` (Add c′ h) = c == c′ || c `belongsTo` h
-}

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle = undefined
