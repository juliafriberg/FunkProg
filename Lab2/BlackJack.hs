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
Empty <+ hand2 = hand2
(Add card hand) <+ hand2 = (Add card (hand <+ hand2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = 
    size (hand1 <+ hand2) == (size hand1 + size hand2)

fullDeck :: Hand
fullDeck = (listToHand (cardsInSuit Hearts)) <+ (listToHand (cardsInSuit Diamonds)) <+ (listToHand (cardsInSuit Spades)) <+ (listToHand (cardsInSuit Clubs))

cardsInSuit :: Suit -> [Card]
cardsInSuit suit = [Card (Numeric a) suit | a <- [2..10]] ++ [Card val suit | val <- [Jack, Queen, King, Ace]]

listToHand :: [Card] -> Hand
listToHand [] = Empty
listToHand [card] = (Add card Empty)
listToHand (card:xs) = (Add card (listToHand xs))

prop_size_FullDeck :: Bool
prop_size_FullDeck = (size fullDeck) == 52

prop_size_cardsInSuit :: Suit -> Bool
prop_size_cardsInSuit suit = (length (cardsInSuit suit)) == 13

prop_size_listToHand :: [Card] -> Bool
prop_size_listToHand cards = (size (listToHand cards)) == length cards

draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add firstCard deck) hand = (deck, (Add firstCard hand))

playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand 
    | value bankHand' > 15 = bankHand'
    | otherwise = playBank' deck' bankHand'
    where (deck',bankHand') = draw deck bankHand

shuffle' :: StdGen -> Hand -> Hand
shuffle' _ Empty = Empty
shuffle' _ (Add card Empty) = (Add card Empty)
shuffle' g hand  =  (Add card (shuffle' g' hand'))
    where 
        (index, g') = randomR (0, (size hand -1)) g
        (hand', card) = removeCardAtIndex hand index

removeCardAtIndex :: Hand -> Integer -> (Hand,Card)
removeCardAtIndex Empty _ = error "removeCardAtIndex: The deck is empty"
removeCardAtIndex (Add card hand) 0 = (hand,card)
removeCardAtIndex (Add card hand) index 
    | index > size hand || index < 0 = error "removeCardAtIndex: invalid index"
    | otherwise = ((Add card hand'), card')
    where (hand', card') = removeCardAtIndex hand (index-1)


prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle' g h


belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = ((c == c') || (c `belongsTo` h))


prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle = undefined


{-implementation = Interface
    { iEmpty    = empty
      , iFullDeck = fullDeck
      , iValue    = value
      , iGameOver = gameOver
      , iWinner   = winner 
      , iDraw     = draw
      , iPlayBank = playBank
      , iShuffle  = shuffle
    }

main :: IO ()
main = runGame implementation-}
