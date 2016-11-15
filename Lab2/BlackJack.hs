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
= 1 + (1 + size Empty)
= 1 + (1 + 0)
= 1 + 1
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
-- Task 4
-- Given two hands, <+ puts the first one on top of the second one
(<+) :: Hand -> Hand -> Hand
Empty <+ hand2 = hand2
(Add card hand) <+ hand2 = Add card (hand <+ hand2)

-- Tests if <+ is associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3


-- Tests that the size of the two hands is the same as the combined hand
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = 
    size (hand1 <+ hand2) == (size hand1 + size hand2)

-- Creates a full deck with 52 cards
fullDeck :: Hand
fullDeck = listToHand (cardsInSuit Hearts) 
        <+ listToHand (cardsInSuit Diamonds) 
        <+ listToHand (cardsInSuit Spades) 
        <+ listToHand (cardsInSuit Clubs)

-- Creates a list of all cards in a suit
cardsInSuit :: Suit -> [Card]
cardsInSuit suit = [Card (Numeric a) suit | a <- [2..10]] 
                ++ [Card val suit | val <- [Jack, Queen, King, Ace]]

-- Creates a hand from a list of cards
listToHand :: [Card] -> Hand
listToHand [] = Empty
listToHand [card] = Add card Empty
listToHand (card:xs) = Add card (listToHand xs)


-- Tests the number of cards in the full deck
prop_size_FullDeck :: Bool
prop_size_FullDeck = size fullDeck == 52


-- Tests the number of cards in a suit
prop_size_cardsInSuit :: Suit -> Bool
prop_size_cardsInSuit suit = length (cardsInSuit suit) == 13

-- Tests that the number of cards in the list is the same as in the hand
prop_size_listToHand :: [Card] -> Bool
prop_size_listToHand cards = size (listToHand cards) == length cards


-- Draws the first card of the deck
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add firstCard deck) hand = (deck, Add firstCard hand)

-- Plays for the bank
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

-- Follows the rules for the bank
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand 
    | value bankHand' > 15 = bankHand'
    | otherwise = playBank' deck' bankHand'
    where (deck',bankHand') = draw deck bankHand

-- Shuffles the hand given and returns a shuffled hand
shuffle' :: StdGen -> Hand -> Hand
shuffle' _ Empty = Empty
shuffle' _ (Add card Empty) = Add card Empty
shuffle' g hand  =  Add card (shuffle' g' hand')
    where 
        (index, g') = randomR (0, size hand -1) g
        (hand', card) = removeCardAtIndex hand index

-- Removes the card at the given index
removeCardAtIndex :: Hand -> Integer -> (Hand,Card)
removeCardAtIndex Empty _ = error "removeCardAtIndex: The deck is empty"
removeCardAtIndex (Add card hand) 0  = (hand,card)
removeCardAtIndex (Add card hand) index 
  | index > size hand || index < 0   = error "removeCardAtIndex: invalid idx"
  | otherwise                        = (Add card hand', card')
  where (hand', card') = removeCardAtIndex hand (index-1)


-- Checks that the card is still in the hand after it has been shuffled
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle' g h

-- Checks if a card is in a hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = (c == c') || (c `belongsTo` h)

-- Checks that the number of cards are the same after it has been shuffled
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (shuffle' g hand)



implementation = Interface
    { iEmpty    = empty
      , iFullDeck = fullDeck
      , iValue    = value
      , iGameOver = gameOver
      , iWinner   = winner 
      , iDraw     = draw
      , iPlayBank = playBank
      , iShuffle  = shuffle'
    }

main :: IO ()
main = runGame implementation
