module BlackJack where
import Cards
import RunGame

-- Part A

{-
Task 3.1
Task 3.2
Task 3.3 - empty, value, gameOver, winner.

-}

-- 3.1
{-
size hand2
= size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
= 1 + size (Add (Card Jack Spades) Empty)
= 1 + 1 + size Empty
= 1 + 1 + 0
= 2

-}

empty :: Hand
empty = Empty

value :: Hand -> Integer
value hand 
	| val <= 21 = val
	| otherwise = val - 10 * numberOfAces hand 
	where val = valueCards hand


valueCards :: Hand -> Integer
valueCards Empty = 0
valueCards (Add card hand) = valueCard card + valueCards hand 

valueRank :: Rank -> Integer
valueRank rank = 
	case rank of 
		Ace -> 11
		King -> 10
		Queen -> 10
		Jack -> 10
		Numeric n -> n

valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

numberOfAces :: Hand -> Integer
numberOfAces Empty           = 0
numberOfAces (Add (Card Ace _) hand)  = 1 + numberOfAces hand
numberOfAces (Add _ hand) = numberOfAces hand


gameOver :: Hand -> Bool
gameOver hand = value hand > 21 

winner :: Hand -> Hand -> Player
winner guestHand bankHand 
	| gameOver guestHand = Bank
	| gameOver bankHand = Guest
	| value guestHand > value bankHand = Guest
	| otherwise = Bank