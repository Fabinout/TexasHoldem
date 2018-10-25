module Cards 
where

type Card = (Rank, Suit) 
data Suit = Hearts | Spades | Diamonds | Clubs deriving (Eq) 
data Rank = Two | Three| Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen|King| Ace deriving (Eq, Ord) 

rank :: Card -> Rank
rank = fst

suit :: Card -> Suit
suit = snd

cards :: String -> [Card]
cards = (map card) . words

card :: String -> Card
card [r, s] = (charToRank r, charToSuit s )

charToSuit :: Char -> Suit
charToSuit 'H' = Hearts
charToSuit 'S' = Spades
charToSuit 'D' = Diamonds
charToSuit 'C' = Clubs

charToRank :: Char -> Rank
charToRank '2' = Two 
charToRank '3' = Three 
charToRank '4' = Four 
charToRank '5' = Five 
charToRank '6' = Six 
charToRank '7' = Seven 
charToRank '8' = Eight 
charToRank '9' = Nine 
charToRank 'T' = Ten 
charToRank 'J' = Jack 
charToRank 'Q' = Queen 
charToRank 'K' = King 
charToRank 'A' = Ace