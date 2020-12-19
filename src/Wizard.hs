{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Wizard  where

import Data.Maybe
import Data.List

data Color = Red | Yellow | Blue | Green deriving (Eq, Show)

data Card = Card {
  number :: Int,
  color :: Color
}
 | Wizard  { color :: Color }
 | Narr  { color :: Color }  deriving (Eq, Show)

data Player = Player
  { name :: !String,
    handCards :: ![Card],
    said :: !Int,
    tricks :: Int
  } deriving (Eq, Show)

data Action
  = PlayCard Card
  | EndGame

data Phase = Guess | Play

data GameState = GameState
  { player :: [Player],
    playerOrder :: [Int],
    mid :: [Card],
    phase :: Phase,
    startedRound :: Player,
    trump :: Maybe Color
  }

initGame :: [String] -> GameState
initGame names = GameState player (cycle [0 .. length player]) [] Guess (head player) Nothing
  where player = initPlayer <$> names

initPlayer :: String -> Player
initPlayer name = Player name [] 0 0

deleteCardFrom :: Player -> Card -> Player
deleteCardFrom p@Player {..} delC = p {handCards = filter (== delC) handCards}

playerHasCard :: Player -> Card -> Bool
playerHasCard Player{..} c = c `elem` handCards

playerAllowedToPlayCard :: GameState -> Card -> Bool
playerAllowedToPlayCard (GameState _ _ _ Guess _ _) _ = False
playerAllowedToPlayCard (GameState _ _ _ Play _ _) (Wizard _) = True
playerAllowedToPlayCard (GameState _ _ _ Play _ _) (Narr _) = True
playerAllowedToPlayCard (GameState _ _ [] Play _ _) _ = True
playerAllowedToPlayCard (GameState player order mid Play _ _) (Card _ colorLaid) = isNothing firstColor ||
                                                                                              fromJust firstColor == colorLaid ||
                                                                                              not (hasColorOnHand colorLaid hand)
                                                                                    where firstColor = serveColor mid
                                                                                          hand =handCards $ player !! head order

hasColorOnHand :: Color -> [Card] -> Bool
hasColorOnHand colorTest hand = elem colorTest $ color <$> hand

canPlayCard :: GameState -> Card -> Bool
canPlayCard gs@GameState{..} card = playerHasCard (player !! head playerOrder) card &&
                                    playerAllowedToPlayCard gs card

playCard :: Card -> GameState -> GameState
playCard card gs@GameState{..} = if canPlayCard gs card
                                 then gs{ player = replace player currentPlayer (deleteCardFrom currentPlayer card),
                                          playerOrder = tail playerOrder,
                                          mid = card:mid}
                                 else gs
                           where currentPlayer =player !! head playerOrder

replace :: Eq a => [a] -> a -> a -> [a] -- not in prelude??
replace l from to = (\p -> if p == from
                           then to
                           else p) <$> l

trickRoundEnded ::GameState -> Bool
trickRoundEnded GameState{..} = length mid == length player

whosePlayersTrick :: [Card] -> [Player] -> Maybe Color -> Player
whosePlayersTrick mid p trump = snd $ foldl1 (\(c1,p1) (c2,p2) -> if compareCard c1 c2 firstColor trump
                                                                  then (c1,p1)
                                                                  else (c2,p2))
                                             (zip (reverse mid) p)
                                                          where firstColor = serveColor mid
  
compareCard :: Card -> Card -> Maybe Color -> Maybe Color -> Bool
compareCard (Narr _) (Narr _) _ _= True 
compareCard (Narr _) _ _ _= False 
compareCard (Wizard _) _ _ _ = True
compareCard (Card _ _) (Narr _) _ _ = True
compareCard (Card _ _) (Wizard _) _ _ = False
compareCard card1@(Card n1 c1) card2@(Card n2 c2) (Just serveCol) (Just trump)
  | c1 == trump && c2 == trump = n1 > n2
  | c1 == trump = True
  | c2 == trump = False
  
  | c1 == serveCol && c2 == serveCol = n1 > n2
  | c1 == serveCol = True
  | c2 == serveCol = False
  | otherwise = error $ "compare shouldn't happen 1 " ++ show card1 
                                                      ++ show card2 
                                                      ++ show serveCol
                                                      ++ show trump
compareCard card1@(Card n1 c1) card2@(Card n2 c2) (Just serveCol) Nothing
  | c1 == serveCol && c2 == serveCol = n1 > n2
  | c1 == serveCol = True
  | c2 == serveCol = False
  | otherwise = error $ "compare shouldn't happen 2 " ++ show card1 
                                                      ++ show card2 
                                                      ++ show serveCol
compareCard card1 card2 serveCol trump = error $ "compare shouldn't happen 3 " ++ show card1 
                                                                               ++ show card2 
                                                                               ++ show serveCol 
                                                                               ++ show trump                          

serveColor :: [Card] -> Maybe Color
serveColor cs = serveColorReversed (reverse cs)

serveColorReversed :: [Card] -> Maybe Color
serveColorReversed [] = Nothing
serveColorReversed (Wizard _:_) = Nothing
serveColorReversed (Narr _:cards) = serveColor cards
serveColorReversed (Card _ col:_) = Just col

addTrick :: Player -> Player 
addTrick p@Player{..} = p{tricks = tricks + 1}

playerToIndex :: Player -> [Player] -> Int
playerToIndex p ps = i
  where (Just i) = elemIndex p ps

toPlayerList :: [Int] -> [Player] -> [Player]
toPlayerList [] _ = []
toPlayerList (i:is) player = (player!!i) : toPlayerList is player 

newTrickRound :: GameState -> GameState
newTrickRound gs@GameState{..} = gs{player = replace player playerWonTrick updatedWonPlayer,
                                    mid = [],
                                    startedRound = updatedWonPlayer,
                                    playerOrder = cycleTo (playerToIndex playerWonTrick player) playerOrder }
                                where playerWonTrick = whosePlayersTrick mid (toPlayerList playerOrder player) trump
                                      updatedWonPlayer = addTrick playerWonTrick

cycleTo :: Int -> [Int] -> [Int]
cycleTo _ [] = [] 
cycleTo p (p1:px) = if p == p1
                    then p1:px
                    else cycleTo p px

applyAction :: GameState -> Action -> GameState
applyAction gs (PlayCard c) = if trickRoundEnded afterPlaingCardState
                              then newTrickRound afterPlaingCardState
                              else afterPlaingCardState
                              where afterPlaingCardState = playCard c gs
applyAction gs _ = gs
