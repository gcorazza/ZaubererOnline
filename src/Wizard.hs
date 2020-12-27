{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Wizard where

import Data.List
import Data.Maybe
import System.Random
import Prelude hiding (round)

data Color = Red | Yellow | Blue | Green deriving (Eq, Show)

data Card
  = Card
      { number :: Int,
        color :: Color
      }
  | Wizard {color :: Color}
  | Narr {color :: Color}
  deriving (Eq, Show)

data Player = Player
  { name :: !String,
    handCards :: ![Card],
    said :: !Int,
    tricks :: Int
  }
  deriving (Eq, Show)

data Action
  = PlayCard Card
  | EndGame

data Phase = Guess | Play deriving (Show)

data GameState = GameState
  { player :: [Player],
    playerOrder :: [Int],
    mid :: [Card],
    phase :: Phase,
    startedRound :: Int,
    trump :: Maybe Color,
    round :: Int
  }

instance Show GameState where
  show GameState{..} = "player: " ++ show player ++ "\n" ++
                       "order: " ++ show (take (length player) playerOrder) ++ "\n" ++
                       "mid: " ++ show mid ++ "\n" ++
                       "phase: " ++ show phase ++ "\n" ++
                       "startedRound: " ++ show startedRound ++ "\n" ++
                       "trump: " ++ show trump ++ "\n" ++
                       "round: " ++ show round ++ "\n"
                       

newtype Trump = Trump (Maybe Color) deriving (Show)

newtype ServeColor = ServeColor (Maybe Color) deriving (Show)

iGame = initGame ["Player1", "Player2", "Player3"]

initGame :: [String] -> IO GameState
initGame names = do deck <- newDeck
                    let gs = GameState player (cycle [0 .. (length player - 1)]) [] Guess 0 Nothing 1
                    return $ newCardsGS deck 1 gs
    where player = initPlayer <$> names

initPlayer :: String -> Player
initPlayer name = Player name [] 0 0

shuffle :: [a] -> StdGen -> [a]
shuffle [] _ = []
shuffle xs gen = fst <$> sortOn snd (zip xs (randoms gen :: [Int]))

newDeck :: IO [Card]
newDeck = do
  shuffle
    ( [ Wizard Red,
        Wizard Blue,
        Wizard Yellow,
        Wizard Green,
        Narr Red,
        Narr Blue,
        Narr Yellow,
        Narr Green
      ]
        ++ ((`Card` Red) <$> [1 .. 13])
        ++ ((`Card` Blue) <$> [1 .. 13])
        ++ ((`Card` Yellow) <$> [1 .. 13])
        ++ ((`Card` Green) <$> [1 .. 13])
    )
    <$> newStdGen

deleteCardFrom :: Player -> Card -> Player
deleteCardFrom p@Player {..} delC = p {handCards = filter (== delC) handCards}

playerHasCard :: Player -> Card -> Bool
playerHasCard Player {..} c = c `elem` handCards

playerAllowedToPlayCard :: GameState -> Card -> Bool
playerAllowedToPlayCard (GameState _ _ _ Guess _ _ _) _ = False
playerAllowedToPlayCard (GameState _ _ _ Play _ _ _) (Wizard _) = True
playerAllowedToPlayCard (GameState _ _ _ Play _ _ _) (Narr _) = True
playerAllowedToPlayCard (GameState _ _ [] Play _ _ _) _ = True
playerAllowedToPlayCard (GameState player order mid Play _ _ _) (Card _ colorLaid) =
  isNothing firstColor
    || fromJust firstColor == colorLaid
    || not (hasColorOnHand colorLaid hand)
  where
    firstColor = serveColor mid
    hand = handCards $ player !! head order

hasColorOnHand :: Color -> [Card] -> Bool
hasColorOnHand colorTest hand = elem colorTest $ color <$> hand

canPlayCard :: GameState -> Card -> Bool
canPlayCard gs@GameState {..} card =
  playerHasCard (player !! head playerOrder) card
    && playerAllowedToPlayCard gs card

playCard :: Card -> GameState -> GameState
playCard card gs@GameState {..} =
  if canPlayCard gs card
    then
      gs
        { player = replace player currentPlayer (deleteCardFrom currentPlayer card),
          playerOrder = tail playerOrder,
          mid = card : mid
        }
    else gs
  where
    currentPlayer = player !! head playerOrder

replace :: Eq a => [a] -> a -> a -> [a] -- not in prelude??
replace l from to =
  ( \p ->
      if p == from
        then to
        else p
  )
    <$> l

trickRoundEnded :: GameState -> Bool
trickRoundEnded GameState {..} = length mid == length player

whosePlayersTrick :: [Card] -> [Player] -> Maybe Color -> Player
whosePlayersTrick mid p trump =
  snd $
    foldl1
      ( \(c1, p1) (c2, p2) ->
          if firstCardStronger c1 c2 (ServeColor firstColor) (Trump trump)
            then (c1, p1)
            else (c2, p2)
      )
      (zip (reverse mid) p)
  where
    firstColor = serveColor mid

firstCardStronger :: Card -> Card -> ServeColor -> Trump -> Bool
firstCardStronger (Wizard _) _ _ _ = True
firstCardStronger _ (Wizard _) _ _ = False
firstCardStronger _ (Narr _) _ _ = True
firstCardStronger (Narr _) _ _ _ = False
firstCardStronger (Card n1 c1) (Card n2 c2) (ServeColor (Just serveCol)) (Trump trump) =
  (Just c1 == trump, c1 == serveCol, n1) > (Just c2 == trump, c2 == serveCol, n2)
firstCardStronger card1 card2 serveCol trump =
  error $
    "compare shouldn't happen 3 " ++ show card1
      ++ show card2
      ++ show serveCol
      ++ show trump

serveColor :: [Card] -> Maybe Color
serveColor cs = serveColorReversed (reverse cs)

serveColorReversed :: [Card] -> Maybe Color
serveColorReversed [] = Nothing
serveColorReversed (Wizard _ : _) = Nothing
serveColorReversed (Narr _ : cards) = serveColorReversed cards
serveColorReversed (Card _ col : _) = Just col

endTrickRound :: GameState -> (GameState, Player)
endTrickRound gs@GameState {..} =
  ( gs
      { player = replace player playerWonTrick updatedWonPlayer,
        mid = []
      },
    playerWonTrick
  )
  where
    playerWonTrick@Player {..} = whosePlayersTrick mid (toPlayerList playerOrder player) trump
    updatedWonPlayer = playerWonTrick {tricks = tricks + 1}

playerToIndex :: Player -> [Player] -> Int
playerToIndex p ps = i
  where
    (Just i) = elemIndex p ps

toPlayerList :: [Int] -> [Player] -> [Player]
toPlayerList [] _ = []
toPlayerList (i : is) player = (player !! i) : toPlayerList is player

newTrickRound :: GameState -> GameState
newTrickRound gs@GameState {..} = ngs {playerOrder = cycleTo (playerToIndex playerWonTrick player) playerOrder}
  where
    (ngs, playerWonTrick) = endTrickRound gs

cycleTo :: Int -> [Int] -> [Int]
cycleTo _ [] = []
cycleTo p (p1 : px) =
  if p == p1
    then p1 : px
    else cycleTo p px

newCards :: [Card] -> [Player] -> Int -> [Player]
newCards _ [] _ = []
newCards cs (p : ps) r = p {handCards = hand} : newCards rest ps r
  where
    (hand, rest) = splitAt r cs

newCardsGS :: [Card] -> Int -> GameState -> GameState
newCardsGS cs r gs@GameState {player} = gs {player = newCards cs player r}

roundEnded :: GameState -> Bool
roundEnded GameState {..} = null $ handCards $ head player

newRound :: GameState -> IO GameState
newRound gs = do
  deck <- newDeck
  return
    ngs
      { player = newCards deck player (round + 1),
        round = round + 1,
        startedRound = cycleTo startedRound playerOrder !! 1,
        playerOrder = cycleTo startedRound playerOrder
      }
  where
    (ngs@GameState {..}, _) = endTrickRound gs

applyAction :: GameState -> Action -> IO GameState
applyAction gs (PlayCard c) =
  if trickRoundEnded afterPlayingCardState
    then (if roundEnded afterPlayingCardState then newRound afterPlayingCardState else return $ newTrickRound afterPlayingCardState)
    else return afterPlayingCardState
  where
    afterPlayingCardState = playCard c gs
applyAction gs _ = return gs
