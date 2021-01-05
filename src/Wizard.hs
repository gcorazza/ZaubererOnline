{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-#LANGUAGE OverloadedStrings #-}

module Wizard where

import Data.Aeson
import Data.List
import Data.Maybe
import GHC.Generics
import System.Random
import Prelude hiding (round)

data Color = Red | Yellow | Blue | Green deriving (Eq, Generic)

data Card
  = Card
      { number :: Int,
        color :: Color
      }
  | Wizard {color :: Color}
  | Narr {color :: Color}
  deriving (Eq, Generic)

data Player = Player
  { name :: !String,
    handCards :: ![Card],
    said :: !(Maybe Int),
    tricks :: Int,
    points :: [(Int, Int)]
  }
  deriving (Eq, Show, Generic)

data Action
  = PlayCard Card
  | GuessTricks Int
  | PrintState

data Phase = Guess | Play deriving (Show, Eq, Generic)

data GameState = GameState
  { player :: [Player],
    turnPlayer :: Int,
    mid :: [Card],
    phase :: Phase,
    startedRound :: Int,
    trump :: Maybe Color,
    round :: Int
  }
  deriving (Generic)

instance ToJSON GameState

instance FromJSON GameState

instance ToJSON Player

instance FromJSON Player

instance ToJSON Card

instance FromJSON Card

instance ToJSON Color

instance FromJSON Color

instance ToJSON Phase

instance FromJSON Phase

instance Show Color where
  show Red = "R"
  show Blue = "B"
  show Green = "G"
  show Yellow = "Y"

instance Show Card where
  show (Wizard c) = "W|" ++ show c
  show (Narr c) = "N|" ++ show c
  show (Card n c) = show n ++ "|" ++ show c

instance Show GameState where
  show GameState {..} =
    "player: " ++ show player ++ "\n"
      ++ "Player's turn: "
      ++ show turnPlayer
      ++ "\n"
      ++ "mid: "
      ++ show mid
      ++ "\n"
      ++ "phase: "
      ++ show phase
      ++ "\n"
      ++ "startedRound: "
      ++ show startedRound
      ++ "\n"
      ++ "trump: "
      ++ show trump
      ++ "\n"
      ++ "round: "
      ++ show round
      ++ "\n"

newtype Trump = Trump (Maybe Color) deriving (Show)

newtype ServeColor = ServeColor (Maybe Color) deriving (Show)

iGame = initGame ["Player1", "Player2", "Player3"]
b5 = Card 5 Blue

bugGs::GameState
bugGs = fromJust $ decode "{\"phase\":\"Play\",\"round\":1,\"startedRound\":0,\"turnPlayer\":2,\"mid\":[{\"tag\":\"Narr\",\"color\":\"Blue\"},{\"tag\":\"Card\",\"color\":\"Yellow\",\"number\":13}],\"player\":[{\"points\":[],\"handCards\":[{\"tag\":\"Card\",\"color\":\"Yellow\",\"number\":13}],\"tricks\":0,\"name\":\"Player1\",\"said\":0},{\"points\":[],\"handCards\":[{\"tag\":\"Narr\",\"color\":\"Blue\"}],\"tricks\":0,\"name\":\"Player2\",\"said\":0},{\"points\":[],\"handCards\":[{\"tag\":\"Card\",\"color\":\"Blue\",\"number\":5}],\"tricks\":0,\"name\":\"Player3\",\"said\":0}],\"trump\":null}"

bugGs2::GameState
bugGs2 = fromJust $ decode "{\"phase\":\"Play\",\"round\":3,\"startedRound\":2,\"turnPlayer\":0,\"mid\":[{\"tag\":\"Card\",\"color\":\"Blue\",\"number\":1},{\"tag\":\"Narr\",\"color\":\"Blue\"}],\"player\":[{\"points\":[[1,1],[1,0]],\"handCards\":[{\"tag\":\"Card\",\"color\":\"Green\",\"number\":9},{\"tag\":\"Wizard\",\"color\":\"Blue\"},{\"tag\":\"Card\",\"color\":\"Yellow\",\"number\":1}],\"tricks\":1,\"name\":\"Player1\",\"said\":1},{\"points\":[[0,1],[0,0]],\"handCards\":[{\"tag\":\"Card\",\"color\":\"Blue\",\"number\":12},{\"tag\":\"Card\",\"color\":\"Yellow\",\"number\":3}],\"tricks\":0,\"name\":\"Player2\",\"said\":1},{\"points\":[[2,1],[0,0]],\"handCards\":[{\"tag\":\"Card\",\"color\":\"Green\",\"number\":1},{\"tag\":\"Card\",\"color\":\"Red\",\"number\":1}],\"tricks\":3,\"name\":\"Player3\",\"said\":1}],\"trump\":null}"

debug :: IO GameState
debug = gameLoop $ return bugGs

initGame :: [String] -> IO GameState
initGame names = do
  deck <- newDeck
  let gs = GameState player 0 [] Guess 0 Nothing 1
  return $ newCardsGS deck 1 gs
  where
    player = initPlayer <$> names

initPlayer :: String -> Player
initPlayer name = Player name [] Nothing 0 []

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
playerAllowedToPlayCard (GameState player tp mid Play _ _ _) (Card _ colorLaid) =
  isNothing firstColor
    || fromJust firstColor == colorLaid
    || not (hasColorOnHand (fromJust firstColor) hand)
  where
    firstColor = serveColor mid
    hand = handCards $ player !! tp

hasColorOnHand :: Color -> [Card] -> Bool
hasColorOnHand colorTest hand = elem colorTest $ color <$> hand

canPlayCard :: GameState -> Card -> Bool
canPlayCard gs@GameState {..} card =
  playerHasCard (player !! turnPlayer) card
    && playerAllowedToPlayCard gs card

nextPlayer :: [Player] -> Int -> Int
nextPlayer ps i =
  if i == length ps - 1
    then 0
    else i + 1

playCard :: Card -> GameState -> GameState
playCard card gs@GameState {..} =
  if canPlayCard gs card
    then
      gs
        { player = replace player currentPlayer currentPlayer{handCards = filter (/= card) handCards},
          turnPlayer = nextPlayer player turnPlayer,
          mid = card : mid
        }
    else gs
  where
    currentPlayer@Player{..} = player !! turnPlayer

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

playerOrder :: Int -> [Player] -> [Player]
playerOrder i ps = (ps!!i):playerOrder (i + 1) ps

endTrickRound :: GameState -> (GameState, Player)
endTrickRound gs@GameState {..} =
  ( gs
      { player = replace player playerWonTrick updatedWonPlayer,
        mid = []
      },
    playerWonTrick
  )
  where
    playerWonTrick@Player {..} = whosePlayersTrick mid (playerOrder turnPlayer player) trump
    updatedWonPlayer = playerWonTrick {tricks = tricks + 1}

playerToIndex :: Player -> [Player] -> Int
playerToIndex p ps = i
  where
    (Just i) = elemIndex p ps

toPlayerList :: [Int] -> [Player] -> [Player]
toPlayerList [] _ = []
toPlayerList (i : is) player = (player !! i) : toPlayerList is player

newTrickRound :: GameState -> GameState
newTrickRound gs@GameState {..} = ngs {turnPlayer = playerToIndex playerWonTrick player}
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

playRoundEnded :: GameState -> Bool
playRoundEnded GameState {..} = all (null . handCards) player

guessRoundEnded :: [Player] -> Bool
guessRoundEnded = not . any (isNothing . said)

nothingSaid :: [Player] -> [Player]
nothingSaid ps = (\p -> p{said = Nothing}) <$> ps

newPlayRound :: GameState -> IO GameState
newPlayRound gs = do
  deck <- newDeck
  return
    ngs
      { player = nothingSaid $ newCards deck player (round + 1),
        round = round + 1,
        startedRound = nextPlayer player startedRound,
        turnPlayer = startedRound,
        phase = Guess
      }
  where
    (ngs@GameState {..}, _) = endTrickRound gs

guessAction :: GameState -> Action -> IO GameState
guessAction gs@GameState {..} (GuessTricks tg) =
  if phase == Guess
    then
      return
        gs
          { turnPlayer = nextPlayer player turnPlayer,
            player = uPlayer,
            phase = if guessRoundEnded uPlayer then Play else Guess
          }
    else return gs
  where
    tp = player!!turnPlayer
    uPlayer = replace player tp tp {said = Just tg}
guessAction gs _ = return gs

blockOfTruth :: GameState -> GameState
blockOfTruth gs@GameState {..} =
  gs {player = (\p@Player {..} -> p {points = (tricks, fromJust said) : points}) <$> player}

gameEnded :: GameState -> Bool
gameEnded GameState {..} =
  div 60 (length player) == round
    && all (\Player {..} -> null handCards) player

calcPoints :: GameState -> [[(Int, Int)]]
calcPoints GameState {..} =
  ( \Player {..} ->
      foldr
        (\(p, got) pl -> if null pl
                         then [(p,got)]
                         else (fst (head pl) + p, got):pl)
        []
        (pointFunc <$> points)
  )
    <$> player
  where
    pointFunc = \(said, got) ->
      ( if said == got
          then 2 + said
          else - abs (said - got),
        said
      )

playCardAction :: GameState -> Action -> IO GameState
playCardAction gs (PlayCard c)
  | gameEnded afterPlayingCardState = return $ blockOfTruth afterPlayingCardState
  | playRoundEnded afterPlayingCardState = newPlayRound $ blockOfTruth afterPlayingCardState
  | trickRoundEnded afterPlayingCardState = return $ newTrickRound afterPlayingCardState
  | otherwise = return afterPlayingCardState
  where
    afterPlayingCardState = playCard c gs
playCardAction gs _ = return gs

getAction :: GameState -> IO Action
getAction gs@GameState {phase, player, turnPlayer, round}
  | phase == Guess =
    do
      print $ name tp ++ ":  Guess 0 - " ++ show round
      GuessTricks . read <$> getLine
  | phase == Play =
    do
      print $ name tp
      print $ show <$> handCardsTurnPlayer
      print $ (\n -> " " ++ show n ++ " ") <$> [1 .. length handCardsTurnPlayer]
      choice <- getLine
      if choice == "s"
        then return PrintState
        else return $ PlayCard (handCardsTurnPlayer !! (read choice - 1))
  where
    handCardsTurnPlayer = handCards tp
    tp = player!!turnPlayer

gameLoop :: IO GameState -> IO GameState
gameLoop gs = do
  g <- gs
  if gameEnded g
    then gs
    else do
      print g
      a <- getAction g
      gameLoop $ applyAction g a

applyAction :: GameState -> Action -> IO GameState
applyAction gs a@(PlayCard _) = playCardAction gs a
applyAction gs a@(GuessTricks _) = guessAction gs a
applyAction gs PrintState = do
  print $ encode gs
  return gs

{- todo:
-}
