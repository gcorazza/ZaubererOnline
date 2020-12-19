import Test.HUnit
import Wizard

main :: IO ()
main = do _ <- runTestTT tests
          return ()
          
tests = test [ 
               "Trick Test 1" ~: whosePlayersTrick [Narr Yellow, Card 1 Yellow, Narr Red] players Nothing  ~=? (players!!1),
               "Trick Test 2" ~: whosePlayersTrick [Card 2 Yellow, Card 1 Yellow, Narr Red] players Nothing  ~=? (players!!2),
               "Trick Test 3" ~: whosePlayersTrick [Narr Yellow, Narr Red, Narr Red] players Nothing  ~=? (players!!0),
               "Trick Test 4" ~: whosePlayersTrick [Card 1 Yellow, Card 1 Blue, Card 1 Red] players Nothing  ~=? (players!!0),
               "Trick Test 5" ~: whosePlayersTrick [Card 1 Yellow, Card 2 Red, Card 1 Red] players Nothing  ~=? (players!!1),
               "Trick Test 6" ~: whosePlayersTrick [Card 2 Red, Card 1 Yellow, Card 1 Red] players Nothing  ~=? (players!!2),
               "Trick Test 7" ~: whosePlayersTrick [Card 2 Red, Wizard Blue, Card 1 Red] players Nothing  ~=? (players!!1),
               "Trick Test 8" ~: whosePlayersTrick [Card 2 Red, Narr Blue, Card 1 Yellow] players Nothing  ~=? (players!!0),
               "Trick Test 9" ~: whosePlayersTrick [Card 2 Red, Narr Blue, Card 1 Yellow] players Nothing  ~=? (players!!0),
               "Trick Test 10" ~: whosePlayersTrick [Card 2 Red, Narr Blue, Narr Yellow] players Nothing  ~=? (players!!2),
               "Trick Test 11" ~: whosePlayersTrick [Card 3 Yellow, Card 2 Yellow, Card 1 Yellow] players Nothing  ~=? (players!!2),
               "Trick Test 12" ~: whosePlayersTrick [Card 3 Yellow, Card 2 Yellow, Card 4 Yellow] players Nothing  ~=? (players!!0),
               "Trick Test 13" ~: whosePlayersTrick [Card 3 Yellow, Card 2 Yellow, Card 4 Red] players Nothing  ~=? (players!!0),
               "Trick Test 14" ~: whosePlayersTrick [Card 3 Yellow, Card 2 Yellow, Wizard Red] players Nothing  ~=? (players!!0),
               "Trick Test 15" ~: whosePlayersTrick [Card 3 Yellow, Wizard Red, Wizard Red] players Nothing  ~=? (players!!0),
               "Trick Test 16" ~: whosePlayersTrick [Wizard Red, Wizard Red, Wizard Red] players Nothing  ~=? (players!!0),
               "Trick Test 17" ~: whosePlayersTrick [Wizard Red, Wizard Red, Card 1 Red] players Nothing  ~=? (players!!1),
               "Trick Test 18" ~: whosePlayersTrick [Wizard Red, Card 1 Red, Card 1 Red] players Nothing  ~=? (players!!2),
               "Trick Test 19" ~: whosePlayersTrick [Wizard Red, Card 1 Red, Card 1 Red] players Nothing  ~=? (players!!2)
             ]

players = player iGame
iGame = initGame ["Player1", "Player2", "Player3"]
