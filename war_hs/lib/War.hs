module War (deal) where

import Data.List

{--
Function stub(s) with type signatures for you to fill in are given below. 
Feel free to add as many additional helper functions as you want. 

The tests for these functions can be found in src/TestSuite.hs. 
You are encouraged to add your own tests in addition to those provided.

Run the tester by executing 'cabal test' from the war directory 
(the one containing war.cabal)
--}
    
-- Function to play the card game
deal :: [Int] -> [Int]
deal shuf = 
    let shuf' = reverse shuf
        (player1, player2) = distributeCards shuf' [] [] 0
        winner = playRecursive player1 player2
    in
        map mapAce winner

-- Distribute cards to players
distributeCards :: [Int] -> [Int] -> [Int] -> Int -> ([Int], [Int])
-- Base case: If cursor is 52, return the players' card lists
distributeCards (card:shuf) player1 player2 52 = (player1, player2)
-- Base case: If shuf is empty, return the players' card lists
distributeCards [] player1 player2 cursor = (player1, player2)
-- Recursive case: Distribute cards to players
distributeCards (card:shuf) player1 player2 cursor =
    let card' = remapAce card
        remainder = cursor `rem` 2
        player1' = if remainder == 0 then player1 ++ [card'] else player1
        player2' = if remainder == 1 then player2 ++ [card'] else player2
    in       
        distributeCards shuf player1' player2' (cursor + 1)

-- Play the game recursively
playRecursive :: [Int] -> [Int] -> [Int]
-- Base case: If player1 has no cards, return player2's cards
playRecursive [] player2 = player2
-- Base case: If player2 has no cards, return player1's cards
playRecursive player1 [] = player1
-- Recursive case: Continue playing the game
playRecursive player1 player2 = 
    let (card1, card2, player1', player2') = dealTwoCards player1 player2
    in
        if card1 == card2
            then
                let (player1'', player2'') = fightWar player1' player2' [card1, card2]
                in playRecursive player1'' player2''
            else
                let (player1'', player2'') =
                        if card1 > card2
                            then (player1' ++ [card1, card2], player2')
                            else (player1', player2' ++ [card2, card1])
                in
                    playRecursive player1'' player2''

-- Deal two cards from players
dealTwoCards :: [Int] -> [Int] -> (Int, Int, [Int], [Int])
dealTwoCards player1 player2 =
    let (card1, player1') = distributeOneCard player1
        (card2, player2') = distributeOneCard player2
    in
        (card1, card2, player1', player2')

-- Distribute one card from a player's deck
distributeOneCard :: [Int] -> (Int, [Int])
-- Pattern match: Extract the first card and the remaining deck
distributeOneCard (card:deck) = (card, deck)

-- Resolve a war between players
fightWar :: [Int] -> [Int] -> [Int] -> ([Int], [Int])
-- Base case: If player1 has no cards, return player2's cards
fightWar [] player2 warCards =  
    let card1 = warCards !! (length warCards - 2)
        card2 = warCards !! (length warCards - 1)
    in
        if card1 > card2
        then (reverse (sort warCards), player2)
        else ([], player2 ++ reverse (sort warCards))

-- Base case: If player2 has no cards, return player1's cards
fightWar player1 [] warCards =  
    let card1 = warCards !! (length warCards - 2)
        card2 = warCards !! (length warCards - 1)
    in
        if card2 > card1
        then (player1, reverse (sort warCards))
        else (player1 ++ reverse (sort warCards), [])

-- Recursive case: Continue resolving the war
fightWar player1 player2 warCards =
    let card1 = warCards !! (length warCards - 2)
        card2 = warCards !! (length warCards - 1)
    in
        if card1 == card2
            then
                let (faceDown1, faceDown2, player1', player2') = dealTwoCards player1 player2
                    warCards' =  [faceDown1, faceDown2] ++ warCards
                in
                if not (null player1' || null player2')
                    then
                        let (card1', card2', player1'', player2'') = dealTwoCards player1' player2'
                            warCards'' = warCards' ++ [card1', card2']
                        in
                        fightWar player1'' player2'' warCards''

                    else
                        fightWar player1' player2' warCards'
            else
                if card1 > card2
                    then (player1 ++ reverse (sort warCards), player2)
                    else (player1, player2 ++ reverse (sort warCards))

-- Remap the Ace card (1 to 14)
remapAce :: Int -> Int
remapAce card = if card == 1 then 14 else card

-- Map the Ace card back to 1 (14 to 1)
mapAce :: Int -> Int
mapAce card = if card == 14 then 1 else card
               
    
