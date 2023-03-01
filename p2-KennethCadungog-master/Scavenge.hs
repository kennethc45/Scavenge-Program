module Scavenge where
import Dictionaries
import Data.List (sort)
import Debug.Trace

--                                          Type Aliases
-- These type aliases help to abstract our code. 
-- 
type Hand = [Char]
type Move = String
type Play = [Move]
type Dictionary = [String] 

-- All undefined values and functions should be completed. Your code will compile and test 
-- (with the --test flag) even if some functions are left undefined.
--
--                                       Milestone
--

-- Score takes a move and returns the sum of the letter values, according to the Scrabble scoring
-- system: https://scrabble.hasbro.com/en-us/faq
-- A helper function may be useful.
letter :: Char -> Integer
letter x 
    |(x == 'A') = 1
    |(x == 'E') = 1
    |(x == 'I') = 1
    |(x == 'O') = 1
    |(x == 'U') = 1
    |(x == 'L') = 1
    |(x == 'N') = 1
    |(x == 'S') = 1
    |(x == 'T') = 1
    |(x == 'R') = 1
    |(x == 'D') = 2
    |(x == 'G') = 2
    |(x == 'B') = 3
    |(x == 'C') = 3
    |(x == 'M') = 3
    |(x == 'P') = 3
    |(x == 'F') = 4
    |(x == 'H') = 4
    |(x == 'V') = 4
    |(x == 'W') = 4
    |(x == 'Y') = 4
    |(x == 'K') = 5
    |(x == 'J') = 8
    |(x == 'X') = 8
    |(x == 'Q') = 10
    |(x == 'Z') = 10
    |otherwise = error "Not a letter"    

score :: Move -> Integer
score [] = 0
score (x:xs) = letter x + score xs

-- score "QA" == 11
-- score "JF" == 12

-- scorePlay takes a play and returns the total score of all words.
scorePlay :: Play -> Integer
scorePlay [ ] = 0
scorePlay (x:xs) = score x + scorePlay xs 

-- scorePlay ["KB", "QA"] == 19 

-- remove takes an element and a list, and returns the list with one copy of that element removed.
-- You should not assume the list is sorted. If there are multiple copies of the element in the list,
-- only remove one of them. If the element doesn't occur, you should throw an error.
remove :: Eq a => a -> [a] -> [a]   
remove x [ ] = error "Element not found"
remove x (y:ys) 
    |(x == y) = ys
    |otherwise = y : remove x ys

-- remove 7 [7,3,1,7,5] = [3,1,7,5] 
-- The order here doesn't matter, if you remove the second 7 it is okay.

-- updateHand should take a hand (a list of characters), and a move (a string), and return the hand
-- that remains after that move is played.
updateHand :: Hand -> Move -> Hand
updateHand hand [ ] = hand
updateHand hand (y:ys) = updateHand (remove y hand) ys

-- updateHand "HELLO" "LO" = "HEL"

-- canMake takes a hand and a move, and tells you if that move can be made with that hand. Be sure to
-- consider the possibility that a letter may occur more times in the move than it does in the hand.
canMake :: Hand -> Move -> Bool
canMake hand [ ] = True
canMake hand (y:ys) = if y `elem` hand then canMake (remove y hand) ys else False
 --
-- "DNAHTSET" `canMake` "HAND" = True 
-- "DNAHTSET" `canMake` "HAAND" = False
-- For full credit, this must run in near-linear time (n log n is sufficient)


-- isValidMove tests if a move is valid with respect to a dictionary and hand: 
-- the move must be a word in the dictionary and a move that can be made with the hand.
isValidMove :: Dictionary -> Hand -> Move -> Bool
isValidMove dictionary hand [ ]= True
isValidMove dictionary hand (y:ys) = if (canMake hand (y:ys) == True) then (if (y:ys) `elem` dictionary then True else False ) else False
-- isValidMove tinyDict "MKEKIOCHAUX" "MAKE" = TRUE
-- isValidMove tinyDict "MKEKIOCHAUX" "MAXKE" = FALSE
-- isValidMove tinyDict "MKEKIOCHAUX" "FAKE" = FALSE


-- isValidPlay checks if a play is valid. Each move in the play must be a word in the dictionary, and
-- must be playable using whatever remains of the hand after all previous moves have been made.
removeString :: Eq a => [a] -> [a] -> [a]   
removeString [ ] e = error "String not found"
removeString [a] e = [ ]
removeString (x:xs) e = if x `elem` e then remove x e else error "Not an element in the lsit"

isValidPlay :: Dictionary -> Hand -> Play -> Bool
isValidPlay dictionary hand [ ] = True
isValidPlay dictionary hand (y:ys) 
    | isValidMove dictionary hand y = isValidPlay dictionary (updateHand hand y) ys
    | otherwise = False
-- isValidPlay tinyDict "TMAKE" ["TAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["MAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["TAKE","MAKE"] = False
    
-- validMoves: takes a dictionary and a hand, and returns all words that can be
-- created by letters in the hand. Order does not matter.
validMoves :: Dictionary -> Hand -> [Move]
validMoves dictionary hand = [x | x <- dictionary, isValidMove dictionary hand x]
-- validMoves shortDict "PEMDOVZIJM" = ["DIE","DO","I","ME","MOVE","MOVIE","PM"]


--                                  End of Milestone!

--                                  Core Project 

-- --- Brute Force Algorithms
-- You are going to search for the best play, instead of just choosing the best move one at a time.
-- To do so, you will consider every possible play, by considering every possible combination of
-- words. You will implement two variants. 
 
-- powerset: return the powerset of the input, i.e. the list of all sub-lists.
-- You may assume the list has no duplicates. 
-- The output should not have duplicates, up to sorting.
powerset :: [a] -> [[a]]
powerset [] =  [[]]
powerset [x] = [[], [x]]
powerset (x:xs) = [x : y | y <- p] ++ p 
    where p = powerset xs

-- powerset [1,2] = [[],[1],[1,2],[2]]
-- It is acceptable to have [2,1] INSTEAD of [1,2], but not in addition.
-- length (powerset "abcde") = 32

-- The Naive Brute Force (naiveBrutePlay) takes every combination of moves in
-- the dictionary: the powerset of the dictionary. It will only work with very small
-- dictionaries, like tenWords.  You will then choose the best valid play out of that list.
naiveBrutePlay :: Dictionary -> Hand -> Play
naiveBrutePlay dictionary hand = 
    let playLst = powerset dictionary -- [x | x <- dictionary, isValidMove dictionary hand x]
    in snd(maximum [(scorePlay x, x) | x <- playLst, isValidPlay dictionary hand x])
    -- let powerLst = powerset dictionary 
        -- playLst = [x | x <- powerLst, isValidPlay dictionary hand x] --The problem is here
    -- in  snd(maximum[(scorePlay x, x) | x <- playLst])
    
-- The issue might be that I need to update hand to prevent letters from being reused. However, recycling of letters should not be possible since isValidPlay
-- would filter out plays that recycle letters. Weird.


-- The Smart Brute Force approach realizes that we can restrict the size of the dictionary
-- before we compute the powerset. There are probably MANY moves in the dictionary that we can't
-- create at all! So, first find all the moves that can be made with this hand. Then, take the
-- powerset of that restricted list to create a list of all plays made up of those moves. Then
-- find the best valid play from that list.
smartBrutePlay :: Dictionary -> Hand -> Play
smartBrutePlay dictionary hand = 
    let validMoves = [x | x <- dictionary, isValidMove dictionary hand x]
        validPlays = [x | x <- powerset validMoves, isValidPlay dictionary hand x]
    in snd(maximum[(scorePlay x, x) | x <- validPlays])

--  I may have a list of valid moves, but I still need to check which play of valid moves are viable.

-- --- Greedy Algorithm

-- greedyPlay: choose the best move you can at any given point in time, then check to see what
-- other moves you can make.


greedyPlay :: Dictionary -> Hand -> Play
greedyPlay dictionary [ ] = [ ]
greedyPlay dictionary hand  =           
      let moves = validMoves dictionary hand 
          bstMove = snd(maximum[(score x, x) | x <- moves])
          newHand = updateHand hand bstMove
      in if moves == [ ] then [ ] else greedyPlay dictionary newHand ++ [bstMove]

-- greedyPlay shortDict "CLOSEFLOOR" = ["FORCE", "SO"] 


-- --- Recursive Game-Tree Algorithm

-- Finally we will try a recursive strategy to find the best play. Even the smart brute force
-- fails for large hands: I can make a lot of moves, but not a lot of moves simultaniously. Thus
-- the list of all possible plays is large, but the list of all valid plays is still likely
-- small. 
-- For this algorithm, start with the list of valid moves. Then, for each move find the
-- best play for the remaining hand. Select the hand that leads to the best overall score, counting both
-- the score of the move and the score of the best remaining play.

helper :: Dictionary -> Hand -> (Integer, [Move])
helper [ ] [ ] = (0, [ ])
helper [ ] hand = (0, [ ])
helper dictionary [ ] = (0, [ ])
helper (x:xs) hand = if score1 > scoreForMove then (score1, plays1) else (scoreForMove, x:plays2)
    where scoreForMove = score(x) + score2
          (score1, plays1) = helper xs hand
          (score2, plays2) = helper (validMoves (x:xs) (updateHand hand x)) (updateHand hand x)

bestPlay:: Dictionary -> Hand -> Play 
bestPlay dictionary hand = snd (helper valid hand)
    where valid = validMoves dictionary hand 

