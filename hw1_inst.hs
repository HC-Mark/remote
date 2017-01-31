{- CS380 Assignment 1
   Name:
   College email:
   Resources / collaborators:

   **DUE BEFORE CLASS ON MONDAY, JANUARY 30, 2017.**
   See forthcoming email for submission instructions.

   (This assignment is directly inspired by an assignment in CIS120 at UPenn.)
-}

module Intro where

import Test.HUnit

{- NOTE: you should _not_ use functions in the Haskell standard libraries,
   especially the ones in the Data.List module, except where they are
   explicitly allowed in the comments. The purpose of this assignment is to
   familiarize you with the basics of Haskell programming, so we want you to
   explicitly write out each of these problems even though there is often a
   built-in function that would achieve the same result. You will not receive
   credit for solutions that are contrary to the spirit of the assignment.

   You MAY use helper functions / other top-level definitions as you wish. -}

--------------------------------------------------------------------------------
-- Problem 1 (counting coins)

{- Your job in this problem is to calculate the smallest number of
   pennies, nickels, and dimes that can be used to add up to the given
   amount. For example, to make 7 cents, a total of 3 coins are
   needed (two pennies and a nickel); to make 99 cents, 14 coins are
   needed (9 dimes, 1 nickel, and 4 pennies). Fill in the body of the
   function 'coins' below so that it returns the right answer. Prefer
   guards over `if`/`then`/`else`. -}

coins :: Int -> Int
coins 0 = 0
coins total
         | total >= 10 = (div total 10) + coins (total `mod` 10)
         | total >= 5  = (div total 5) + coins (total `mod` 5)
         | total > 0 = total
         | otherwise = error "invalid amount of money"
--coins = error "coins: unimplemented"
-- I provide two test cases. You must provide two more.
-- See https://hackage.haskell.org/package/HUnit-1.5.0.0/docs/Test-HUnit-Base.html
-- for the functions to create tests. (For now, treat Assertion and Test as
-- interchangeable.)

coins_tests = "coins" ~:
              TestList [ "coins 7"  ~: coins 7  ~?= 3
                       , "coins 99" ~: coins 99 ~?= 14
                       , "coins 81" ~: coins 81 ~?= 9
                       , "coins 1234" ~:coins 1234 ~?= 127 ]

--------------------------------------------------------------------------------
-- Problem 2 (geometry)

{- Sometimes street magicians need to use crates as tables in their
   acts.  Given the dimensions of a crate, find the largest surface
   area it can provide when used as a table.

   Hint: Haskell provides built-in max and min functions that take in two
   arguments and behave exactly as you might expect: `max 5 2` returns 5,
   for example.

   Note: The behavior of this function when at least one of the input side
   lengths is <= 0 is undefined. Your function may return any value in this
   case; we will not test this case on submission. -}

maximumTableArea :: Int -> Int -> Int -> Int
maximumTableArea l w h
               | l == max l (max w h) = l * max w h
               | w == max l (max w h) = w * max l h
               | h == max l (max w h) = h * max l w
maximumTableArea_tests = "maximumTableArea" ~:
                         TestList [ "mta 1 2 3" ~: maximumTableArea 1 2 3 ~?= 6
                                  , "mta 4 3 3" ~: maximumTableArea 4 3 3 ~?= 12
                                  , "mta 10 2 8" ~: maximumTableArea 10 2 8 ~?= 80                 
                                  , "mta 20 1 100" ~: maximumTableArea 20 1 100 ~?= 2000 ]
--maximumTableArea side1 side2 side3 = error "maximumTableArea: unimplemented"
--------------------------------------------------------------------------------
-- Problem 3 (simulating robot movement)

{- Help a robot move along its track (with spaces numbered 0 through
   99) by calculating its new position when given `dir` (equal to
   "forward" or "backward") and `num_moves` indicating a non-negative
   number of spaces.  Keep in mind that the robot can't move past the
   0 or 99 spot so when it reaches either end it stays there. -}

moveRobot :: Int -> String -> Int -> Int
moveRobot now dir step
      | (now < 0) || (now > 99) = error "Invalid start position"
      | (dir == "forward") && (now + step <= 99) = now + step
      | (dir == "forward") && (now + step > 99) = 99
      | (dir == "backward") && (now - step >= 0) = now - step
      | (dir == "backward") && (now - step < 0) = 0
      | otherwise = error "invalid direction"
      
moveRobot_tests = "moveRobot" ~:
                  TestList [ "10 forward 3" ~: moveRobot 10 "forward" 3 ~?= 13
                           , "1 backward 4" ~: moveRobot 1 "backward" 4 ~?= 0
                           , "97 forward 3" ~: moveRobot 97 "forward" 3 ~?= 99
                           , "22 forward 100" ~: moveRobot 22 "forward" 100 ~?= 99 ]
--moveRobot cur_pos dir num_moves = error "moveRobot: unimplemented"
--------------------------------------------------------------------------------
-- Problem 4 (Philadelphia geography)

{- Philadelphia has a fairly logical layout: the numbered streets
   are typically one-way, and their direction is determined by their
   number and where you are in the city.

   Even streets go one way and odd streets go another:

     East of Broad (<14th): even go south, odd go north
     West of Broad (>14th): even go north, odd go south
     West Philly  (>=32nd): even go south, odd go north
     West Philly  (>=46th): two-way

   There are, however, a few exceptions.
     - 1st and 14th do not actually exist as street names -- they're
       called Front and Broad. We'll ignore this and pretend they do.
     - Broad (14th), 25th, 38th, 41st, and 42nd are all two-way.
     - 24th and 59th go south.
     - 58th goes north.

   Write a program that returns one of four string values for each street
   number:
     - "N/A" when the street doesn't exist. We only consider Front
       (=1st) through 69th Streets.
     - "N" when the street goes north.
     - "S" when the street goes south.
     - "NS" when the street is two-way.
     - you might find the infix 'mod' (modulo) function useful:
           (x mod 2)
       evaluates to 0 if x is even and 1 otherwise.
     - sometimes there's no 'simple' way of writing down complex case
       analysis... -}

streetDirection :: Int -> String
streetDirection num
 | (num <= 0) || (num > 69) = error "we only consideer 1st through 69th street"
 | (num >= 46) || (num == 14) || (num == 25) || (num == 41)||
   (num == 42) || (num == 38) = "NS"
 | (num >= 32) && ( num `mod` 2 == 0) = "S"
 | (num >= 32) && ( num `mod` 2 == 1) = "N"
 | (num `mod` 2 == 0) && (num > 14) = "N"
 | (num `mod` 2 == 1) && (num > 14) = "S"
 | (num `mod` 2 == 0) && (num < 14) = "S"
 | (num `mod` 2 == 1) && (num < 14) = "N"

streetDirection_tests = "streetDirection" ~:
                        TestList [ "14" ~: streetDirection 14 ~?= "NS"
                                 , "9"  ~: streetDirection 9  ~?= "N"
                                 , "18" ~: streetDirection 18 ~?= "N"
                                 , "32" ~: streetDirection 32 ~?= "S"
                                 , "21" ~: streetDirection 21 ~?= "S" ]

--------------------------------------------------------------------------------
-- Problem 5 (exists)

{- Write a function that determines whether at least one boolean value
   in its input list is true. -}

exists :: [Bool] -> Bool
exists [] = error "it is am empty list"
exists (x:xs)     
   | x == True = True
   | length xs == 0 = x
   | otherwise = exists xs

exists_tests = "exists" ~:
               TestList [ "FF"  ~: exists [False, False]       ~?= False
                        , "TFT" ~: exists [False, True, False] ~?= True
                        , "TTFT" ~: exists [True, True, False, True] ~?= True
                        , "FFFFF" ~: exists [False, False, False, False, False] ~?= False ]

--------------------------------------------------------------------------------
-- Problem 6 (join)

{- Write a function that takes a list of strings and "flattens" it
   into a single string. This function also takes an additional
   argument, a separator string, which is interspersed between all of
   the strings in the list. -}

join :: String -> [String] -> String
join sing [] = ""
join sing [x] = x
join sing (x:xs) = x ++ sing ++ join sing xs 

join_tests = "join" ~:
             TestList [ ", abc" ~: join "," ["a", "b", "c"] ~?= "a,b,c"
                      , "abc"   ~: join ""  ["a", "b", "c"] ~?= "abc"
                      , "empty" ~: join "," []              ~?= ""
                      , "Mark" ~:  join "" ["M","a","r","k"] ~?= "Mark"
                      , ",I am the best" ~: join "," ["I", " am", " the", " best"] ~?= "I, am, the, best" ]

--------------------------------------------------------------------------------
-- Problem 7 (finding dolls in a toy store)

{- Write a function that checks whether a list of toys contains some
   particular toy. -}

containsStr :: [String] -> String -> Bool
containsStr [] aim = False
containsStr (x:xs) aim = x == aim || containsStr xs aim

containsStr_tests
  = "containsStr" ~:
    TestList [ "barbie" ~:
               containsStr ["truck", "barbie", "top"] "barbie" ~?= True
             , "woody" ~:
               containsStr ["truck", "barbie", "top"] "woody"  ~?= False
             , "Zeus" ~:containsStr ["truck", "barbie", "top"] "Zeus" ~?= False
             , "panda" ~: containsStr ["truck", "barbie", "top"] "panda" ~?= False ]

{- Next, write a function that, given a list of toys and a list of
   dolls, filters the toys list so that only dolls remain. Your
   function should return a list containing all the elements of a
   given list of toy names that appear in a given list of doll
   names. -}

dollsOf :: [String]  -- all toys
        -> [String]  -- dolls
        -> [String]  -- the toys that are dolls
dollsOf toy doll
        | (length doll == 1) && (containsStr toy (head doll) == True) = head doll:[]
        | containsStr toy (head doll) == True = head doll: dollsOf toy (tail doll)
        |otherwise = []

dollsOf_tests
  = "dollsOf" ~:
    TestList [ "barbie" ~:
               dollsOf ["truck", "barbie", "top"] ["barbie", "woody"]
                 ~?= ["barbie"]
             , "none" ~:
               dollsOf [] ["barbie", "woody"] ~?= []
             , "reverse" ~: dollsOf ["truck", "barbie", "top"] ["top", "barbie", "truck"] ~?= ["top", "barbie", "truck"]
             , "empty" ~:dollsOf [] [] ~?= [] ]

--------------------------------------------------------------------------------
-- Problem 8 (merging lists)

{- Write a function that merges two input lists into a single list
   that contains all the elements from both input lists in alternating order:
   the first, third, etc. elements come from the first input list and
   the second, fourth, etc. elements come from the second input list.

   The lengths of the two lists may not be the same -- any
   extra elements should appear at the very end of the result. -}

merge :: [a] -> [a] -> [a]
merge [] [] = []
merge list1 list2
    | length list1 == 0 = head list2:[] ++ merge [] (tail list2)
    | length list2 == 0 = head list1:[] ++ merge (tail list1) []
    | otherwise = head list1:[] ++ head list2:[] ++ merge (tail list1) (tail list2) 
      
   -- (x:xs) (y:ys) 
   -- |length (x:xs) == 0 = y:[] ++ merge [] ys
   -- |length (y:ys) == 0 = x:[] ++ merge xs []
   -- |otherwise = x:[] ++ y:[] ++ merge xs ys -- why it does not work?

merge_tests = "merge" ~:
              TestList [ "1 through 8" ~: merge [1,3,5,7] [2,4,6,8] ~?= [1..8]
                       , "empty list"  ~: merge [1,2,3]   []        ~?= [1,2,3]
                       , "prime" ~: merge [2,3,5][11,13,15] ~?= [2,11,3,13,5,15]
                       , "lazy" ~: merge[1,1,1,1] [0,0,0,0] ~?= [1,0,1,0,1,0,1,0] ]

--------------------------------------------------------------------------------
-- Problem 9 (is_sorted)

{- Write a function that determines whether a given list of integers
   is SORTED -- that is, whether the elements appear in ascending
   order. It is okay if the list has repeated elements, so long as they
   are next to each other.

   For the purposes of this function, we consider lists containing zero
   or one elements to be sorted. -}

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted list = head list <= head(tail list) && isSorted (tail list)
  -- | head list > head(tail list) = False
  -- | (length list == 2) && (head list <= head(tail list)) = True
  -- | head list <= head(tail list) = isSorted (tail list) || True

isSorted_tests = "isSorted" ~:
                 TestList [ "123" ~: isSorted [1,2,3] ~?= True
                          , "321" ~: isSorted [3,2,1] ~?= False
                          , "159357" ~: isSorted [1,5,9,3,5,7] ~?= False
                          , "13579" ~: isSorted [1,3,5,7,9] ~?= True ]

--------------------------------------------------------------------------------
-- Problem 10 (merge_sorted)

{- Write a function that takes two sorted lists (in ascending order)
   and yields a merged list that is also sorted and contains all the
   elements from the two input lists. -}

mergeSorted :: [Int] -> [Int] -> [Int]
mergeSorted [] [] = []
--mergeSorted [] _ = head list2:[] ++ mergeSorted [] (tail list2)
--mergeSorted _ [] = head list1:[] ++ mergeSorted (tail list1) []
mergeSorted list1 list2
    | length list1 == 0 = head list2:[] ++ mergeSorted [] (tail list2)
    | length list2 == 0 = head list1:[] ++ mergeSorted (tail list1) []
    | head list1 <= head list2 = head list1:[] ++ mergeSorted (tail list1) list2
    | head list1 > head list2 = head list2:[] ++ mergeSorted list1 (tail list2)
    | otherwise = []

mergeSorted_tests
  = "mergeSorted" ~:
    TestList [ "primes"     ~: mergeSorted [2,7] [3,5,11] ~?= [2,3,5,7,11]
             , "sequential" ~: mergeSorted [1,2,3] [4,5,6] ~?= [1,2,3,4,5,6]
             , "even" ~: mergeSorted [2,8,18,20] [4,10,16,30] ~?= [2,4,8,10,16,18,20,30]
             , "odd" ~: mergeSorted [1,3,9,19] [5,7,15,21] ~?= [1,3,5,7,9,15,19,21] ]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- NOTE:
--   From here on out (other than the challenge problem), **NO RECURSION** is
--   allowed. Instead, use the library functions `map`, `filter`, and `zipWith`.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Problem 11 (evens)

{- Write a function that takes a list of integers and returns a list containing
   only the even numbers from the input list. -}

evensOnly :: [Int] -> [Int]
evensOnly list = filter even' list 
       where
         even' :: Int -> Bool
         even' x
                | x `mod` 2 == 0 = True
                | otherwise = False
evensOnly_tests = "evensOnly" ~:
                  TestList [ "12345" ~: evensOnly [1,2,3,4,5] ~?= [2,4]
                           , "2468"  ~: evensOnly [2,4,6,8]   ~?= [2,4,6,8]
                           , "10101010" ~: evensOnly [1,0,1,0,1,0,1,0] ~?= [0,0,0,0]
                           , "prime" ~: evensOnly[2,3,5,7,9] ~?= [2] ]

--------------------------------------------------------------------------------
-- Problem 12 (squares)

{- Write a function that takes a list of integers and returns a list containing
   the squares of the numbers in the input list. -}

squares :: [Int] -> [Int]
squares list = zipWith (*) list list

squares_tests = "squares" ~:
                TestList [ "123"  ~: squares [1,2,3]    ~?= [1,4,9]
                         , "negs" ~: squares [-1,-2,-3] ~?= [1,4,9]
                         , "prime" ~: squares [2,3,5] ~?= [4,9,25]
                         , "large" ~: squares[180,444,246] ~?= [32400,197136,60516] ]

--------------------------------------------------------------------------------
-- Problem 13 (wurble)

{- Write a function that takes a list of integers and returns a list containing
   the squares of the negative integers in the input list, as long as that square's
   last digit is a 6. -}

wurble :: [Int] -> [Int]
wurble list = zipWith (*) (filter helper list) (filter helper list)
   where
     helper :: Int -> Bool
     helper x  = x < 0 && head (zipWith (*) [x] [x]) `mod` 10 == 6

wurble_tests = "wurble" ~:
               TestList [ "negs" ~: wurble [-1,-2,-3,-4,-5] ~?= [16]
                        , "neg6" ~: wurble [1,2,3,4,5,-6]   ~?= [36]
                        , "double_neg" ~: wurble [1,2,3,-4,5,-6] ~?= [16,36]
                        , "6_list" ~: wurble [6,16,-16,-36,-6] ~?= [256,1296,36]  ]

--------------------------------------------------------------------------------
-- Problem 14 (sums)

{- Write a function that takes two lists of integers and returns a list
   containing the sums of corresponding integers. If one list is longer than
   the other, simply ignore the extra elements. -}

sums :: [Int] -> [Int] -> [Int]
sums list1 list2 
    | (length list1 == 0) || (length list2 ==0) = []
    | otherwise =zipWith (+) list1 list2

sums_tests = "sums" ~:
             TestList [ "123,456" ~: sums [1,2,3] [4,5,6] ~?= [5,7,9]
                      , "1234,00" ~: sums [1,2,3,4] [0,0] ~?= [1,2]
                      , "even,odd" ~: sums [2,4,6] [1,3,5] ~?= [3,7,11]
                      , "power" ~: sums [2,4,8] [256,512,1024] ~?= [258, 516, 1032] ]

--------------------------------------------------------------------------------
-- Problem 15 (permutations)

-- This one is a challenge problem, so it's worth 0 points -- kudos only.
-- You *MAY* use recursion here.

{- A PERMUTATION of a list l is a list that has the same elements as l
   but is not necessarily in the same order.

   Write a function that, given a list l, calculates ALL of the
   permutations of l (and returns them as a list). For example,

       permutations [1,2,3]

   might yield

       [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]].

   (We say "might yield" here because we haven't specified the
   order of the permutations in the list returned by your function.
   For example, the result

       [[1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1], [1,2,3]]

   would also be correct.)

   Hint: Begin by writing a unit test or two, to make sure you understand the
   problem (even though you may need to rewrite them if your answer comes out
   in a different order, the exercise is useful). Also, you'll probably want
   to break the problem down into one or more sub-problems, each of which can
   be solved by recursion.

   Note: Do not remove or comment out this function stub, even if you
   choose not to attempt the challenge problem. Your file will not
   compile when you upload it for grading if 'permutations' is
   missing. -}

permutations :: [a] -> [[a]]
permutations = error "permutations: unimplemented"

{- Note that you will also have to think about how to TEST
   permutations, as there may be several correct solutions for each
   input. -}


permutations_tests
  = "permutations" ~:
    TestList [ "your test" ~: assertFailure "unwritten test"
             , "your test" ~: assertFailure "unwritten test" ]

--------------------------------------------------------------------------------
-- All the tests, for a quick overview.
-- You may remove the permutations tests if you don't want them here.

all_tests = TestList [ coins_tests
                     , maximumTableArea_tests
                     , moveRobot_tests
                     , streetDirection_tests
                     , exists_tests
                     , join_tests
                     , containsStr_tests
                     , dollsOf_tests
                     , merge_tests
                     , isSorted_tests
                     , mergeSorted_tests
                     , evensOnly_tests
                     , squares_tests
                     , wurble_tests
                     , sums_tests]
                   --  , permutations_tests ]

--------------------------------------------------------------------------------
{- Now that you've finished the assignment, please answer the following
   questions:

1. How did this assignment go for you?
   It is very difficult for me and also overwhelming for me. Since I need to catch up the class for about a week, I am not very good at the syntax. So I spent more than 10 hours on it.
2. What questions do you have?
   1.can we use recursion in a list comprehension?
   2. what is the different between (x:xs) and head & tail of a list
   3. why anonoymous function does not work on my program...
3. How long did this assignment take?
   10 hours.
-}

