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
-- I provide two test cases. You must provide two more.
-- See https://hackage.haskell.org/package/HUnit-1.5.0.0/docs/Test-HUnit-Base.html
-- for the functions to create tests. (For now, treat Assertion and Test as
-- interchangeable.)

coins_tests = "coins" ~:
              TestList [ "coins 7"  ~: coins 7  ~?= 3
                       , "coins 99" ~: coins 99 ~?= 14
                       , "your test" ~: assertFailure "unwritten test"
                       , "your test" ~: assertFailure "unwritten test" ]
