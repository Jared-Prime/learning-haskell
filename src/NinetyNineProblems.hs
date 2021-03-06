module NinetyNineProblems where

-- 1. Find the last element of a list
myLast :: [a] -> a
myLast []     = error "Nothing in list!"
myLast [x]    = x
myLast (_:xs) = myLast xs

-- 2. Find the last, but one, element of a list
lastButOne :: [a] -> a
lastButOne [x,_] = x              -- only two elements left
lastButOne (_:xs) = lastButOne xs -- recurse over tail

-- 3. Find the k'th element of a list (one indexed)
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x                   -- only asked for the first
elementAt (_:xs) k = elementAt xs (k-1) -- recurse over tail

-- 4. Find the length of a list
myLength :: [a] -> Int
myLength list = acc_Length list 0
  where
    acc_Length [] n = n
    acc_Length (_:xs) n = acc_Length xs (n + 1)

-- 5. Reverse a list
myReverse :: [a] -> [a]
-- @TODO provide explanation based off standard definition(s) of fold
--       https://wiki.haskell.org/Fold
myReverse = foldl (flip (:)) []

-- 6. Detect a palindrome
-- type `a` must implement the `Eq` class
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == (myReverse list)

-- 7. Flatten a nested list
data NestableList a = FlattenedList a | NestedList [NestableList a] deriving Show

myFlatten :: NestableList a -> [a]
myFlatten (FlattenedList x) = [x]
myFlatten (NestedList x) = concatMap myFlatten x

-- 8. Eliminate consecutive duplicates
uniq :: (Eq a) => [a] -> [a]
uniq list = acc_uniq list []
  where
    acc_uniq [] acc = acc
    acc_uniq [x] acc = acc ++ [x]
    acc_uniq (x:xs) acc
      | x == (head xs) = acc_uniq xs acc
      | otherwise      = acc_uniq xs (acc ++ [x])


-- 9. Pack consecutive duplicates into sublists
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)

-- 10. Perform run-length encoding of a list
data ListEncoding a = Encode(Int, a) deriving Show

encodeList :: (Eq a) => [a] -> [ListEncoding a]
encodeList = map encode . pack
  where
    encode list = Encode(myLength list, (head list))
