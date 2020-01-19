-- Problem 1
myLast :: [a] -> a
myLast [] = error "List is empty!"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "List is empty!"
myButLast [x] = error "List has only one element!"
myButLast (x:xs)
    | (length xs == 1) = x
    | otherwise = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt list index
    | (length list < index) = error "Index outside of list!"
    | (index == 1) = head list
    | otherwise = elementAt (tail list) (index - 1)

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = reverse xs ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = l == (myReverse l)

-- Problem 7
-- First, define a NestedList type, since Haskell lists are homogenous
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8
