--01. Find the last element of a list
myLast [x] = x
myLast (_:xs) = myLast xs
myLast _ = error "an empty list do not have last value"

--02. Find the last but one element of a list.
myButLast = myLast . init

myButLast' xs = myLast (init xs)

myButLast'' [x,y] = x
myButLast'' (_:xs) = myButLast xs
myButLast'' _ = error "something went wrong"

--03. Find the K'th element of a list. The first element in the list is number 1.
elementAt lst index = lst !! (index-1)

elementAt' (x:xs) 1 = x
elementAt' (_:xs) i = elementAt' xs (i-1)
elementAt' _ _ = error "something went wrong"


--04. Find the number of elements of a list.
myLength = length

myLength' [] = 0
myLength' (_:xs) = 1 + myLength xs

--05. Reverse a list.
myReverse = reverse

myReverse' [] = []
myReverse' (x:xs) = myReverse' xs ++ [x]

--06. Find out whether a list is a palindrome.
--    A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome xs = xs == (myReverse xs)

--07. Flatten a nested list structure.



--08. Eliminate consecutive duplicates of list elements.
compress [] = []
compress [x] = [x]
compress (x:y:ys)
    | x == y    = compress (y:ys)
    | otherwise = [x] ++ compress (y:ys)


fib 1 = 1
fib 0 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' = 1:1:(zipWith (+) fib' (tail fib'))


--09. Pack consecutive duplicates of list elements into sublists.
--    If a list contains repeated elements they should be placed
--    in separate sublists.



--10. Run-length encoding of a list. Use the result of problem P09
--    to implement the so-called run-length encoding data compression
--    method. Consecutive duplicates of elements are encoded as
--    lists (N E) where N is the number of duplicates of the element E.
