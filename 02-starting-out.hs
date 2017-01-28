{-
 -Once you've installed Haskell from http://www.haskell.org/platform/, load the interpreter with the command ghci.
 -
 -You can load (and reload) this file in the interpreter with the command: ":l 2-starting-out.hs"
 -
 -The first function has been completed as an example. All the other functions are undefined.
 -They can be implemented in one line using the material covered in http://learnyouahaskell.com/starting-out
 -
 -All indices are zero based.
 -}

-- Find the penultimate element in list l
penultimate :: [a] -> a
penultimate l = last (init l)

-- Find the element at index k in list l
-- For example: "findK 2 [0,0,1,0,0,0]" returns 1
findK :: (Integral k) => k -> [a] -> a
findK _ [] = error "Index out of range"
findK k (x:xs)
    | k < 0 = error "Negativ index"
    | k == 0 = x
    | otherwise = findK (k - 1) xs

-- Determine if list l is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = l == reverse l

{-
 - Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
 - Hint: The "concat [l]" function flattens a list of lists into a single list. 
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}
duplicate :: [a] -> [a]
duplicate [] = []
duplicate [x] = [x, x]
duplicate (x:xs) = x:x:duplicate xs

{-
 - Imitate the functinality of zip
 - The function "min x y" returns the lower of values x and y
 - For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
 -}
ziplike :: [a] -> [b] -> [(a, b)]
ziplike [] _ = []
ziplike _ [] = []
ziplike (x:xs) (y:ys) = (x, y):ziplike xs ys

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 3 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2])
splitAtIndex :: (Integral k) => k -> [a] -> ([a], [a])
splitAtIndex _ [] = error "Index out of range"
splitAtIndex k all@(x:xs)
    | k < 0 = error "Negativ index"
    | k == 0 = ([], all)
    | otherwise = ((x:h), t)
        where (h, t) = splitAtIndex (k - 1) xs

-- Drop the element at index k in list l
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]
dropK :: (Integral k) => k -> [a] -> [a]
dropK _ [] = error "Index out of range"
dropK k all@(x:xs)
    | k < 0 = error "Negativ index"
    | k == 0 = xs
    | otherwise = x:dropK (k - 1) xs

-- Extract elements between ith and kth element in list l. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]
slice :: (Integral i) => i -> i -> [a] -> [a]
slice i k l
    | i < 0 = error "Negativ start index"
    | i > k = error "Start index must be smaller than end "
    | k == 0 = []
    | i == 0 = x:slice 0 (k - 1) xs
    | otherwise = slice (i - 1) (k - 1) xs
    where (x:xs) = l

-- Insert element x in list l at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]
insertElem :: (Integral k) => a -> k -> [a] -> [a]
insertElem x k l
    | k < 0 = error "Negativ index"
    | k == 0 = x:l
    | length l == 0 = error "Index out of range"
    | otherwise = y:insertElem x (k - 1) ys
    where (y:ys) = l

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]
rotate :: (Integral n) => n -> [a] -> [a]
rotate 0 l = l
rotate _ [] = []
rotate n l
    | n < 0 = rotate (n + fromIntegral (length l)) l
    | otherwise = rotate (n - 1)(las:ini)
    where ini = init l
          las = last l
