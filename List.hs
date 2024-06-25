-- 1.	Define a function doubleElts, which takes a list of integers, and doubles them. Thus, doubleElts [1,2,3] would return [2,4,6].

doubleElts :: [Int] -> [Int]
doubleElts [] = []                          -- Base case: empty list returns empty list
doubleElts (x:xs) = (2 * x) : doubleElts xs -- Double the head element and recurse on the tail


-- 2.	Write a function sumList, which given a list of integers, returns the sum of the elements.

sumList :: [Int] -> Int
sumList [] = 0                  -- Base case: empty list returns 0
sumList (x:xs) = x + sumList xs -- Add the head element to the sum of the tail

-- 3.	Define lastElt, which returns the last element of a list.

lastElt :: [a] -> a
lastElt [x] = x             -- Base case: list with one element returns that element
lastElt (x:xs) = lastElt xs -- Recurse on the tail

-- 4.	Define the functions, takeN and dropN which, given an integer n and list xs, return a list containing the first n elements of xs and a list containing all the elements of xs beyond the first n, respectively. Therefore, takeN 2 [1,2,3] would return [1,2], while dropN 2 [1,2,3] would return [3]. If n is larger than the length of the given list, takeN should return the whole list, while dropN should return the empty list.

takeN :: Int -> [a] -> [a]
takeN _ [] = []                        -- Base case: takeN from empty list returns empty list
takeN n (x:xs)                         -- Pattern matching on the list
  | n <= 0    = []                     -- Base case: takeN 0 elements returns empty list
  | otherwise = x : takeN (n - 1) xs   -- Add the head element and recurse on the tail 

dropN :: Int -> [a] -> [a]
dropN _ [] = []                        -- Base case: dropN from empty list returns empty list
dropN n xs@(x:xs')                     -- xs@ is a pattern that binds the whole list to xs and the head element to x
  | n <= 0    = xs                     -- Base case: dropN 0 elements returns the whole list
  | otherwise = dropN (n - 1) xs'


-- 5.	Define the function join, which is given two lists and pairs up the elements. For example, join [1,2,3] [’a’,’b’,’c’] would return [(1,’a’), (2,’b’), (3,’c’)]. If one of the lists is shorter, ignore the extra trailing part. join [1,2] [’a’] would thus return [(1,’a’)].

join :: [a] -> [b] -> [(a, b)]
join [] _ = []                            -- Base case: If the first list is empty, return an empty list
join _ [] = []                            -- Base case: If the second list is empty, return an empty list
join (x:xs) (y:ys) = (x, y) : join xs ys  -- Pair up the first elements and recursively call join on the remaining lists

-- 6.	Write a function oddElts, which given a list, returns the odd-positioned (first, third, fifth . . . ) elements of the list.

oddElts :: [a] -> [a]
oddElts [] = []                          -- Base case: empty list returns empty list
oddElts [x] = [x]                        -- Base case: list with one element returns that element
oddElts (x:_:xs) = x : oddElts xs        -- Take the head element and recurse on the tail

-- 7.	The aim of this question is to construct code to manipulate and format text. Strings in Haskell may include characters such as tabs and newlines. A tab character is written as \t, while a newline is written as \n. At the ghci prompt, a string returned may have such control characters appearing. If you want to see the string with these characters interpreted to cause a newline or tab, you may use the function putStr. For example, if someone wrote a function hello which returns a string which includes newlines, you can see its result using putStr:

--   Prelude> hello 3 "a\naa\naaa\n"

--   Prelude> putStr (hello 3) 
--   a
--   aa 
--   aaa

-- (a)	Write a function skipWhiteSpace, which given a string, returns the same string, but with initial white space skipped. Haskell has a useful function called isSpace, which returns whether or not a given character is white space. To use this function you have to import the Haskell character library, by starting the source code with: import Char

import Char

skipWhiteSpace :: String -> String
skipWhiteSpace [] = []                          -- Base case: empty string returns empty string
skipWhiteSpace (x:xs)                           -- Pattern matching on the string
  | isSpace x = skipWhiteSpace xs               -- If the head element is white space, skip it
  | otherwise = x : xs                          -- Otherwise, return the string as is

-- (b)	Hence or otherwise, write a function getWord, which given a string, returns two strings - the first word in the given string, and the rest of the given string. For example, getWord " Hello world, goodbye!" returns ("Hello", " world, goodbye!").

getWord :: String -> (String, String)
getWord [] = ([], [])                            -- Base case: empty string returns a tuple of two empty strings
getWord (x:xs)                                   -- Pattern matching on the string
  | isSpace x = ([], xs)                         -- If the head element is white space, return an empty string and the rest of the string
  | otherwise = let (word, rest) = getWord xs    -- Otherwise, recurse on the tail
                in (x : word, rest)              -- Add the head element to the word

-- (c)	Using getWord, write a function intoWords, which given a string, returns a list of strings — each of which was a word (separated by white space) in the original input.

intoWords :: String -> [String]
intoWords [] = []                                                  -- Base case: empty string returns empty list
intoWords xs = let (word, rest) = getWord (skipWhiteSpace xs) in   -- Skip white space and get the first word
    if null word then []                                           -- If the word is empty, return an empty list
    else word : intoWords rest                                     -- Otherwise, add the word to the list and recurse on the rest of the string

-- (d)	Define a function getLine, which given a list of strings, and an integer width representing the maximum line length, returns a string containing the first words (separated by a space, and whose length does not exceed width, and such that adding another word would make the line longer than width), and a list of strings (the remaining words in the text). For example, getLine ["The","sky","is","not","blue"] 12 would return ("The sky is",["not","blue"]).
getLine :: [String] -> Int -> (String, [String])
getLine [] _ = ([], [])                     -- Base case: empty list returns an empty string and an empty list
getLine (x:xs) width                           
    | length x <= width = let (line, remaining) = getLine xs (width - length x - 1) in (x ++ " " ++ line, remaining) -- If the length of the first word is less than the width, add it to the line and recurse on the remaining words
    | otherwise = ("", x:xs)                -- Otherwise, return an empty string and the list of words

-- (e)	Hence or otherwise, define a function leftJustified, which given an integer width and a string s, splits the string into a list of lines, each of which is no longer than width, and without splitting a word in half.

leftJustified :: Int -> String -> [String]
leftJustified _ [] = []                                     -- Base case: empty string returns empty list
leftJustified width text =               
    let (line, remaining) = getLine (intoWords text) width  -- Get the first line and the remaining words
    in line : leftJustified width (unwords remaining)       -- Add the line to the list and recurse on the remaining words

-- ((f)	Define a function justified, which given an integer width and a string s, splits the text into lines as in the case of leftJustified but adds additional spaces between words so as to ensure that all the lines are exactly width characters wide.


