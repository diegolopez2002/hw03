
{-
Name: Diego Lopez
-}

{-

This is the first homework assignment for CMSC 433. It provides
practice with the basic built-in data structures of Haskell, including
lists, tuples and maybes, as well as recursion and pattern
matching. It also covers the basics of Haskell code style and
test-driven development. If you have not read the Basics module, and
completed the associated quiz, you should do that first.

This page is a "literate" Haskell program, meaning that explanation is
interspersed with actual Haskell code. To complete your assignment,
edit Main.hs and submit it through Gradescope.

This file starts by first declaring that we are creating a module
called Main and are using functions defined in the modules Prelude,
Test.HUnit, Data.List and Data.Char.

The Prelude line imports all except for the functions listed (which
you will write). The module Prelude is special in that it is always
imported by default, so the the point of this line is not to import
more functions, but rather to exclude a few functions. (Haskell does
not allow functions to be redefined in the same module.)

The Test.HUnit line imports all functions defined in that module. The
line Data.List imports all functions from that module, but makes them
available with qualified names, such as List.intersperse, etc.

-}

module Main where
import Prelude hiding (reverse, concat, zip, (++), takeWhile, all)
import Test.HUnit




{-

The main "entry point" for this assignment runs the tests for each
homework problem below. You should not edit this definition. Instead,
your goal is to modify the problems below so that all of the tests
pass. Note that the definitions in Haskell modules do not need to come
in any particular order; here, the main function uses the definitions
testStyle, testLists, etc, even though their definitions come much
later in the file.

-} 



main :: IO ()
main = do
  _ <- runTestTT testStyle
  _ <- runTestTT testLists
  _ <- runTestTT testHO
  _ <- runTestTT testFoldr
  _ <- runTestTT testTree
  return ()


{-

Now that we have the preliminaries out of the way, we can start the actual problems.

Recall that you can load this file into ghci with the command stack
ghci Main.hs. Or, you can build the executable first with stack build
and then run the test cases above with the command line stack exec --
hw01. (For each of these, make sure that you are in the hw01
subdirectory.)

-}

--------------------------------------------------------------------------------
-- Problem (Good Style)
-------------------------------------------------------------------------------- 

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , tarithmetic, treverse, tzip ]
 


    

{-

All of the following Haskell code does what it is supposed to do
(i.e. the tests pass), but it is difficult to read. Rewrite the
following expressions so that they exactly follow the style guide. Be
careful: the style guide includes quite a few rules, and we've broken
most of them in what follows! (You don't need to rewrite the test
following each part, but you do need to make sure that you don't break
the code as you refactor it!)

NOTE: Do not change the name of any of the top level declarations
below, even if you think that they aren't very good (they aren't). We
will be using automatic testing to ensure that you do not break
anything when you rewrite these functions. On the other hand, local
variables (such as function parameters and those bound by let and
where) can and should be renamed.

-}

-- Part One


--------------------------------------------------------------------------------
-- Problem (Good Style)
-------------------------------------------------------------------------------- 

-- Part One

abc :: Bool -> Bool -> Bool -> Bool
abc x y z =
  if x
    then if y
           then True
           else if x && z
                  then True
                  else False
    else False

tabc :: Test
tabc = "abc" ~: TestList
  [ abc True False True  ~?= True
  , abc True False False ~?= False
  , abc False True True  ~?= False
  ]

-- Part Two

arithmetic :: ((Int, Int), Int) -> ((Int,Int), Int) -> (Int, Int, Int)
arithmetic x1 x2 =
  let a = fst (fst x1)
      b = snd (fst x1)
      c = snd x1
      d = fst (fst x2)
      e = snd (fst x2)
      f = snd x2
  in (((b*f) - (c*e)), ((c*d) - (a*f)), ((a*e)-(b*d)))

tarithmetic :: Test
tarithmetic = "arithmetic" ~:
   TestList
     [ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3)
     , arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7)
     ]

-- Part Three

reverse' :: [a] -> [a]
reverse' l = reverseAux l []
  where
    reverseAux [] acc = acc
    reverseAux (x:xs) acc = reverseAux xs (x : acc)

treverse :: Test
treverse = "reverse" ~: TestList
    [ reverse' [3,2,1] ~?= ([1,2,3] :: [Int])
    , reverse' [1]     ~?= ([1] :: [Int])
    , reverse' []      ~?= ([] :: [Int])
    ]

-- Part Four

zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys = g 0 xs ys
  where
    g n xs' ys' =
      if n == length xs' || n == length ys'
        then []
        else (xs' !! n, ys' !! n) : g (n + 1) xs' ys'

tzip :: Test
tzip = "zip" ~: TestList
    [ zip' [1,2,3] ['a','b','c'] ~?= zip' [1,2,3] ['a','b','c']
    , zip' [1,2,3] []            ~?= zip' [1,2,3] ([] :: [Int])
    , zip' [] ['a','b','c']      ~?= zip' ([] :: [Int]) ['a','b','c']
    , zip' [] []                 ~?= zip' ([] :: [Int]) ([] :: [Char])
    , zip' ['a','b','c'] [1,2,3] ~?= zip' ['a','b','c'] [1,2,3]
    ]

--------------------------------------------------------------------------------
-- Problem (List library chops)
-------------------------------------------------------------------------------- 

{-

Define, debug and test the following functions. Some of these
functions are part of the Haskell standard prelude or standard
libraries like Data.List. Their solutions are readily available
online. You should not google for this code: instead, implement them
yourself.

For each part of this problem, you should replace the testcase for
that part based on the description in the comments. Make sure to test
with multiple inputs using TestList. We will be grading your test
cases as well as the correctness and style of your solutions! HINT:
your testing code should include any tests that we give you in the the
comments!

Do not use any list library functions in this problem. This includes
any function from the Prelude or from Data.List thats take arguments
or returns a result with a list type. Note that (:) and [] are data
constructors for the list type, not functions, so you are free to use
them. Please also avoid list comprehension syntax, as it actually
de-sugars into library functions! This also includes foldr/map/etc.
You'll get a chance to use those further below! 

-}

testLists :: Test
testLists = "testLists" ~: TestList
  [tminimumMaybe, tstartsWith, tendsWith, ttranspose, tcountSub]

-- Part One

-- | The 'minimumMaybe` function computes the mininum value of a
-- nonempty list. If the list is empty, it returns Nothing.
--
-- >>> minumumMaybe []
-- Nothing
-- >>> minumumMaybe [2,1,3]
-- Just 1 


minimumMaybe :: [Int] -> Maybe Int
minimumMaybe [] = Nothing
minimumMaybe (x:xs) = Just $ foldl min x xs

tminimumMaybe :: Test
tminimumMaybe = "minimumMaybe" ~: TestList
  [ minimumMaybe [] ~?= Nothing
  , minimumMaybe [2, 1, 3] ~?= Just 1
  , minimumMaybe [5, 7, 2, 8, 4, 6] ~?= Just 2
  , minimumMaybe [99] ~?= Just 99
  , minimumMaybe [0, -1, -2] ~?= Just (-2)
  ]

-- Part Two

-- | The 'startsWith' function takes two strings and returns 'True'
-- iff the first is a prefix of the second.
--
-- >>> "Hello" `startsWith` "Hello World!"
-- True
--
-- >>> "Hello" `startsWith` "Wello Horld!"
-- False

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

tstartsWith :: Test
tstartsWith = "startsWith" ~: TestList
  [ "Hello" `startsWith` "Hello World!" ~?= True
  , "Hello" `startsWith` "Wello Horld!" ~?= False
  , "Goodbye" `startsWith` "Goodbye" ~?= True
  , "Good" `startsWith` "Goodbye" ~?= True
  , "" `startsWith` "Anything" ~?= True
  , "Not" `startsWith` "" ~?= False
  ]

-- Part Three

-- | The 'endsWith' function takes two lists and returns 'True' iff
-- the first list is a suffix of the second. The second list must be
-- finite.
--
-- >>> "ld!" `endsWith` "Hello World!"
-- True
--
-- >>> "World" `endsWith` "Hello World!"
-- False


endsWith :: String -> String -> Bool
endsWith [] _ = True
endsWith _ [] = False
endsWith xs ys = startsWith (drop (length ys - length xs) ys) xs

tendsWith :: Test
tendsWith = "endsWith" ~: TestList
  [ "ld!" `endsWith` "Hello World!"  ~?= True
  , "World" `endsWith` "Hello World!" ~?= False
  , "" `endsWith` "Some String"      ~?= True
  , "String" `endsWith` ""           ~?= False
  ]


-- Part Four

-- | The 'transpose' function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is *not* the same behavior as the library version
-- of transpose (i.e. the version of transpose from Data.List).
--
-- >>> transpose [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
-- >>> transpose [] 
-- []
-- >>> transpose [[]] 
-- []
-- >>> transpose [[3,4,5]]
-- [[3],[4],[5]]
-- >>> transpose [[1,2],[3,4,5]]
-- [[1,3],[2,4]]
-- (WARNING: this one is tricky!)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose xss = map head xss : transpose (map tail xss)

ttranspose :: Test
ttranspose = "transpose" ~: (assertFailure "testcase for transpose" :: Assertion)

-- Part Five

-- | The 'countSub' function returns the number of (potentially overlapping)
-- occurrences of a substring sub found in a string.
--
-- >>> countSub "aa" "aaa"
-- 2
-- >>> countSub "" "aaac"
-- 5

countSub :: String -> String -> Int
countSub _ [] = 0
countSub sub str
  | sub `startsWith` str = 1 + countSub sub (drop (length sub) str)
  | otherwise = countSub sub (tail str)

tcountSub :: Test
tcountSub = "countSub" ~: TestList
  [ countSub "aa" "aaa" ~?= 2
  , countSub "" "aaac" ~?= 5
  , countSub "cat" "I have a cat named Catalyst." ~?= 1
  , countSub "an" "banana" ~?= 2
  , countSub "ab" "abababab" ~?= 4
  , countSub "z" "The quick brown fox jumps over the lazy dog." ~?= 0
  ]

--------------------------------------------------------------------------------
-- Problem (Higher-order list operations)
-------------------------------------------------------------------------------- 

{-

Complete these operations which take higher-order functions as
arguments. (For extra practice, you may try to define these operations
using foldr, but that is not required for this problem.) Otherwise,
you may not use any list library functions for this problem.

-}

testHO :: Test
testHO = TestList [ttakeWhile, tfind, tall, tmap2, tmapMaybe]

-- | `takeWhile`, applied to a predicate `p` and a list `xs`,
-- returns the longest prefix (possibly empty) of `xs` of elements
-- that satisfy `p`.
--
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: (assertFailure "testcase for takeWhile" :: Assertion)

-- | `find pred lst` returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a `Maybe`.
--
-- >>> find odd [0,2,3,4]
-- Just 3

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs)
  | p x       = Just x
  | otherwise = find p xs

tfind :: Test
tfind = "find" ~: (assertFailure "testcase for find" :: Assertion)


-- | `all pred lst` returns `False` if any element of `lst`
-- fails to satisfy `pred` and `True` otherwise.
--
-- >>> all odd [1,2,3]
-- False

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs)
  | p x       = all p xs
  | otherwise = False

tall :: Test
tall = "all" ~: (assertFailure "testcase for all" :: Assertion)
-- | `map2 f xs ys` returns the list obtained by applying `f` to
-- to each pair of corresponding elements of `xs` and `ys`. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- >>> map2 (+) [1,2] [3,4]
-- [4,6]
--
-- NOTE: `map2` is called `zipWith` in the Prelude

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys


tmap2 :: Test
tmap2 = "map2" ~: (assertFailure "testcase for map2" :: Assertion)

-- | Apply a partial function to all the elements of the list,
-- keeping only valid outputs.
--
-- >>> mapMaybe root [0.0, -1.0, 4.0]
-- [0.0,2.0]
--
-- (where `root` is defined below.)
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) =
  case f x of
    Just y  -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs

tmapMaybe :: Test
tmapMaybe = "mapMaybe" ~: (assertFailure "testcase for mapMaybe" :: Assertion)



root :: Double -> Maybe Double
root d = if d < 0.0 then Nothing else Just $ sqrt d

--------------------------------------------------------------------------------
-- Problem (map and foldr practice for lists)
-------------------------------------------------------------------------------- 

{- 

Go back to the following functions that you defined above and redefine
them using one of the higher-order functions map, foldr or para (see
below). These are the only list library functions that you should use
on this problem. If you need any additional helper functions you must
define them yourself (and any helper functions should also use map,
foldr or para instead of explicit recursion).

-}

testFoldr :: Test
testFoldr = TestList [ tconcat',  tstartsWith', tendsWith', ttails, tcountSub' ]

-- | The concatenation of all of the elements of a list of lists
--
-- >>> concat [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,4,5,6,7,8,9]
--

{-

NOTE: remember you cannot use any list functions from the Prelude or
Data.List for this problem, even for use as a helper
function. Instead, define it yourself.

-}

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = concatHelper x (concat' xs)
  where
    concatHelper :: [a] -> [a] -> [a]
    concatHelper [] ys = ys
    concatHelper (y:ys) zs = y : concatHelper ys zs

tconcat' :: Test
tconcat' = "concat" ~: (assertFailure "testcase for concat" :: Assertion)

-- | The 'startsWith' function takes two strings and returns 'True'
-- iff the first is a prefix of the second.
--
-- >>> "Hello" `startsWith` "Hello World!"
-- True
--
-- >>> "Hello" `startsWith` "Wello Horld!"
-- False

-- NOTE: use foldr for this one, but it is tricky! (Hint: the value returned by foldr can itself be a function.)

startsWith' :: String -> String -> Bool
startsWith' [] _ = True
startsWith' _ [] = False
startsWith' (x:xs) (y:ys) = x == y && startsWith' xs ys

tstartsWith' :: Test
tstartsWith' = "startsWith'" ~: TestList
  [ "Hello" `startsWith'` "Hello World!"  ~?= True
  , "Hello" `startsWith'` "Wello Horld!"  ~?= False
  , ""      `startsWith'` "Anything"      ~?= True
  , "No"    `startsWith'` ""              ~?= False
  ]

-- INTERLUDE: para

{-

Now consider a variant of foldr called para. In the case of cons,
foldr provides access to the head of the list and the result of the
fold over the tail of the list. The para function should do the same,
but should also provide access to the tail of the list (before it has
been processed).

-}

-- | foldr variant that provides access to each tail of the list
para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para _ b [] = b
para f b (x:xs) = f x xs (para f b xs)

-- For example, consider the tails function.

-- | The 'tails' function calculates all suffixes of a give list and returns them
-- in decreasing order of length. For example:
--
-- >>> tails "abc"
-- ["abc", "bc", "c", ""],
--

tails' :: [a] -> [[a]]
tails' = para (\x xs rest -> (x:xs) : rest) [[]]

ttails :: Test
ttails = "tails'" ~: TestList
  [ Main.tails' "abc" ~?= ["abc", "bc", "c", ""]
  , Main.tails' ""    ~?= [""]
  , Main.tails' "a"   ~?= ["a",""]
  ]

{- 

It is a natural fit to implement tails using para. See if you can
redefine the function above so that the test cases still pass.

-}


-- | The 'endsWith' function takes two lists and returns 'True' iff
-- the first list is a suffix of the second. The second list must be
-- finite.
--
-- >>> "ld!" `endsWith` "Hello World!"
-- True
--
-- >>> "World" `endsWith` "Hello World!"
-- False

-- NOTE: use para for this one!

endsWith' :: String -> String -> Bool
endsWith' xs ys = endsWithHelper xs ys (length xs) (length ys)
  where
    endsWithHelper _ [] _ _ = True
    endsWithHelper [] _ _ _ = False
    endsWithHelper (x:xs') (y:ys') lenX lenY
      | lenX > lenY = False  -- If xs is longer than ys, it can't be a suffix
      | x /= y = False       -- If the characters don't match, not a suffix
      | otherwise = endsWithHelper xs' ys' (lenX - 1) (lenY - 1)


tendsWith' :: Test
tendsWith' = "endsWith'" ~: TestList
  [ "ld!" `endsWith'` "Hello World!"  ~?= True
  , "World" `endsWith'` "Hello World!" ~?= False
  , "" `endsWith'` "Some String"      ~?= True
  , "String" `endsWith'` ""           ~?= False
  ]



-- | The 'countSub' function returns the number of (potentially overlapping)
-- occurrences of a substring sub found in a string.
--
-- >>> countSub "aa" "aaa"
-- 2
-- >>> countSub "" "aaac"
-- 5

-- (You may use the para and startsWith' functions in countSub'.)

countSub' :: String -> String -> Int
countSub' sub = foldr (\xs acc -> (if startsWith' sub xs then 1 else 0) + acc) 0 . tails'

tcountSub' :: Test
tcountSub' = "countSub'" ~: TestList
  [ countSub' "aa" "aaa"   ~?= 2
  , countSub' "" "aaac"    ~?= 5
  , countSub' "aaa" "a"   ~?= 0
  , countSub' "ab" "ababab" ~?= 3
  ]

--------------------------------------------------------------------------------
-- Problem (Tree Processing)
-------------------------------------------------------------------------------- 

testTree :: Test
testTree = TestList [
    tappendTree, tinvertTree, ttakeWhileTree, tallTree, tmap2Tree ]

{-

This next problem involves writing some library functions for tree
data structures. The following datatype defines a binary tree, storing
data at each internal node.

-}

-- | a basic tree data structure
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

{- This is the definition of a mappping operation for this data structure: -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Branch x t1 t2) = Branch (f x) (mapTree f t1) (mapTree f t2)

{- And here is a fold-like operations for trees: -}

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ e Empty = e
foldTree f e (Branch a n1 n2) = f a (foldTree f e n1) (foldTree f e n2)

{- Use one of these functions to define the following operations over trees. -}

-- The `appendTree` function takes two trees and replaces all of the `Empty`
-- constructors in the first with the second tree.  For example:
--
-- >>> appendTree (Branch 'a' Empty Empty) (Branch 'b' Empty Empty)
-- Branch 'a' (Branch 'b' Empty Empty) (Branch 'b' Empty Empty)
--
-- and
-- 
-- >>> appendTree Empty (Branch 'a' Empty Empty)
-- Branch 'a' Empty Empty

appendTree :: Tree a -> Tree a -> Tree a
appendTree Empty t = t
appendTree t Empty = t
appendTree (Branch x l r) t2 = Branch x (appendTree l t2) (appendTree r t2)

tappendTree :: Test
tappendTree = "appendTree" ~: TestList
  [ appendTree (Branch 'a' Empty Empty) (Branch 'b' Empty Empty)  ~?= Branch 'a' (Branch 'b' Empty Empty) (Branch 'b' Empty Empty)
  , appendTree Empty (Branch 'a' Empty Empty)                     ~?= Branch 'a' Empty Empty
  , appendTree (Branch 'a' Empty Empty) Empty                     ~?= Branch 'a' Empty Empty
  ]

-- The `invertTree` function takes a tree of pairs and returns a new tree
-- with each pair reversed.  For example:
--
-- >>> invertTree (Branch ("a",True) Empty Empty)
-- Branch (True,"a") Empty Empty

invertTree :: Tree (a, b) -> Tree (b, a)
invertTree Empty = Empty
invertTree (Branch (x, y) left right) = Branch (y, x) (invertTree left) (invertTree right)

tinvertTree :: Test
tinvertTree = "invertTree" ~: (assertFailure "testcase for invertTree" :: Assertion)


-- `takeWhileTree`, applied to a predicate `p` and a tree `t`,
-- returns the largest prefix tree of `t` (possibly empty)
-- where all elements satisfy `p`.
-- For example, given the following tree

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty)

-- >>> takeWhileTree (< 3) tree1
-- Branch 1 (Branch 2 Empty Empty) Empty
--
-- >>> takeWhileTree (< 0) tree1
-- Empty

takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree _ Empty = Empty
takeWhileTree p (Branch x l r)
  | p x = Branch x (takeWhileTree p l) (takeWhileTree p r)
  | otherwise = Empty

ttakeWhileTree :: Test
ttakeWhileTree = "takeWhileTree" ~: (assertFailure "testcase for takeWhileTree" :: Assertion)

-- `allTree pred tree` returns `False` if any element of `tree`
-- fails to satisfy `pred` and `True` otherwise. For example:
--
-- >>> allTree odd tree1
-- False
--

allTree :: (a -> Bool) -> Tree a -> Bool
allTree _ Empty = True
allTree p (Branch x l r) = p x && allTree p l && allTree p r

tallTree :: Test
tallTree = "allTree" ~: (assertFailure "testcase for allTree" :: Assertion)

-- WARNING: This one is a bit tricky!  (Hint: use `foldTree` and remember
--  that the value returned by `foldTree` can itself be a function. If you are
-- stuck on this problem, go back to `startsWith` and make sure you understand
-- how that function can work with a single fold.)

-- `map2Tree f xs ys` returns the tree obtained by applying `f` to
-- to each pair of corresponding elements of `xs` and `ys`. If
-- one branch is longer than the other, then the extra elements
-- are ignored.
-- for example:
-- 
-- >>> map2Tree (+) (Branch 1 Empty (Branch 2 Empty Empty)) (Branch 3 Empty Empty)
-- Branch 4 Empty Empty

map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2Tree _ Empty _ = Empty
map2Tree _ _ Empty = Empty
map2Tree f (Branch x1 l1 r1) (Branch x2 l2 r2) =
  Branch (f x1 x2) (map2Tree f l1 l2) (map2Tree f r1 r2)

tmap2Tree :: Test
tmap2Tree = "map2Tree" ~: (assertFailure "testcase for map2Tree" :: Assertion)


