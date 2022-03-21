--
-- Appendix A
-- Utilities module
--

module Utils where

-- The following definitions are used to make some synonyms for routines in the Gofer prelude to be more Miranda compatible

shownum n = show n

hd :: [a] -> a
hd = head -- in Gofer standard prelude

tl :: [a] -> [a]
tl = tail -- in Gofer standard prelude

zip2 :: [a] -> [b] -> [(a, b)]
zip2 = zip -- in Gofer standard prelude

-- can't do anything about # = length, since # not binary.

--
-- A.1 ヒープ型
--

-- A.1.1 Specification

-- A.1.2 Representation

type Heap a = (Int, [Int], [(Int, a)])
type Addr   = Int

hInitial :: Heap a
hInitial = (0, [1..], [])
--hInitial = (0, [1..100], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next : free), cts) n = ((size + 1, free, (next, n) : cts), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, cts) a n = (size, free, (a, n) : remove cts a)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) a = (size - 1, a : free, remove cts a)

hLookup :: Heap a -> Addr -> a
hLookup (size, free, cts) a = aLookup cts a (error ("can't find node " ++ showaddr a ++ " in heap"))

hAddresses :: Heap a -> [Addr]
hAddresses (size, free, cts) = [addr | (addr, node) <- cts]

hSize :: Heap a -> Int
hSize (size, free, cts) = size

hNull :: Addr
hNull = 0

hIsnull :: Addr -> Bool
hIsnull a = a == 0

showaddr :: Addr -> [Char]
showaddr a = "#" ++ shownum a -- Print # to identify addresses

remove :: [(Int, a)] -> Int -> [(Int, a)]
remove [] a = error ("Attempt to update or free nonexistent addresses #" ++ shownum a)
remove ((a', n) : cts) a | a == a' = cts
                         | a /= a' = (a', n) : remove cts a

--
-- A.2 連想リスト型
--

type ASSOC a b = [(a, b)]

aLookup :: (Eq a) => ASSOC a b -> a -> b -> b
aLookup []            k' def = def
aLookup ((k, v) : bs) k' def | k == k' = v
                             | k /= k' = aLookup bs k' def

aDomain :: ASSOC a b -> [a]
aDomain alist = [key | (key, val) <- alist]

aRange :: ASSOC a b -> [b]
aRange alist = [val | (key, val) <- alist]

aEmpty :: ASSOC a b
aEmpty = []

--
-- A.5 その他の便利な関数
--

first  (a, b) = a
second (a, b) = b

foldll :: (a -> b -> a) -> a -> [b] -> a
foldll = foldl -- in Gofer standard prelude.

mapAccuml :: (a -> b -> (a, c)) -- Function of accumulator and element
                                --   input list, returning new
                                --   accumulator and element of result list
             -> a               -- Initial accumulator
             -> [b]             -- Input list
             -> (a, [c])        -- Final accumulator and result list

mapAccuml f acc [] = (acc, [])
mapAccuml f acc (x : xs) = (acc2, x' : xs')
                           where
                             (acc1, x')  = f acc x
                             (acc2, xs') = mapAccuml f acc1 xs

{-
f a b = (a + b, a * 2)

*Main> mapAccuml f 0 [1, 2, 3]
(6,[0,2,6])
-}

sort [] = []
sort [x] = [x]
sort (x : xs) = [y | y <- xs, y < x] ++ x : [y | y <- xs, y >= x]

space n = take n (repeat ' ')
