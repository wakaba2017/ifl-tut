import Language
import Utils

-------------------------
-- 全体構造 (ここから) --
-------------------------
-- The funtion run is already defined in gofers standard.prelude
runProg :: [Char] -> [Char]
runProg = showResults . eval . compile . parse

-- :a language.lhs -- parser data types
-- :a util.lhs -- heap data type and other library funtions
-------------------------
-- 全体構造 (ここまで) --
-------------------------

-------------------------------
-- データ型の定義 (ここから) --
-------------------------------
type GmState
  = (GmCode,    -- Current instruction stream
     GmStack,   -- Current stack
     GmHeap, -- Heap of nodes
     GmGlobals, -- Global addresses in heap
     GmStats)   -- Statistics

data Instruction
   = Unwind
   | Pushglobal Name
   | Pushint Int
   | Push Int
   | Mkap
   | Update Int  -- Mark2で追加
   | Pop Int     -- Mark2で追加
-- | Slide Int   -- Mark2で削除
instance Eq Instruction
  where
    Unwind       == Unwind       = True
    Pushglobal a == Pushglobal b = a == b
    Pushint    a == Pushint    b = a == b
    Push       a == Push       b = a == b
    Mkap         == Mkap         = True
    Update     a == Update     b = a == b  -- Mark2で追加
--  Slide      a == Slide      b = a == b  -- Mark2で削除
    _            == _            = False

type GmCode = [Instruction]

getCode :: GmState -> GmCode
getCode (i, stak, heap, globals, stats) = i

putCode :: GmCode -> GmState -> GmState
putCode i' (i, stak, heap, globals, stats)
  = (i', stak, heap, globals, stats)

type GmStack = [Addr]

getStack :: GmState -> GmStack
getStack (i, stack, heap, globals, stats) = stack

putStack :: GmStack -> GmState -> GmState
putStack stack' (i, stack, heap, globals, stats)
  = (i, stack', heap, globals, stats)

type GmHeap = Heap Node

getHeap :: GmState -> GmHeap
getHeap (i, stack, heap, globals, stats) = heap
putHeap :: GmHeap -> GmState -> GmState
putHeap heap' (i, stack, heap, globals, stats)
  = (i, stack, heap', globals, stats)

data Node
  = NNum    Int         -- Numbers
  | NAp     Addr Addr   -- Applications
  | NGlobal Int  GmCode -- Globals
  | NInd    Addr        -- Indirections  -- Mark2で追加
instance Eq Node
  where
    NNum    a   == NNum    b   = a == b -- needed to check conditions
    NAp     a b == NAp     c d = False  -- not needed
    NGlobal a b == NGlobal c d = False  -- not needed
    NInd    a   == NInd    b   = False  -- not needed

type GmGlobals = ASSOC Name Addr

getGlobals :: GmState -> GmGlobals
getGlobals (i, stack, heap, globals, stats) = globals

-- Mark1の改良版で追加
putGlobals :: GmGlobals -> GmState -> GmState  -- 遷移規則 (3.14)
putGlobals globals' (i, stack, heap, globals, stats) = (i, stack, heap, globals', stats)

statInitial  :: GmStats
statIncSteps :: GmStats -> GmStats
statGetSteps :: GmStats -> Int

type GmStats = Int

statInitial    = 0
statIncSteps s = s + 1
statGetSteps s = s

getStats :: GmState -> GmStats
getStats (i, stack, heap, globals, stats) = stats

putStats :: GmStats -> GmState -> GmState
putStats stats' (i, stack, heap, globals, stats)
  = (i, stack, heap, globals, stats')
-------------------------------
-- データ型の定義 (ここまで) --
-------------------------------

-----------------------
-- 評価器 (ここから) --
-----------------------
eval :: GmState -> [GmState]
eval state = state: restStates
             where
               restStates | gmFinal state = []
                          | otherwise = eval nextState
               nextState = doAdmin (step state)

doAdmin :: GmState -> GmState
doAdmin s = putStats (statIncSteps (getStats s)) s

gmFinal :: GmState -> Bool
gmFinal s = case (getCode s) of
            [] -> True
            otherwise -> False  -- otherwiseではなく_らしい

step :: GmState -> GmState
step state = dispatch i (putCode is state)
             where (i : is) = getCode state

dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint    n) = pushint n
dispatch Mkap           = mkap
dispatch (Push       n) = push n
{-
dispatch (Slide      n) = slide n  -- Mark2で削除
-}
dispatch Unwind         = unwind
dispatch (Update     n) = update n  -- Mark2で追加
dispatch (Pop        n) = pop n     -- Mark2で追加

pushglobal :: Name -> GmState -> GmState  -- 遷移規則 (3.5)
pushglobal f state
  = putStack (a: getStack state) state
    where a = aLookup (getGlobals state) f (error ("Undeclared global " ++ f))

-- Mark1の改良版で追加
pushint_sub :: Int -> GmGlobals -> Bool  -- 遷移規則 (3.14)
pushint_sub n [] = False
pushint_sub n ((k, v) : bs) | k == (show n) = True
                            | k /= (show n) = pushint_sub n bs
pushint :: Int -> GmState -> GmState  -- 遷移規則 (3.13, 3.14)
pushint n state
  | pushint_sub n (getGlobals state) == True  -- 遷移規則 (3.13)
    = putStack (a : getStack state) state
      where a = aLookup (getGlobals state) (show n) (error ("Undeclared global " ++ (show n)))
pushint n state  -- このように書かないと、error: parse error on input ‘|’と言われてしまう模様。
  | otherwise  -- 遷移規則 (3.14)
    = putGlobals (((show n), a) : getGlobals state') state'
      where (heap', a) = hAlloc (getHeap state) (NNum n)
            state' = putHeap heap' (putStack (a: getStack state) state)

mkap :: GmState -> GmState  -- 遷移規則 (3.7)
mkap state
  = putHeap heap' (putStack (a:as') state)
    where (heap', a) = hAlloc (getHeap state) (NAp a1 a2)
          (a1:a2:as') = getStack state

push :: Int -> GmState -> GmState  -- 遷移規則 (3.8)
push n state
  = putStack (a:as) state
    where as = getStack state
          a = getArg (hLookup (getHeap state) (as !! (n+1)))

getArg :: Node -> Addr
getArg (NAp a1 a2) = a2

{-
slide :: Int -> GmState -> GmState  -- 遷移規則 (3.9)
slide n state
  = putStack (a: drop n as) state
    where (a:as) = getStack state
-}

unwind :: GmState -> GmState  -- 遷移規則 (3.10, 3.11, 3.12)
unwind state
  = newState (hLookup heap a)
    where
      (a:as) = getStack state
      heap = getHeap state
      newState (NNum n) = state  -- 遷移規則 (3.10)
      newState (NAp a1 a2) = putCode [Unwind] (putStack (a1:a:as) state)  -- 遷移規則 (3.11)
      newState (NGlobal n c)  -- 遷移規則 (3.12)
        | length as < n = error "Unwinding with too few arguments"
        | otherwise = putCode c state
      newState (NInd n) = putCode [Unwind] (putStack (n : as) state)  -- 遷移規則 (3.17)

-- Mark2で追加
update :: Int -> GmState -> GmState  -- 遷移規則 (3.15)
  {-
  stateからスタックを取り出す。
  取り出したスタックの先頭要素aを抜き出す。
  先頭要素を抜き出したスタックのn+1番目の値a_nを読み取る。
  stateからヒープを取り出す。
  取り出したヒープのアドレスa_nの内容を(NInd a)に置き換える。
  -}
update n state
  = putHeap newHeap (putStack as state)
    where (a : as) = getStack state
          an = as !! n
          (size, free, cts) = getHeap state
          newHeap = hUpdate (size, free, cts) an (NInd a)

-- Mark2で追加
pop :: Int -> GmState -> GmState  -- 遷移規則 (3.16)
pop n state
  = putStack (drop n (getStack state)) state
-----------------------
-- 評価器 (ここまで) --
-----------------------

---------------------------------------
-- プログラムのコンパイル (ここから) --
---------------------------------------
compile :: CoreProgram -> GmState
compile program
  = (initialCode, [], heap, globals, statInitial)
    where (heap, globals) = buildInitialHeap program

buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program
  = mapAccuml allocateSc hInitial compiled
    where compiled = map compileSc (preludeDefs ++ program) ++
                      compiledPrimitives
    --where
    --  compiled = map compileSc program

type GmCompiledSC = (Name, Int, GmCode)

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns)
  = (heap', (name, addr))
    where (heap', addr) = hAlloc heap (NGlobal nargs instns)

initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC  -- SCスキーム
compileSc (name, env, body)
  = (name, length env, compileR body (zip2 env [0..]))

compileR :: GmCompiler  -- Rスキーム
--compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]
compileR e env = compileC e env ++ [Update d, Pop d, Unwind]
                 where d = length env

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

type GmEnvironment = ASSOC Name Int

compileC :: GmCompiler  -- Cスキーム
compileC (EVar v) env
  | elem v (aDomain env) = [Push n]
  | otherwise = [Pushglobal v]
  where n = aLookup env v (error "Can't happen")
compileC (ENum n) env = [Pushint n]
compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v,m) <- env]

compiledPrimitives :: [GmCompiledSC]
compiledPrimitives = []
---------------------------------------
-- プログラムのコンパイル (ここまで) --
---------------------------------------

---------------------------
-- 結果の表示 (ここまで) --
---------------------------
showResults :: [GmState] -> [Char]
showResults states
  = iDisplay (iConcat [iStr "Supercombinator definitions", iNewline,
                       iInterleave iNewline (map (showSC s) (getGlobals s)),
                       iNewline, iNewline, iStr "State transitions", iNewline, iNewline,
                       iLayn (map showState states),
                       iNewline, iNewline,
                       showStats (last states)])
    where (s:ss) = states

showSC :: GmState -> (Name, Addr) -> Iseq
showSC s (name, addr)
  = iConcat [ iStr "Code for ", iStr name, iNewline,
              showInstructions code, iNewline, iNewline]
    where (NGlobal arity code) = (hLookup (getHeap s) addr)

showInstructions :: GmCode -> Iseq
showInstructions is
  = iConcat [iStr " Code:{",
             iIndent (iInterleave iNewline (map showInstruction is)),
             iStr "}", iNewline]

showInstruction :: Instruction -> Iseq
showInstruction Unwind         = iStr "Unwind"
showInstruction (Pushglobal f) = (iStr "Pushglobal ") `iAppend` (iStr f)
showInstruction (Push n)       = (iStr "Push ") `iAppend` (iNum n)
showInstruction (Pushint n)    = (iStr "Pushint ") `iAppend` (iNum n)
showInstruction Mkap           = iStr "Mkap"
{-
showInstruction (Slide n)      = (iStr "Slide ") `iAppend` (iNum n)  -- Mark2で削除
-}
showInstruction (Update n)     = (iStr "Update ") `iAppend` (iNum n)  -- Mark2で追加
showInstruction (Pop n)        = (iStr "Pop ") `iAppend` (iNum n)  -- Mark2で追加

showState :: GmState -> Iseq
showState s
  = iConcat [showStack s, iNewline,
             showInstructions (getCode s), iNewline]

showStack :: GmState -> Iseq
showStack s
  = iConcat [iStr " Stack:[",
             iIndent (iInterleave iNewline
                     (map (showStackItem s) (reverse (getStack s)))),
             iStr "]"]

showStackItem :: GmState -> Addr -> Iseq
showStackItem s a
  = iConcat [iStr (showaddr a), iStr ": ",
             showNode s a (hLookup (getHeap s) a)]

showNode :: GmState -> Addr -> Node -> Iseq
showNode s a (NNum n) = iNum n
showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
                             where v = head [n | (n,b) <- getGlobals s, a==b]
showNode s a (NAp a1 a2) = iConcat [iStr "Ap ", iStr (showaddr a1),
                                    iStr " ", iStr (showaddr a2)]
showNode s a (NInd n) = iConcat [iStr "Ind ", iStr(showaddr a)]  -- Mark2で追加

showStats :: GmState -> Iseq
showStats s
  = iConcat [ iStr "Steps taken = ", iNum (statGetSteps (getStats s))]
---------------------------
-- 結果の表示 (ここまで) --
---------------------------

---------------------------------
-- テストプログラム (ここから) --
---------------------------------
-- 超基本テスト --
b_1_1_1 = "main = I 3"
b_1_1_2 = "id = S K K ;" ++
          "main = id 3"
b_1_1_3 = "id = S K K ;" ++
          "main = twice twice twice id 3"
-- 演習 3.6 のテスト --
test_ex_3_6 = "main = K 2 2"
-- 更新のテスト --
b_1_2 = "main = twice (I I I) 3"
-- 無限リストのテスト(遅延評価のテスト) --
b_1_3 = "cons a b cc cn = cc a b ;" ++
        "nil cc cn = cn ;" ++
        "hd list = list K abort ;" ++
        "tl list = list K1 abort ;" ++
        "abort = abort ;" ++
        "infinite x = cons x (infinite x) ;" ++
        "main = hd (tl (infinite 4))"
---------------------------------
-- テストプログラム (ここまで) --
---------------------------------
