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
     GmDump,    -- current Dump  -- Mark4で追加
     GmHeap,    -- Heap of nodes
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
   | Slide Int   -- Mark3で復活
   | Alloc Int   -- Mark3で追加
   | Eval        -- Mark4で追加
   | Add | Sub | Mul | Div | Neg  -- Mark4で追加
   | Eq | Ne | Lt | Le | Gt | Ge  -- Mark4で追加
   | Cond GmCode GmCode  -- Mark4で追加
  deriving Show  -- テキストにはないけれど追加
instance Eq Instruction
  where
    Unwind       == Unwind       = True
    Pushglobal a == Pushglobal b = a == b
    Pushint    a == Pushint    b = a == b
    Push       a == Push       b = a == b
    Mkap         == Mkap         = True
    Update     a == Update     b = a == b  -- Mark2で追加
    Pop        a == Pop        b = a == b  -- SGM Mark2で追加
    Slide      a == Slide      b = a == b  -- Mark3で復活
    Alloc      a == Alloc      b = a == b  -- Mark3で追加
    Eval         == Eval         = True    -- Mark4で追加
    Add          == Add          = True    -- Mark4で追加
    Sub          == Sub          = True    -- Mark4で追加
    Mul          == Mul          = True    -- Mark4で追加
    Div          == Div          = True    -- Mark4で追加
    Neg          == Neg          = True    -- Mark4で追加
    Eq           == Eq           = True    -- Mark4で追加
    Ne           == Ne           = True    -- Mark4で追加
    Lt           == Lt           = True    -- Mark4で追加
    Le           == Le           = True    -- Mark4で追加
    Gt           == Gt           = True    -- Mark4で追加
    Ge           == Ge           = True    -- Mark4で追加
    Cond a b     == Cond c d     = (a == c) && (b == d)  -- Mark4で追加
    _            == _            = False

type GmCode = [Instruction]

getCode :: GmState -> GmCode
getCode (i, stak, dump, heap, globals, stats) = i

putCode :: GmCode -> GmState -> GmState
putCode i' (i, stak, dump, heap, globals, stats)
  = (i', stak, dump, heap, globals, stats)

type GmStack = [Addr]

getStack :: GmState -> GmStack
getStack (i, stack, dump, heap, globals, stats) = stack

putStack :: GmStack -> GmState -> GmState
putStack stack' (i, stack, dump, heap, globals, stats)
  = (i, stack', dump, heap, globals, stats)

-- Mark4で追加
type GmDump = [GmDumpItem]
type GmDumpItem = (GmCode, GmStack)

-- Mark4で追加
getDump :: GmState -> GmDump
getDump (i, stack, dump, heap, globals, stats) = dump

-- Mark4で追加
putDump :: GmDump -> GmState -> GmState
putDump dump' (i, stack, dump, heap, globals, stats)
  = (i, stack, dump', heap, globals, stats)

type GmHeap = Heap Node

getHeap :: GmState -> GmHeap
getHeap (i, stack, dump, heap, globals, stats) = heap

putHeap :: GmHeap -> GmState -> GmState
putHeap heap' (i, stack, dump, heap, globals, stats)
  = (i, stack, dump, heap', globals, stats)

data Node
  = NNum Int           -- Numbers
  | NAp Addr Addr      -- Applications
  | NGlobal Int GmCode -- Globals
  | NInd Addr          -- Indirections  -- Mark2で追加
instance Eq Node
  where
    NNum    a   == NNum    b   = a == b -- needed to check conditions
    NAp     a b == NAp     c d = False  -- not needed
    NGlobal a b == NGlobal c d = False  -- not needed
    NInd    a   == NInd    b   = False  -- not needed

type GmGlobals = ASSOC Name Addr

getGlobals :: GmState -> GmGlobals
getGlobals (i, stack, dump, heap, globals, stats) = globals

-- Mark1の改良版で追加
putGlobals :: GmGlobals -> GmState -> GmState  -- 遷移規則 (3.14)
putGlobals globals' (i, stack, dump, heap, globals, stats) = (i, stack, dump, heap, globals', stats)

statInitial  :: GmStats
statIncSteps :: GmStats -> GmStats
statGetSteps :: GmStats -> Int

type GmStats = Int

statInitial    = 0
statIncSteps s = s + 1
statGetSteps s = s

getStats :: GmState -> GmStats
getStats (i, stack, dump, heap, globals, stats) = stats

putStats :: GmStats -> GmState -> GmState
putStats stats' (i, stack, dump, heap, globals, stats)
  = (i, stack, dump, heap, globals, stats')
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
dispatch (Slide      n) = slide n   -- Mark3で復活
dispatch Unwind         = unwind
dispatch (Update     n) = update n  -- Mark2で追加
dispatch (Pop        n) = pop n     -- Mark2で追加
dispatch (Alloc      n) = alloc n   -- Mark3で追加
dispatch Eval           = eval'     -- Mark4で追加
dispatch Add            = add       -- Mark4で追加
dispatch Sub            = sub       -- Mark4で追加
dispatch Mul            = mul       -- Mark4で追加
dispatch Div            = div'      -- Mark4で追加
dispatch Neg            = neg       -- Mark4で追加
dispatch Eq             = eq        -- Mark4で追加
dispatch Ne             = ne        -- Mark4で追加
dispatch Lt             = lt        -- Mark4で追加
dispatch Le             = le        -- Mark4で追加
dispatch Gt             = gt        -- Mark4で追加
dispatch Ge             = ge        -- Mark4で追加
dispatch (Cond     a b) = cond a b  -- Mark4で追加

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

push :: Int -> GmState -> GmState  -- 遷移規則 (3.18)
push n state
  = putStack (a:as) state
    where as = getStack state
          a = as !! n

getArg :: Node -> Addr
getArg (NAp a1 a2) = a2

slide :: Int -> GmState -> GmState  -- 遷移規則 (3.9)
slide n state
  = putStack (a: drop n as) state
    where (a:as) = getStack state

unwind :: GmState -> GmState  -- 遷移規則 (3.11, 3.17, 3.19, 3.22)
unwind state
  = newState (hLookup heap a)
    where
      (a:as) = getStack state
      heap = getHeap state
      newState (NNum n) = putCode i' (putStack (a : s') (putDump d state))  -- 遷移規則 (3.22)
        where ((i', s') : d) = getDump state
      newState (NAp a1 a2) = putCode [Unwind] (putStack (a1:a:as) state)  -- 遷移規則 (3.11)
      newState (NGlobal n c)  -- 遷移規則 (3.19)
        | length as < n = error "Unwinding with too few arguments"
        | otherwise = putCode c (putStack as' state)
          where as' = rearrange n (getHeap state) (getStack state)
      newState (NInd n) = putCode [Unwind] (putStack (n : as) state)  -- 遷移規則 (3.17)

-- Mark3で追加
rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as
  = take n as' ++ drop n as
    where as' = map (getArg . hLookup heap) (tl as)

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

-- Mark3で追加
alloc :: Int -> GmState -> GmState  -- 遷移規則 (3.20)
alloc n state
  = putHeap newHeap (putStack newStack state)
    where (newHeap, as) = allocNodes n (getHeap state)
          newStack = as ++ (getStack state)

-- Mark3で追加
allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
allocNodes 0 heap = (heap,  [])
{-
allocNodes (n+1) heap = (heap2, a:as)  -- (n+1)というパターンが使えない模様
                                       -- (1つ目の定義式で0の場合を定義しているので、2つ目の定義式は0以外だから単にnと書けばいいということ？)
                        where (heap1, as) = allocNodes n heap
-}
allocNodes n heap = (heap2, a:as)
                    where (heap1, as) = allocNodes (n - 1) heap
                          (heap2, a)  = hAlloc heap1 (NInd hNull)

-- Mark4で追加
eval' :: GmState -> GmState  -- 遷移規則 (3.23)
eval' state
  = putCode [Unwind] (putStack [a] (putDump newDump state))
    where (a : s) = getStack state
          newDump = (getCode state, s) : (getDump state)

-- Mark4で追加
add :: GmState -> GmState  -- 遷移規則 (3.24)
add state
  = arithmetic2 (+) state

-- Mark4で追加
sub :: GmState -> GmState  -- 遷移規則 (3.24)
sub state
  = arithmetic2 (-) state

-- Mark4で追加
mul :: GmState -> GmState  -- 遷移規則 (3.24)
mul state
  = arithmetic2 (*) state

-- Mark4で追加
div' :: GmState -> GmState  -- 遷移規則 (3.24)
div' state
  = arithmetic2 div state

-- Mark4で追加
neg :: GmState -> GmState  -- 遷移規則 (3.25)
neg state
  = arithmetic1 (* (-1)) state

-- Mark4で追加
eq :: GmState -> GmState  -- 遷移規則 (3.26)
eq state
  = comparison (==) state

-- Mark4で追加
ne :: GmState -> GmState  -- 遷移規則 (3.26)
ne state
  = comparison (/=) state

-- Mark4で追加
lt :: GmState -> GmState  -- 遷移規則 (3.26)
lt state
  = comparison (<) state

-- Mark4で追加
le :: GmState -> GmState  -- 遷移規則 (3.26)
le state
  = comparison (<=) state

-- Mark4で追加
gt :: GmState -> GmState  -- 遷移規則 (3.26)
gt state
  = comparison (>) state

-- Mark4で追加
ge :: GmState -> GmState  -- 遷移規則 (3.26)
ge state
  = comparison (>=) state

-- Mark4で追加
cond :: GmCode -> GmCode -> GmState -> GmState  -- 遷移規則 (3.27, 3.28)
cond i1 i2 state
  = newState (hLookup heap a)
    where
      (a : s) = getStack state
      heap = getHeap state
      newState c = putCode newCode (putStack s state)
        where i = getCode state 
              newCode | c == (NNum 1) = i1 ++ i  -- 遷移規則 (3.27)
                      | c == (NNum 0) = i2 ++ i  -- 遷移規則 (3.28)

-- Mark4で追加
boxInteger :: Int -> GmState -> GmState
boxInteger n state
  = putStack (a: getStack state) (putHeap h' state)
    where (h', a) = hAlloc (getHeap state) (NNum n)

-- Mark4で追加
unboxInteger :: Addr -> GmState -> Int
unboxInteger a state
  = ub (hLookup (getHeap state) a)
    where ub (NNum i) = i
          ub n        = error "Unboxing a non-integer"

-- Mark4で追加
primitive1 :: (b -> GmState -> GmState) -- boxing function
              -> (Addr -> GmState -> a) -- unbixing function
              -> (a -> b) -- operator
              -> (GmState -> GmState) -- state transition
primitive1 box unbox op state
  = box (op (unbox a state)) (putStack as state)
    where (a:as) = getStack state

-- Mark4で追加
primitive2 :: (b -> GmState -> GmState) -- boxing function
              -> (Addr -> GmState -> a) -- unboxing function
              -> (a -> a -> b) -- operator
              -> (GmState -> GmState) -- state transition
primitive2 box unbox op state
  = box (op (unbox a0 state) (unbox a1 state)) (putStack as state)
    where (a0:a1:as) = getStack state

-- Mark4で追加
arithmetic1 :: (Int -> Int)            -- arithmetic operator
               -> (GmState -> GmState) -- state transition
arithmetic1 = primitive1 boxInteger unboxInteger

-- Mark4で追加
arithmetic2 :: (Int -> Int -> Int)     -- arithmetic operation
               -> (GmState -> GmState) -- state transition
arithmetic2 = primitive2 boxInteger unboxInteger

-- Mark4で追加
boxBoolean :: Bool -> GmState -> GmState
boxBoolean b state
  = putStack (a: getStack state) (putHeap h' state)
    where (h',a) = hAlloc (getHeap state) (NNum b')
          b' | b         = 1
             | otherwise = 0

-- Mark4で追加
comparison :: (Int -> Int -> Bool) -> GmState -> GmState
comparison = primitive2 boxBoolean unboxInteger
-----------------------
-- 評価器 (ここまで) --
-----------------------

---------------------------------------
-- プログラムのコンパイル (ここから) --
---------------------------------------
compile :: CoreProgram -> GmState
compile program
  = (initialCode, [], [], heap, globals, statInitial)
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
initialCode = [Pushglobal "main", Eval]

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC  -- SCスキーム
compileSc (name, env, body)
  = (name, length env, compileR body (zip2 env [0..]))

compileR :: GmCompiler  -- Rスキーム
--compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]
compileR e env = compileC e env ++ [Update d, Pop d, Unwind]  -- 代わりに[Eval]をappendすると動かなくなる模様@勉強会
                 where d = length env

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

type GmEnvironment = ASSOC Name Int

compileC :: GmCompiler  -- Cスキーム
compileC (EVar v) args
  | elem v (aDomain args) = [Push n]
  | otherwise             = [Pushglobal v]
  where n = aLookup args v (error "")
compileC (ENum n)    env = [Pushint n]
compileC (EAp e1 e2) env = compileC e2 env ++
                           compileC e1 (argOffset 1 env) ++
                           [Mkap]
compileC (ELet recursive defs e) args  -- Mark3で追加
  | recursive = compileLetrec compileC defs e args  -- recursive が True  の場合
  | otherwise = compileLet    compileC defs e args  -- recursive が False の場合

-- Mark3で追加
compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
{-
GmCompiler の定義を分解すると、compileLet 関数の型シグネチャは
compileLet :: GmCompiler -> [(Name, CoreExpr)] -> CoreExpr -> GmEnvironment -> GmCode
となる。
compileLet comp defs expr env という関数定義の左辺は、
関数 compileLet に引数 comp : GmCompiler, defs : [(Name, CoreExpr)], expr : CoreExpr, env : GmEnvironment を与えると、
GmCode 型の戻り値を返すということを表している模様。
-}
compileLet comp defs expr env
  = compileLet' defs env ++ comp expr env' ++ [Slide (length defs)]
    where env' = compileArgs defs env

-- Mark3で追加
compileLet' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLet' [] env = []
compileLet' ((name, expr):defs) env
  = compileC expr env ++ compileLet' defs (argOffset 1 env)

-- Mark3で追加
compileLetrec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetrec comp defs expr env
  = [Alloc (length defs)] ++ compileLetrec' defs env' ++ comp expr env' ++ [Slide (length defs)]
    where env' = compileArgs defs env

-- Mark3で追加
compileLetrec' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLetrec' [] env = []
compileLetrec' ((name, expr):defs) env
  = compileC expr env ++ [Update (length defs)] ++ compileLetrec' defs env

-- Mark3で追加
compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env
  = zip (map first defs) [n-1, n-2 .. 0] ++ argOffset n env
    where n = length defs

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v,m) <- env]

compiledPrimitives :: [GmCompiledSC]
compiledPrimitives
  = [
     ("+",      2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind]),      -- Mark4で追加
     ("-",      2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind]),      -- Mark4で追加
     ("*",      2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind]),      -- Mark4で追加
     ("/",      2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind]),      -- Mark4で追加
     ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind]),                    -- Mark4で追加
     ("==",     2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind]),       -- Mark4で追加
     ("~=",     2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind]),       -- Mark4で追加
     ("<",      2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind]),       -- Mark4で追加
     ("<=",     2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind]),       -- Mark4で追加
     (">",      2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind]),       -- Mark4で追加
     (">=",     2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind]),       -- Mark4で追加
     ("if",     3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])  -- Mark4で追加
    ]
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
showInstruction (Push       n) = (iStr "Push ") `iAppend` (iNum n)
showInstruction (Pushint    n) = (iStr "Pushint ") `iAppend` (iNum n)
showInstruction Mkap           = iStr "Mkap"
showInstruction (Slide      n) = (iStr "Slide ") `iAppend` (iNum n)   -- Mark3で復活
showInstruction (Update     n) = (iStr "Update ") `iAppend` (iNum n)  -- Mark2で追加
showInstruction (Pop        n) = (iStr "Pop ") `iAppend` (iNum n)     -- Mark2で追加
showInstruction (Alloc      n) = (iStr "Alloc ") `iAppend` (iNum n)   -- Mark3で追加
showInstruction Eval           = iStr "Eval"                          -- Mark4で追加
showInstruction Add            = iStr "Add"                           -- Mark4で追加
showInstruction Sub            = iStr "Sub"                           -- Mark4で追加
showInstruction Mul            = iStr "Mul"                           -- Mark4で追加
showInstruction Div            = iStr "Div"                           -- Mark4で追加
showInstruction Neg            = iStr "Neg"                           -- Mark4で追加
showInstruction Eq             = iStr "Eq"                            -- Mark4で追加
showInstruction Ne             = iStr "Ne"                            -- Mark4で追加
showInstruction Lt             = iStr "Lt"                            -- Mark4で追加
showInstruction Le             = iStr "Le"                            -- Mark4で追加
showInstruction Gt             = iStr "Gt"                            -- Mark4で追加
showInstruction Ge             = iStr "Ge"                            -- Mark4で追加
showInstruction (Cond     a b)
  = (iStr "Cond ") `iAppend`
    (shortShowInstructions 3 a) `iAppend`
    (iStr " ") `iAppend`
    (shortShowInstructions 3 b)

showState :: GmState -> Iseq
showState s
  = iConcat [showStack s, iNewline,
             showDump s, iNewline,
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

-- Mark4で追加
showDump :: GmState -> Iseq
showDump s
  = iConcat [iStr " Dump:[",
             iIndent (iInterleave iNewline
                     (map showDumpItem (reverse (getDump s)))),
             iStr "]"]

-- Mark4で追加
showDumpItem :: GmDumpItem -> Iseq
showDumpItem (code, stack)
  = iConcat [iStr "<",
             shortShowInstructions 3 code, iStr ", ",
             shortShowStack stack, iStr ">"]

-- Mark4で追加
shortShowInstructions :: Int -> GmCode -> Iseq
shortShowInstructions number code
  = iConcat [iStr "{", iInterleave (iStr "; ") dotcodes, iStr "}"]
    where codes = map showInstruction (take number code)
          dotcodes | length code > number = codes ++ [iStr "..."]
                   | otherwise            = codes

-- Mark4で追加
shortShowStack :: GmStack -> Iseq
shortShowStack stack
  = iConcat [iStr "[",
             iInterleave (iStr ", ") (map (iStr . showaddr) stack),
             iStr "]"]

showNode :: GmState -> Addr -> Node -> Iseq
showNode s a (NNum n) = iNum n
showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
                             where v = head [n | (n,b) <- getGlobals s, a==b]
showNode s a (NAp a1 a2) = iConcat [iStr "Ap ", iStr (showaddr a1),
                                    iStr " ", iStr (showaddr a2)]
showNode s a (NInd n) = iConcat [iStr "Ind ", iStr(showaddr n)]  -- Mark2で追加

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
-- letとletrecのテスト --
b_2_1 = "main = let id1 = I I I " ++
                "in id1 id1 3"
b_2_2 = "oct g x = let h = twice g " ++ -- gの後のスペースは必要(gとinの間にスペースを入れる必要があるため)
                   "in let k = twice h " ++ -- hの後のスペースは必要(hとinの間にスペースを入れる必要があるため)
                   "in k (k x) ;" ++
        "main = oct I 4"
b_2_3 = "cons a b cc cn = cc a b ;" ++
        "nil cc cn = cn ;" ++
        "hd list = list K abort ;" ++
        "tl list = list K1 abort ;" ++
        "abort = abort ;" ++
        "infinite x = letrec xs = cons x xs " ++  -- xsの後のスペースは必要(xsとinの間にスペースを入れる必要があるため)
                     "in xs ;" ++
        "main = hd (tl (tl (infinite 4)))"
-- 算術演算のテスト --
-- 条件分岐なし --
b_3_1_1 = "main = 4*5+(2-5)"
b_3_1_2 = "inc x = x+1;" ++
          "main = twice twice inc 4"
b_3_1_3 = "cons a b cc cn = cc a b ;" ++
          "nil cc cn = cn ;" ++
          "length xs = xs length1 0 ;" ++
          "length1 x xs = 1 + (length xs) ;" ++
          "main = length (cons 3 (cons 3 (cons 3 nil)))"
-- 条件分岐あり --
b_3_2_1 = "fac n = if (n==0) 1 (n * fac (n-1)) ;" ++
          "main = fac 5"
b_3_2_2 = "gcd a b = if (a==b) " ++
                          "a " ++
                    "if (a<b) (gcd b a) (gcd b (a-b)) ;" ++
          "main = gcd 6 10"
{-
b_3_2_3 = "nfib n = if (n==0) 1 (1 + nfib (n-1) + nfib (n-2)) ;" ++  -- 自分のパーサは明示的に括弧で囲って2項演算にしてあげないとパースできなかった。
          "main = nfib 4"
-}
{-
b_3_2_3 = "nfib n = if (n==0) 1 ((1 + nfib (n-1)) + nfib (n-2)) ;" ++  -- 左記のように書き換えたら処理を実行するようになったが、終わる気配なし。
          "main = nfib 4"  -- nfib 3 にしても終わる気配なし。
-}
b_3_2_3 = "nfib n = if (n==0) 0 " ++
                   "if (n==1) 1 " ++
                   "(nfib (n-1) + nfib (n-2)) ;" ++
          "main = nfib 4"  -- このように書き換えたら、正しい値4が得られたが、962ステップもかかってしまった。

ex_4_20_2_1_1 = "x = 2 * 2 ; " ++
                "main = 1 + x"  -- (EAp (EAp op e1) e2) の op が2項演算子で e2 が EVar v の場合 (e1 は ENum n)

ex_4_20_2_1_2 = "x = 2 * 2 ; " ++
                "y = 2 / 2 ; " ++
                "main = y + x"  -- (EAp (EAp op e1) e2) の op が2項演算子で e2 が EVar v の場合 (e1 は EVar v)

ex_4_20_2_1_3 = "x = 2 * 2 ; " ++
                "inc n = n + 1 ; " ++
                "main = (inc 2) + x"  -- (EAp (EAp op e1) e2) の op が2項演算子で e2 が EVar v の場合 (e1 は ENum n, EVar v 以外)

ex_4_20_2_1_4 = "x = 2 * 2 ; " ++
                "inc n = n + 1 ; " ++
                "main = (K1 1 (inc 2)) + x"  -- (EAp (EAp op e1) e2) の op が2項演算子で e2 が EVar v の場合 (e1 は ENum n, EVar v 以外)

ex_4_20_2_2_1 = "main = 1 + 2"  -- (EAp (EAp op e1) e2) の op が2項演算子で e2 が ENum n の場合 (e1 は ENum n)

ex_4_20_2_2_2 = "y = 2 / 2 ; " ++
                "main = y + 2"  -- (EAp (EAp op e1) e2) の op が2項演算子で e2 が ENum n の場合 (e1 は EVar v)

ex_4_20_2_2_3 = "inc n = n + 1 ; " ++
                "main = (inc 1) + 2"  -- (EAp (EAp op e1) e2) の op が2項演算子で e2 が ENum n の場合 (e1 は ENum n, EVar v 以外)

ex_4_20_2_2_4 = "inc n = n + 1 ; " ++
                "main = (K1 1 (inc 1)) + 2"  -- (EAp (EAp op e1) e2) の op が2項演算子で e2 が EVar v の場合 (e1 は ENum n, EVar v 以外)

ex_4_20_2_3_1 = "inc n = n + 1 ; " ++
                "main = 1 + (inc 2)"  -- (EAp (EAp op e1) e2) の op が2項演算子で e2 が EVar v, ENum n 以外の場合 (e1 は ENum n)

ex_4_20_2_3_2 = "y = 2 / 2 ; " ++
                "inc n = n + 1 ; " ++
                "main = y + (inc 2)"  -- (EAp (EAp op e1) e2) の op が2項演算子で e2 が EVar v, ENum n 以外の場合 (e1 は EVar v)

ex_4_20_2_3_3 = "inc n = n + 1 ; " ++
                "main = (inc 1) + (inc 2)"  -- (EAp (EAp op e1) e2) の op が2項演算子で e2 が EVar v, ENum n 以外の場合 (e1 は ENum n, EVar v 以外)

ex_4_20_2_3_4 = "inc n = n + 1 ; " ++
                "main = (K1 1 (inc 1)) + (inc 2)"  -- (EAp (EAp op e1) e2) の op が2項演算子で e2 が EVar v, ENum n 以外の場合 (e1 は ENum n, EVar v 以外)

ex_4_20_2_4_1 = "x = 2 * 2 ; " ++
                "main = K1 1 x"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が EVar v の場合 (e1 は ENum n)

ex_4_20_2_4_2 = "x = 2 * 2 ; " ++
                "y = 2 / 2 ; " ++
                "main = K1 y x"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が EVar v の場合 (e1 は EVar v)

ex_4_20_2_4_3 = "x = 2 * 2 ; " ++
                "inc n = n + 1 ; " ++
                "main = K1 (inc 1) x"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が EVar v の場合 (e1 は ENum n, EVar v 以外)

ex_4_20_2_4_4 = "x = 2 * 2 ; " ++
                "inc n = n + 1 ; " ++
                "main = K1 (K1 1 (inc 1)) x"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が EVar v の場合 (e1 は ENum n, EVar v 以外)

ex_4_20_2_5_1 = "main = K1 1 2"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が ENum n の場合 (e1 は ENum n)

ex_4_20_2_5_2 = "y = 2 / 2 ; " ++
                "main = K1 y 2"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が ENum n の場合 (e1 は EVar v)

ex_4_20_2_5_3 = "inc n = n + 1 ; " ++
                "main = K1 (inc 2) 2"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が ENum n の場合 (e1 は ENum n, EVar v 以外)

ex_4_20_2_5_4 = "inc n = n + 1 ; " ++
                "main = K1 (K1 1 (inc 2)) 2"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が ENum n の場合 (e1 は ENum n, EVar v 以外)

ex_4_20_2_6_1 = "inc n = n + 1 ; " ++
                "main = K1 1 (inc 2)"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が EVar v, ENum n 以外の場合 (e1 は ENum n)

ex_4_20_2_6_2 = "y = 2 / 2 ; " ++
                "inc n = n + 1 ; " ++
                "main = K1 y (inc 2)"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が EVar v, ENum n 以外の場合 (e1 は EVar v)

ex_4_20_2_6_3 = "inc n = n + 1 ; " ++
                "main = K1 (inc 1) (inc 2)"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が EVar v, ENum n 以外の場合 (e1 は EVar v, ENum n 以外)

ex_4_20_2_6_4 = "inc n = n + 1 ; " ++
                "main = K1 (K1 1 (inc 1)) (inc 2)"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が EVar v, ENum n 以外の場合 (e1 は EVar v, ENum n 以外)

ex_4_20_2_7_1 = "inc n = n + 1 ; " ++
                "main = K1 1 (K1 1 (inc 1))"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が EVar v, ENum n 以外の場合 (e1 は ENum n)

ex_4_20_2_7_2 = "y = 2 / 2 ; " ++
                "inc n = n + 1 ; " ++
                "main = K1 y (K1 1 (inc 1))"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が EVar v, ENum n 以外の場合 (e1 は EVar v)

ex_4_20_2_7_3 = "inc n = n + 1 ; " ++
                "main = K1 (inc 2) (K1 1 (inc 1))"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が EVar v, ENum n 以外の場合 (e1 は EVar v, ENum n 以外)

ex_4_20_2_7_4 = "inc n = n + 1 ; " ++
                "main = K1 (K1 2 (inc 2)) (K1 1 (inc 1))"  -- (EAp (EAp op e1) e2) の op が2項演算子以外で e2 が EVar v, ENum n 以外の場合 (e1 は EVar v, ENum n 以外)
---------------------------------
-- テストプログラム (ここまで) --
---------------------------------
