import Language
import Utils

-------------------------
-- 全体構造 (ここから) --
-------------------------
runProg     :: [Char] -> [Char]
compile     :: CoreProgram -> TimState
eval        :: TimState -> [TimState]
showResults :: [TimState] -> [Char]

runProg = showResults . eval . compile . parse

fullRun :: [Char] -> [Char]
fullRun = showFullResults . eval . compile . parse
-------------------------
-- 全体構造 (ここまで) --
-------------------------

-------------------------------
-- データ型の定義 (ここから) --
-------------------------------
data Instruction = Take  Int Int                      -- Mark3で変更
                 | Enter TimAMode
                 | PushV ValueAMode                   -- Mark2で追加
                 | Push  TimAMode
                 | Return                             -- Mark2で追加
                 | Op    Op                           -- Mark2で追加
                 | Cond  [Instruction] [Instruction]  -- Mark2で追加
                 | Move  Int TimAMode                 -- Mark3で追加
                 deriving Show  -- テキストにはないけれど追加
                 {- Mark2からTIMは3命令マシンではなくなった。 -}

-- Mark2で追加
data Op = Add
        | Sub
        | Mult
        | Div
        | Neg
        | Gr
        | GrEq
        | Lt
        | LtEq
        | Eq
        | NotEq
        -- deriving (Eq) -- KH
        deriving (Eq, Show) -- KH

data TimAMode = Arg      Int
              | Label    [Char]
              | Code     [Instruction]
              | IntConst Int
              deriving Show  -- テキストにはないけれど追加

-- Mark2で追加
data ValueAMode = FramePtr
                | IntVConst Int
                deriving Show  -- テキストにはないけれど追加

type TimState = ([Instruction], -- The current instruction stream
                 FramePtr,      -- Address of current frame
                 TimStack,      -- Stack of arguments
                 TimValueStack, -- Value stack (Mark2から使用開始)
                 TimDump,       -- Dump (not used yet)
                 TimHeap,       -- Heap of frames
                 CodeStore,     -- Labelled blocks of code
                 TimStats)      -- Statistics

data FramePtr = FrameAddr Addr -- The address of a frame
              | FrameInt Int   -- An integer value
              | FrameNull      -- Uninitialised
              deriving Show  -- テキストにはないけれど追加

type TimStack = [Closure]
type Closure = ([Instruction], FramePtr)

type TimValueStack = [Int]  -- Mark2で変更
data TimDump       = DummyTimDump

type TimHeap = Heap Frame

fAlloc  :: TimHeap -> [Closure] -> (TimHeap, FramePtr)
fGet    :: TimHeap -> FramePtr -> Int -> Closure
fUpdate :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
fList   :: Frame -> [Closure] -- Used when printing

type Frame = [Closure]

fAlloc heap xs = (heap', FrameAddr addr)
                 where
                 (heap', addr) = hAlloc heap xs

fGet heap (FrameAddr addr) n = f !! (n-1)
                               where
                                 f = hLookup heap addr
fGet heap _                n = ([], FrameNull)

fUpdate heap (FrameAddr addr) n closure
  = hUpdate heap addr new_frame
    where
      frame = hLookup heap addr
      new_frame = take (n-1) frame ++ [closure] ++ drop n frame

fList f = f

type CodeStore = ASSOC Name [Instruction]

codeLookup :: CodeStore -> Name -> [Instruction]
codeLookup cstore l
  = aLookup cstore l (error ("Attempt to jump to unknown label "
                             ++ show l))

statInitial  :: TimStats
statIncSteps :: TimStats -> TimStats
statGetSteps :: TimStats -> Int

-- type TimStats = Int -- The number of steps
type TimStats
 = (
    Int, -- The number of steps
    Int, -- Execution time
    Int, -- Total amount of heap allocated in the run
    Int, -- Total amount of closure allocated in the run
    Int  -- Maximum stack depth
   )
{-
statInitial    = 0
statIncSteps s = s+1
statGetSteps s = s
-}
statInitial    = (0, 0, 0, 0, 0)
statIncSteps (steps, exctime, totalheap, totalclosure, maxstkdepth)
  = (steps + 1, exctime, totalheap, totalclosure, maxstkdepth)
statGetSteps (steps, exctime, totalheap, totalclosure, maxstkdepth)
  = steps

statUpdExectime :: TimState -> TimState
statUpdExectime (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, totalclosure, maxstkdepth))
  = (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime', totalheap, totalclosure, maxstkdepth))
    where
      curInstr | null instr = Take 0 0  -- Mark3で変更
               | otherwise  = head instr
      exctime' = case curInstr of
                 Take t n -> exctime + t  -- Mark3で変更
                 _      -> exctime + 1

statGetExectime :: TimStats -> Int
statGetExectime (steps, exctime, totalheap, totalclosure, maxstkdepth)
  = exctime

statUpdAllcdheap :: TimState -> TimState
statUpdAllcdheap (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, totalclosure, maxstkdepth))
  = (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap', totalclosure, maxstkdepth))
    where
      totalheap' | curTotalHeap > totalheap = curTotalHeap
                 | otherwise                = totalheap
      curTotalHeap = hSize heap

statGetAllcdheap :: TimStats -> Int
statGetAllcdheap (steps, exctime, totalheap, totalclosure, maxstkdepth)
  = totalheap

statUpdAllcdclosure :: TimState -> TimState
statUpdAllcdclosure (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, totalclosure, maxstkdepth))
  = (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, totalclosure_, maxstkdepth))
    where
      curTotalclosure = sum $ map (length . (hLookup heap)) (hAddresses heap)
      totalclosure_ | curTotalclosure > totalclosure = curTotalclosure
                    | otherwise                      = totalclosure

statGetAllcdclosure :: TimStats -> Int
statGetAllcdclosure (steps, exctime, totalheap, totalclosure, maxstkdepth)
  = totalclosure

statUpdMaxstkdpth :: TimState -> TimState
statUpdMaxstkdpth (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, totalclosure, maxstkdepth))
  = (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, totalclosure, maxstkdepth'))
    where
      maxstkdepth' | curStackDepth > maxstkdepth = curStackDepth
                   | otherwise                   = maxstkdepth
      curStackDepth = length stack

statGetMaxstkdpth :: TimStats -> Int
statGetMaxstkdpth (steps, exctime, totalheap, totalclosure, maxstkdepth)
  = maxstkdepth

-- :a util.lhs -- heap data type and other library functions
-------------------------------
-- データ型の定義 (ここまで) --
-------------------------------

---------------------------------------
-- プログラムのコンパイル (ここから) --
---------------------------------------
compile program
  = ([Enter (Label "main")], -- Initial instructions
     FrameNull,              -- Null frame pointer
     initialArgStack,        -- Argument stack
     initialValueStack,      -- Value stack
     initialDump,            -- Dump
     hInitial,               -- Empty heap
     compiled_code,          -- Compiled code for supercombinators
     statInitial)            -- Initial statistics
    where
      sc_defs = preludeDefs ++ program
      compiled_sc_defs = map (compileSC initial_env) sc_defs
      compiled_code = compiled_sc_defs ++ compiledPrimitives
      initial_env = [(name, Label name) | (name, args, body) <- sc_defs] ++
                    [(name, Label name) | (name, code) <- compiledPrimitives]

initialArgStack = [([], FrameNull)]  -- Mark2で変更

initialValueStack = []  -- Mark2で変更
initialDump = DummyTimDump

compiledPrimitives :: [([Char], [Instruction])]
compiledPrimitives  -- Mark2で変更
  = [
    {-
      ("+",      [Take 2, Push (Code [Push (Code [Op Add,   Return]), Enter (Arg 1)]), Enter (Arg 2)])
    , ("-",      [Take 2, Push (Code [Push (Code [Op Sub,   Return]), Enter (Arg 1)]), Enter (Arg 2)])
    , ("*",      [Take 2, Push (Code [Push (Code [Op Mult,  Return]), Enter (Arg 1)]), Enter (Arg 2)])
    , ("/",      [Take 2, Push (Code [Push (Code [Op Div,   Return]), Enter (Arg 1)]), Enter (Arg 2)])
    , ("negate", [Take 1, Push (Code [Op Neg, Return]),                                Enter (Arg 1)])
    , (">",      [Take 2, Push (Code [Push (Code [Op Gr,    Return]), Enter (Arg 1)]), Enter (Arg 2)])
    , (">=",     [Take 2, Push (Code [Push (Code [Op GrEq,  Return]), Enter (Arg 1)]), Enter (Arg 2)])
    , ("<",      [Take 2, Push (Code [Push (Code [Op Lt,    Return]), Enter (Arg 1)]), Enter (Arg 2)])
    , ("<=",     [Take 2, Push (Code [Push (Code [Op LtEq,  Return]), Enter (Arg 1)]), Enter (Arg 2)])
    , ("==",     [Take 2, Push (Code [Push (Code [Op Eq,    Return]), Enter (Arg 1)]), Enter (Arg 2)])
    , ("~=",     [Take 2, Push (Code [Push (Code [Op NotEq, Return]), Enter (Arg 1)]), Enter (Arg 2)])
    , ("if",     [Take 3, Push (Code [Cond [Enter (Arg 2)] [Enter (Arg 3)]]), Enter (Arg 1)])
    -}
    ]

type TimCompilerEnv = [(Name, TimAMode)]

compileSC :: TimCompilerEnv -> CoreScDefn -> (Name, [Instruction])  -- SCスキーム
compileSC env (name, args, body)
--  | lenArgs == 0 = (name, instructions)
  | lenRqdSlts == 0 = (name, instructions)
  | otherwise    = (name, Take lenRqdSlts lenArgs : instructions)  -- Mark3で変更
    where
      (lenRqdSlts, instructions) = compileR body new_env lenArgs  -- Mark3で変更
      new_env = (zip2 args (map Arg [1..])) ++ env
      lenArgs = length args

compileR :: CoreExpr -> TimCompilerEnv -> Int -> (Int, [Instruction])  -- Rスキーム  Mark3で変更
compileR (ELet recursion defs e) env d  -- Mark3で追加
  = (d_, mvinstrs ++ is)
    where
      defs1 = map fst defs
      defs2 = map snd defs
      subFunc1 dd ee
        | recursion == True = compileA ee env__ dd  -- letrec の場合
        | otherwise         = compileA ee env   dd  -- let    の場合
      (dn, ams) = mapAccuml subFunc1 (d + length defs) defs2
      env_  = (zip2 defs1 (map Arg [(d+1)..])) ++ env
      -- env__ = (zip2 defs1 (map Code (map (replicate 1) (map Enter (map Arg [(d+1)..]))))) ++ env
      env__ = (zip2 defs1 (map (Code . (replicate 1) . Enter . Arg) [(d+1)..])) ++ env
      (d_, is) = compileR e env_ dn
      subFunc2 n am = (n + 1, Move n am)
      (_, mvinstrs) = mapAccuml subFunc2 (d + 1) ams
compileR (EAp (EAp (EAp (EVar "if") e0) e1) e2) env d  -- Mark3で変更
  = compileB e0 env d3 [Cond e1_ e2_]  -- Mark3で変更
    where (d1, e1_) = compileR e1 env d  -- Mark3で変更
          (d2, e2_) = compileR e2 env d  -- Mark3で変更 (これだと、then節とelse節が必要とするスロットが、共通に領域確保される。)
          -- (d2, e2_) = compileR e2 env d1  -- Mark3で変更 (これだと、then節とelse節が必要とするスロットが、個別に領域確保される。)
          d3 = max d1 d2
compileR (EAp (EAp (EVar op) e1) e2) env d  -- Mark3で変更
  | op `elem` op_list = compileB (EAp (EAp (EVar op) e1) e2) env d [Return]  -- Mark3で変更
  | otherwise         = (d3, Push am1 : Push am2 : is)  -- Mark3で変更
  where op_list = ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "~="]
        (d1, am1) = compileA e2 env d
        (d2, am2) = compileA e1 env d1  -- Mark3で変更
        (d3, is) = compileR (EVar op) env d2  -- Mark3で変更
compileR (EAp (EVar "negate") e) env d  -- Mark3で変更
  = compileB (EAp (EVar "negate") e) env d [Return]  -- Mark3で変更
compileR (EAp e1 e2) env d = (d2, Push am : is)  -- Mark3で変更
                             where (d1, am) = compileA e2 env d  -- Mark3で変更
                                   (d2, is) = compileR e1 env d1  -- Mark3で変更
compileR (EVar v)    env d = (d, [Enter (snd (compileA (EVar v) env d))])  -- Mark3で変更
-- compileR (ENum n)    env = [Enter (compileA (ENum n) env)]
compileR (ENum n)    env d = compileB (ENum n) env d [Return]  -- Mark3で変更
compileR e           env d = (d_, [Enter am])   -- Mark3で変更
                             where (d_, am) = compileA e env d  -- Mark3で変更

compileA :: CoreExpr -> TimCompilerEnv -> Int -> (Int, TimAMode)  -- Aスキーム  Mark3で変更
compileA (EVar v) env d = (d, aLookup env v (error ("Unknown variable " ++ v)))  -- Mark3で変更
compileA (ENum n) env d = (d, IntConst n)  -- Mark3で変更
compileA e env d = (d_, Code is)   -- Mark3で変更
                   where (d_, is) = compileR e env d  -- Mark3で変更

-- Mark2で追加
compileB :: CoreExpr -> TimCompilerEnv -> Int -> [Instruction] -> (Int, [Instruction])  -- Bスキーム  Mark3で変更
compileB (EAp (EAp (EVar op) e1) e2) env d cont  -- Mark3で変更
  = (d2, is2)  -- Mark3で変更
    where
      i = case op of
          "+"  -> Op Add
          "-"  -> Op Sub
          "*"  -> Op Mult
          "/"  -> Op Div
          ">"  -> Op Gr
          ">=" -> Op GrEq
          "<"  -> Op Lt
          "<=" -> Op LtEq
          "==" -> Op Eq
          "~=" -> Op NotEq
      (d1, is1) = compileB e1 env d (i : cont)  -- Mark3で変更
      -- (d2, is2) = compileB e2 env d1 is1  -- Mark3で変更
      (d2, is2) = compileB e2 env d is1  -- Mark3で変更
      d3 = max d1 d2
compileB (EAp (EVar "negate") e) env d cont  -- Mark3で変更
  = compileB e env d (Op Neg : cont)  -- Mark3で変更
compileB (ENum n) env d cont  -- Mark3で変更
  = (d, PushV (IntVConst n) : cont)
compileB e env d cont  -- Mark3で変更
  = (d_, Push (Code cont) : is)  -- Mark3で変更
    where (d_, is) = compileR e env d  -- Mark3で変更

-- Mark2で追加
mkIndMode :: Int -> TimAMode
mkIndMode n = Code [Enter (Arg n)]
---------------------------------------
-- プログラムのコンパイル (ここまで) --
---------------------------------------

-----------------------
-- 評価器 (ここから) --
-----------------------
eval state
  = state : rest_states
    where
      rest_states | timFinal state = []
                  | otherwise      = eval next_state
      -- next_state = doAdmin (step state)
      next_state = (statUpdMaxstkdpth . statUpdAllcdclosure . statUpdAllcdheap . statUpdExectime . doAdmin) (step state)

doAdmin :: TimState -> TimState
doAdmin state = applyToStats statIncSteps state

timFinal ([], frame, stack, vstack, dump, heap, cstore, stats) = True
timFinal state                                                 = False

applyToStats :: (TimStats -> TimStats) -> TimState -> TimState
applyToStats stats_fun (instr, frame, stack, vstack,
                        dump, heap, cstore, stats)
  = (instr, frame, stack, vstack, dump, heap, cstore, stats_fun stats)

step ((Take t n : instr), fptr, stack, vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.x)  Mark3で変更
  | (t >= n) && (length stack >= n) = (instr, fptr', drop n stack, vstack, dump, heap', cstore, stats)
  | otherwise                       = error "Too small alloc area or too few args for Take instruction"
  where (heap', fptr') = fAlloc heap tmpFrame
        tmpFrame = (take n stack) ++ (take (t - n) (repeat ([], FrameNull)))
step ([Enter am], fptr, stack, vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.6, 4.7, 4.8, 4.9)
  = (instr', fptr', stack, vstack, dump, heap, cstore, stats)
    where (instr',fptr') = amToClosure am fptr heap cstore
step ((Push am:instr), fptr, stack, vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.2, 4.3, 4.4, 4.5)
  = (instr, fptr, amToClosure am fptr heap cstore : stack, vstack, dump, heap, cstore, stats)
step ([Return], fptr, (instr', fptr') : stack, vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.11)
  = (instr', fptr', stack, vstack, dump, heap, cstore, stats)
step ((PushV (IntVConst n) : instr), fptr, stack, vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.14)
  = (instr, fptr, stack, n : vstack, dump, heap, cstore, stats)
step ((PushV FramePtr : instr), (FrameInt n), stack, vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.12)
  = (instr, (FrameInt n), stack, n : vstack, dump, heap, cstore, stats)
step ((Op Neg : instr), fptr, stack, n : vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.10)
  = (instr, fptr, stack, -n : vstack, dump, heap, cstore, stats)
step ((Op op : instr), fptr, stack, n1 : n2 : vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.10)
  = (instr, fptr, stack, result : vstack, dump, heap, cstore, stats)
    where
      result = case op of
               Add   -> n1 + n2
               Sub   -> n1 - n2
               Mult  -> n1 * n2
               Div   -> n1 `div` n2
               Gr    -> if n1 >  n2 then 0 else 1
               GrEq  -> if n1 >= n2 then 0 else 1
               Lt    -> if n1 <  n2 then 0 else 1
               LtEq  -> if n1 <= n2 then 0 else 1
               Eq    -> if n1 == n2 then 0 else 1
               NotEq -> if n1 /= n2 then 0 else 1
step ([Cond i1 i2], fptr, stack, n : vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.13)
  = (i, fptr, stack, vstack, dump, heap, cstore, stats)
    where i | n == 0    = i1
            | otherwise = i2
step ((Move i a : instr), fptr, stack, vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.x)  Mark3で追加
  = (instr, fptr, stack, vstack, dump, newHeap, cstore, stats)
    where
      curFrameSize = case fptr of
                     FrameAddr addr -> length $ hLookup heap addr
                     _ -> 0
      newHeap | i <= curFrameSize = fUpdate heap fptr i (amToClosure a fptr heap cstore)
              | otherwise = error ("Move instruction argument slot number " ++ show i ++ " exceeds frame size " ++ show curFrameSize)

amToClosure :: TimAMode -> FramePtr -> TimHeap -> CodeStore -> Closure
amToClosure (Arg n)      fptr heap cstore = fGet heap fptr n             -- 遷移規則 (4.2, 4.7)
amToClosure (Code il)    fptr heap cstore = (il, fptr)                   -- 遷移規則 (4.4, 4.8)
amToClosure (Label l)    fptr heap cstore = (codeLookup cstore l, fptr)  -- 遷移規則 (4.3, 4.6)
amToClosure (IntConst n) fptr heap cstore = (intCode, FrameInt n)        -- 遷移規則 (4.5, 4.9)

intCode = [PushV FramePtr, Return]  -- Mark2で変更

----------------------
-- 評価器 (ここまで) --
----------------------

--------------------------
-- 結果の表示 (ここから) --
--------------------------
showFullResults states
  = iDisplay (iConcat [
      iStr "Supercombinator definitions", iNewline, iNewline,
      showSCDefns first_state, iNewline, iNewline,
      iStr "State transitions", iNewline,
      iLayn (map showState states), iNewline, iNewline,
      showStats (last states)
    ])
    where
      (first_state:rest_states) = states

showResults states
  = iDisplay (iConcat [
      showState last_state, iNewline, iNewline, showStats last_state
    ])
    where last_state = last states

showSCDefns :: TimState -> Iseq
showSCDefns (instr, fptr, stack, vstack, dump, heap, cstore, stats)
  = iInterleave iNewline (map showSC cstore)

showSC :: (Name, [Instruction]) -> Iseq
showSC (name, il)
  = iConcat [
      iStr "Code for ", iStr name, iStr ":", iNewline,
      iStr " ", showInstructions Full il, iNewline, iNewline
    ]

showState :: TimState -> Iseq
showState (instr, fptr, stack, vstack, dump, heap, cstore, stats)
  = iConcat [
      iStr "Code: ", showInstructions Terse instr, iNewline,
      showFrame heap fptr,
      showStack stack,
      showValueStack vstack,
      showDump dump,
      showUsedHeap heap,  -- デバッグ用
      iNewline
    ]

showUsedHeap :: TimHeap -> Iseq
showUsedHeap heap
  = iConcat [
      iStr "Used Heap : [",
      iIndent (iInterleave iNewline (map (showFrame heap) (map FrameAddr (hAddresses heap)))),
      iStr "]", iNewline
    ]

showFrame :: TimHeap -> FramePtr -> Iseq
showFrame heap FrameNull = iStr "Null frame ptr" `iAppend` iNewline
showFrame heap (FrameAddr addr)
  = iConcat [
      iStr "addr: ", iNum addr, iStr ", ",  -- for debug
      iStr "Frame: <",
      iIndent (iInterleave iNewline
      (map showClosure (fList (hLookup heap addr)))),
      iStr ">", iNewline
    ]
showFrame heap (FrameInt n)
  = iConcat [ iStr "Frame ptr (int): ", iNum n, iNewline ]

showStack :: TimStack -> Iseq
showStack stack
  = iConcat [ iStr "Arg stack: [",
      iIndent (iInterleave iNewline (map showClosure stack)),
      iStr "]", iNewline
    ]

showValueStack :: TimValueStack -> Iseq
showValueStack vstack
  -- Mark2で変更
  = iConcat [ iStr "Value stack: [",
      iIndent (iInterleave iNewline (map iNum vstack)),
      iStr "]", iNewline
    ]

showDump :: TimDump -> Iseq
showDump dump = iNil

showClosure :: Closure -> Iseq
showClosure (i,f)
  = iConcat [ iStr "(", showInstructions Terse i, iStr ", ",
      showFramePtr f, iStr ")"
    ]

showFramePtr :: FramePtr -> Iseq
showFramePtr FrameNull     = iStr "null"
showFramePtr (FrameAddr a) = iStr (show a)
showFramePtr (FrameInt n)  = iStr "int " `iAppend` iNum n

showStats :: TimState -> Iseq
showStats (instr, fptr, stack, vstack, dump, heap, code, stats)
  = iConcat [ iStr "Steps taken = ", iNum (statGetSteps stats), iNewline,
              iStr "No of frames allocated = ", iNum (hSize heap),
              iNewline
            , iStr "Execution time = ", iNum (statGetExectime stats),
              iNewline
            , iStr "Total amount of heap allocated in the run = ", iNum (statGetAllcdheap stats),
              iNewline
            , iStr "Total amount of closure allocated in the run = ", iNum (statGetAllcdclosure stats),
              iNewline
            , iStr "Maximum stack depth = ", iNum (statGetMaxstkdpth stats),
              iNewline
    ]

data HowMuchToPrint = Full | Terse | None

showInstructions :: HowMuchToPrint -> [Instruction] -> Iseq
showInstructions None il = iStr "{..}"
showInstructions Terse il
  = iConcat [iStr "{", iIndent (iInterleave (iStr ", ") body), iStr "}"]
    where
      instrs = map (showInstruction None) il
      body | length il <= nTerse = instrs
           | otherwise           = (take nTerse instrs) ++ [iStr ".."]
showInstructions Full il
  = iConcat [iStr "{ ", iIndent (iInterleave sep instrs), iStr " }"]
    where
      sep = iStr "," `iAppend` iNewline
      instrs = map (showInstruction Full) il

showInstruction d (Take t n)  = (iStr "Take ") `iAppend` (iNum t) `iAppend` (iStr " ") `iAppend` (iNum n)  -- Mark3で変更
showInstruction d (Enter x) = (iStr "Enter ") `iAppend` (showArg d x)
showInstruction d (Push x)  = (iStr "Push ")  `iAppend` (showArg d x)
showInstruction d (PushV x)    = (iStr "PushV ") `iAppend` iseqx             -- Mark2で追加
                                 where iseqx = case x of
                                               FramePtr    -> iStr "FramePtr"
                                               IntVConst n -> (iStr "IntVConst ") `iAppend` (iNum n)
showInstruction d Return       = iStr "Return"                               -- Mark2で追加
showInstruction d (Op op)      = (iStr "Op ")    `iAppend` (iStr (show op))  -- Mark2で追加
showInstruction d (Cond i1 i2) = (iStr "Cond ")  `iAppend`
                                 (showInstructions d i1) `iAppend`
                                 (iStr " ") `iAppend`
                                 (showInstructions d i2)                     -- Mark2で追加
showInstruction d (Move i a)   = (iStr "Move ") `iAppend` (iNum i) `iAppend` (iStr " ") `iAppend` (showArg d a)  -- Mark3で追加

showArg d (Arg m)      = (iStr "Arg ")      `iAppend` (iNum m)
showArg d (Code il)    = (iStr "Code ")     `iAppend` (showInstructions d il)
showArg d (Label s)    = (iStr "Label ")    `iAppend` (iStr s)
showArg d (IntConst n) = (iStr "IntConst ") `iAppend` (iNum n)

nTerse = 3

showCompiledCode :: String -> String
showCompiledCode coreprg
  = show codes
    where
      (_, _, _, _, _, _, codes, _) = compile $ parse coreprg
--------------------------
-- 結果の表示 (ここまで) --
--------------------------

--------------------------------
-- テストプログラム (ここから) --
--------------------------------
ex_4_1_1 = "main = S K K 4"
ex_4_1_2 = "id = S K K ; " ++
           "id1 = id id ; " ++
           "main = id1 4"
test_program1 = "main = K 1 2"
test_program2 = "main = K1 1 2"
ex_4_5_1 = "main = if 0 1 2"
ex_4_5_2 = "main = if 1 1 2"
ex_4_5_3 = "factorial n = if n 1 (n * factorial (n-1)) ; " ++
           "main = factorial 3"
ex_4_8 = "fib n = if (n < 2) 1 (fib (n-1) + fib (n-2)) ; " ++
         "main = fib 10"
ex_4_9 = "multipleof3 x = ((x / 3) * 3) == x ; " ++
         "f y = if (multipleof3 y) 0 1 ; " ++
         "main = f 3"
ex_4_9' = "multipleof3 x = ((x / 3) * 3) == x ; " ++
          "f y = if (multipleof3 y) 0 1 ; " ++
          "main = f 4"
-- 超基本テスト --
b_1_1_1 = "main = I 3"
b_1_1_2 = "id = S K K ;" ++
          "main = id 3"
b_1_1_2' = "id = S K K ;" ++
           "main = twice id 3"
b_1_1_2'' = "id = S K K ;" ++
            "main = twice twice id 3"
b_1_1_3 = "id = S K K ;" ++
          "main = twice twice twice id 3"
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
                    "(if (a<b) (gcd b a) (gcd b (a-b))) ;" ++  -- 正常終了するためにはelse説を括弧で囲む必要あり
          "main = gcd 6 10"
b_3_2_3 = "nfib n = if (n==0) 0 " ++
                   "(if (n==1) 1 " ++
                   "(nfib (n-1) + nfib (n-2))) ;" ++
          "main = nfib 8"
b_3_2_3' = "nfib n = if (n<2) 1 " ++
                    "(nfib (n-1) + nfib (n-2)) ;" ++
           "main = nfib 10"
ex_4_12_1 = --"f x y z = let p = x+y in p+x+y+z ; " ++
            "f x y z = let p = x+y in (p+x)+(y+z) ; " ++  -- パーサの実装が不十分！括弧で囲んで明示的に2項演算の形にしないとパースできない模様。
            "main = f 1 2 3"
{-
ex_4_12_2 = "f' p x y z = p+x+y+z ; " ++
            "f x y z = f' (x+y) x y z ; " ++
            "main = f 1 2 3"
-}
ex_4_12_2 = "f_ p x y z = (p+x)+(y+z) ; " ++  -- パーサの実装が不十分！括弧で囲んで明示的に2項演算の形にしないとパースできない模様。
            "f x y z = f_ (x+y) x y z ; " ++  -- パーサの実装が不十分！コア言語プログラムの変数名や関数名に「'」があるとパースできない模様。
            "main = f 1 2 3"
ex_4_13 = "f x = letrec p = if (x==0) 1 q ; " ++
                       "q = if (x==0) p 2 " ++
                "in p+q ; " ++
          "main = f 1"
ex_4_13' = "f x = letrec a = b ; " ++
                        "b = x " ++
                  "in a ; " ++
           "main = f 10"
ex_4_15_1 = "f x = if x (let x = 1 in x) (let y = 2 in y) ; " ++
            "main = f 0"
ex_4_15_2 = "f x = if x (let x = 1 in x) (let y = 2 in y) ; " ++
            "main = f 1"
ex_4_15_3 = "main = (let x = 1 in x) + (let y = 2 in y)"
test_program_for_let1 = "main = let x = 1 " ++
                               "in " ++
                               "  let y = 2 " ++
                               "  in x + 2"
--------------------------------
-- テストプログラム (ここまで) --
--------------------------------

main :: IO()
--main = (putStrLn . fullRun) b_3_2_3'
--main = (putStrLn . runProg) ex_4_8
main = (putStrLn . runProg) b_1_1_3
