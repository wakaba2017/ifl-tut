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
data Instruction = Take  Int
                 | Enter TimAMode
                 | Push  TimAMode

data TimAMode = Arg      Int
              | Label    [Char]
              | Code     [Instruction]
              | IntConst Int

type TimState = ([Instruction], -- The current instruction stream 
                                -- (現在の命令ストリームは、命令のリストで表されます。)
                 FramePtr,      -- Address of current frame
                                -- (通常、フレームポインタはヒープ内のフレームのアドレスですが、
                                --  整数値を保持するために使用されるか、
                                --  初期化されていない可能性があります。)
                 TimStack,      -- Stack of arguments
                                -- (スタックにはクロージャが含まれており、
                                --  それぞれがコードポインタとフレームポインタを含むペアです。
                                --  スタックをリストとして表します。)
                 TimValueStack, -- Value stack (not used yet)
                 TimDump,       -- Dump (not used yet)
                 TimHeap,       -- Heap of frames
                                -- (ヒープにはフレームが含まれており、それぞれがクロージャのタプルです。)
                 CodeStore,     -- Labelled blocks of code
                                -- (ラベルごとに、コードストアは対応するコンパイル済みコードを提供します。)
                 TimStats)      -- Statistics

data FramePtr = FrameAddr Addr -- The address of a frame
              | FrameInt Int   -- An integer value
              | FrameNull      -- Uninitialised

type TimStack = [Closure]
type Closure = ([Instruction], FramePtr)

data TimValueStack = DummyTimValueStack
data TimDump       = DummyTimDump

type TimHeap = Heap Frame

fAlloc  :: TimHeap -> [Closure] -> (TimHeap, FramePtr)
fGet    :: TimHeap -> FramePtr -> Int -> Closure
fUpdate :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
fList   :: Frame -> [Closure] -- Used when printing

{-
type Frame = [Closure]
-}
data Frame = FClosure [Closure]
           | FMarked Frame

fAlloc heap xs = (heap', FrameAddr addr)
                 where
                 {-
                 (heap', addr) = hAlloc heap xs
                 -}
                 (heap', addr) = hAlloc heap (FClosure xs)

fGet heap (FrameAddr addr) n = f !! (n-1)
                               where
                                 {-
                                 f = hLookup heap addr
                                 -}
                                 FClosure f = hLookup heap addr

fUpdate heap (FrameAddr addr) n closure
  {-
  = hUpdate heap addr new_frame
  -}
  = hUpdate heap addr (FClosure new_frame)
    where
      {-
      frame = hLookup heap addr
      -}
      FClosure frame = hLookup heap addr
      new_frame = take (n-1) frame ++ [closure] ++ drop n frame

{-
fList f = f
-}
fList (FClosure f) = f

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
    Int  -- Maximum stack depth
   )
{-
statInitial    = 0
statIncSteps s = s+1
statGetSteps s = s
-}
statInitial    = (0, 0, 0, 0)
statIncSteps (steps, exctime, totalheap, maxstkdepth)
  = (steps + 1, exctime, totalheap, maxstkdepth)
statGetSteps (steps, exctime, totalheap, maxstkdepth)
  = steps

statUpdExectime :: TimState -> TimState
statUpdExectime (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, maxstkdepth))
  = (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime', totalheap, maxstkdepth))
    where
      curInstr | null instr = Take 0
               | otherwise  = head instr
      exctime' = case curInstr of
                 Take n -> exctime + n
                 _      -> exctime + 1

statGetExectime :: TimStats -> Int
statGetExectime (steps, exctime, totalheap, maxstkdepth)
  = exctime

statUpdAllcdheap :: TimState -> TimState
statUpdAllcdheap (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, maxstkdepth))
  = (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap', maxstkdepth))
    where
      totalheap' | curTotalHeap > totalheap = curTotalHeap
                 | otherwise                = totalheap
      curTotalHeap = hSize heap

statGetAllcdheap :: TimStats -> Int
statGetAllcdheap (steps, exctime, totalheap, maxstkdepth)
  = totalheap

statUpdMaxstkdpth :: TimState -> TimState
statUpdMaxstkdpth (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, maxstkdepth))
  = (instr, frame, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, maxstkdepth'))
    where
      maxstkdepth' | curStackDepth > maxstkdepth = curStackDepth
                   | otherwise                   = maxstkdepth
      curStackDepth = length stack

statGetMaxstkdpth :: TimStats -> Int
statGetMaxstkdpth (steps, exctime, totalheap, maxstkdepth)
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

initialArgStack = []

initialValueStack = DummyTimValueStack
initialDump = DummyTimDump

compiledPrimitives = []

type TimCompilerEnv = [(Name, TimAMode)]

compileSC :: TimCompilerEnv -> CoreScDefn -> (Name, [Instruction])  -- SCスキーム
compileSC env (name, args, body)
  | lenArgs == 0 = (name, instructions)
  | otherwise    = (name, Take lenArgs : instructions)
    where
      instructions = compileR body new_env
      new_env = (zip2 args (map Arg [1..])) ++ env
      lenArgs = length args

compileR :: CoreExpr -> TimCompilerEnv -> [Instruction]  -- Rスキーム
compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
compileR (EVar v)    env = [Enter (compileA (EVar v) env)]
compileR (ENum n)    env = [Enter (compileA (ENum n) env)]
compileR e           env = error "compileR: can't do this yet"

compileA :: CoreExpr -> TimCompilerEnv -> TimAMode  -- Aスキーム
compileA (EVar v) env = aLookup env v (error ("Unknown variable " ++ v))
compileA (ENum n) env = IntConst n
compileA e env = Code (compileR e env)
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
      (_, _, _, _, _, heap, _, _) = step state
      -- next_state = (statUpdMaxstkdpth . statUpdAllcdheap . statUpdExectime . doAdmin) (step state)
      next_state' | hSize heap >= 5 = gc (step state)
                  | otherwise       = step state
      next_state = (statUpdMaxstkdpth . statUpdAllcdheap . statUpdExectime . doAdmin) next_state'

doAdmin :: TimState -> TimState
doAdmin state = applyToStats statIncSteps state

timFinal ([], frame, stack, vstack, dump, heap, cstore, stats) = True
timFinal state                                                 = False

applyToStats :: (TimStats -> TimStats) -> TimState -> TimState
applyToStats stats_fun (instr, frame, stack, vstack,
                        dump, heap, cstore, stats)
  = (instr, frame, stack, vstack, dump, heap, cstore, stats_fun stats)

step ((Take n : instr), fptr, stack, vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.1)
  | length stack >= n = (instr, fptr', drop n stack, vstack, dump, heap', cstore, stats)
  | otherwise         = error "Too few args for Take instruction"
  where (heap', fptr') = fAlloc heap (take n stack)
step ([Enter am], fptr, stack, vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.6, 4.7, 4.8, 4.9)
  = (instr', fptr', stack, vstack, dump, heap, cstore, stats)
    where (instr',fptr') = amToClosure am fptr heap cstore
step ((Push am:instr), fptr, stack, vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.2, 4.3, 4.4, 4.5)
  = (instr, fptr, amToClosure am fptr heap cstore : stack, vstack, dump, heap, cstore, stats)

amToClosure :: TimAMode -> FramePtr -> TimHeap -> CodeStore -> Closure
amToClosure (Arg n)      fptr heap cstore = fGet heap fptr n             -- 遷移規則 (4.2, 4.7)
amToClosure (Code il)    fptr heap cstore = (il, fptr)                   -- 遷移規則 (4.4, 4.8)
amToClosure (Label l)    fptr heap cstore = (codeLookup cstore l, fptr)  -- 遷移規則 (4.3, 4.6)
amToClosure (IntConst n) fptr heap cstore = (intCode, FrameInt n)        -- 遷移規則 (4.5, 4.9)

intCode = []  -- 今のところ、私たちのマシンは演算を行わないので、
              -- 簡単な解決策は \texttt{intCode} を空のコードシーケンスにすることです。

gc :: TimState -> TimState
gc (instr, fptr, stack, vstack, dump, heap, cstore, stats)
  = (instr, fptr, stack, vstack, dump, newHeap, cstore, stats)
    where
      adrlst1 = findStackRoots stack heap
      adrlst2 = findFrmPtrRoots fptr heap
      heap'   = mapAccuml' markFrom heap (adrlst1 ++ adrlst2)
      newHeap = mapAccuml' scanHeap heap' (hAddresses heap')
      mapAccuml' f acc []
        = acc
      mapAccuml' f acc (x : xs)
        = acc2
          where acc1 = f acc x
                acc2 = mapAccuml' f acc1 xs
{-
  1. TIMの状態から、ヒープとスタックとフレームポインタを取得する。
  2. 1.で取得したスタックを findStackRoots に渡して、スタックに格納されているフレームと、
     それらのフレームが参照しているフレームのヒープアドレスのリストを取得する。
  3. 1.で取得したフレームポインタを findFrmPtrRoots に渡して、フレームポインタが指しているフレームと、
     それらのフレームが参照しているフレームのヒープアドレスのリストを取得する。
  4. mapAccuml' に
       markFrom
       1.で取得したヒープ
       2.と3.で取得したヒープアドレスのリストを連結したリスト
     を渡して、使用中のフレームをマークしたヒープを作成する。
  5. mapAccuml' に
       scanHeap
       4.で作成したヒープ
       4.で作成したヒープの使用中アドレス (hAddressesを使って取得)
     を渡して、マークされていないフレームの格納領域を解放したヒープを作成する。
     (マークされたフレームは、マークされていない状態に戻る。)
-}

findStackRoots :: [Closure] -> TimHeap -> [Addr]
findStackRoots [] heap = []
findStackRoots ((_, FrameAddr addr) : as) heap
  = (findFrmPtrRoots (FrameAddr addr) heap) ++ (findStackRoots as heap)
findStackRoots ((_, _) : as) heap
  = findStackRoots as heap

findFrmPtrRoots :: FramePtr -> TimHeap -> [Addr]
findFrmPtrRoots (FrameAddr addr) heap
  = [addr] ++ addr'
    where
      frame = hLookup heap addr
      addr' = case frame of
              FClosure (c : cs) -> case c of
                                   (is, FrameAddr addr'') -> (findFrmPtrRoots (FrameAddr addr'') heap) ++
                                                             (findStackRoots cs heap)
                                   _                      -> findStackRoots cs heap
              _                 -> []
findFrmPtrRoots _ heap
  = []

markFrom :: TimHeap -> Addr -> TimHeap
markFrom heap addr
  = case f of
    FClosure f' -> hUpdate heap addr (FMarked (FClosure f'))
    FMarked _ -> heap
    where
      f = hLookup heap addr
  {-
    addr が指す heap の内容 f を、FMarked f に置き換える。(f が FMarked f' だったら、そのまま何もしない。)
    内容を更新した新しいヒープを返す。
  -}

scanHeap :: TimHeap -> Addr -> TimHeap
scanHeap heap addr
  = case item of
    FMarked item' -> hUpdate heap addr item'
    _             -> hFree heap addr
    where item = hLookup heap addr
  {-
    ヒープを表す三つ組みの第三要素(二つ組のリスト)を取り出す。
    取り出した二つ組のリストの第一要素(アドレス)だけ取り出したリストを作る。
    mapAccumul関数と補助関数とヒープとアドレスのリストを使って、未使用領域を解放したヒープを作る。
    補助関数は、ヒープとアドレスをもらって、アドレスが指す中身を調べて、
      FMarked _ だったら、FMarkedを外す。
      FMarked _ でなかったら、hFreeで領域を解放する。
  -}
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
      iNewline
    ]

showFrame :: TimHeap -> FramePtr -> Iseq
showFrame heap FrameNull = iStr "Null frame ptr" `iAppend` iNewline
showFrame heap (FrameAddr addr)
  = iConcat [
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
showValueStack vstack = iNil

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

showInstruction d (Take m)  = (iStr "Take ")  `iAppend` (iNum m)
showInstruction d (Enter x) = (iStr "Enter ") `iAppend` (showArg d x)
showInstruction d (Push x)  = (iStr "Push ")  `iAppend` (showArg d x)

showArg d (Arg m)      = (iStr "Arg ")      `iAppend` (iNum m)
showArg d (Code il)    = (iStr "Code ")     `iAppend` (showInstructions d il)
showArg d (Label s)    = (iStr "Label ")    `iAppend` (iStr s)
showArg d (IntConst n) = (iStr "IntConst ") `iAppend` (iNum n)

nTerse = 3
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
--------------------------------
-- テストプログラム (ここまで) --
--------------------------------