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
                 deriving Show  -- テキストにはないけれど追加

data TimAMode = Arg      Int
              | Label    [Char]
              | Code     [Instruction]
              | IntConst Int
              deriving Show  -- テキストにはないけれど追加

type TimState = ([Instruction], -- The current instruction stream 
                 FramePtr,      -- Address of current frame
                 UsdSltNmbrs,   -- Used slot numbers
                 TimStack,      -- Stack of arguments
                 TimValueStack, -- Value stack (not used yet)
                 TimDump,       -- Dump (not used yet)
                 TimHeap,       -- Heap of frames
                 CodeStore,     -- Labelled blocks of code
                 TimStats)      -- Statistics

data FramePtr = FrameAddr Addr -- The address of a frame
              | FrameInt Int   -- An integer value
              | FrameNull      -- Uninitialised
              deriving (Eq, Show)  -- テキストにはないけれど追加

type UsdSltNmbrs = [Int]

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
statUpdExectime (instr, frame, usedslot, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, totalclosure, maxstkdepth))
  = (instr, frame, usedslot, stack, vstack, dump, heap, cstore, (steps, exctime', totalheap, totalclosure, maxstkdepth))
    where
      curInstr | null instr = Take 0
               | otherwise  = head instr
      exctime' = case curInstr of
                 Take n -> exctime + n
                 _      -> exctime + 1

statGetExectime :: TimStats -> Int
statGetExectime (steps, exctime, totalheap, totalclosure, maxstkdepth)
  = exctime

statUpdAllcdheap :: TimState -> TimState
statUpdAllcdheap (instr, frame, usedslot, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, totalclosure, maxstkdepth))
  = (instr, frame, usedslot, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap', totalclosure, maxstkdepth))
    where
      totalheap' | curTotalHeap > totalheap = curTotalHeap
                 | otherwise                = totalheap
      curTotalHeap = hSize heap

statGetAllcdheap :: TimStats -> Int
statGetAllcdheap (steps, exctime, totalheap, totalclosure, maxstkdepth)
  = totalheap

statUpdAllcdclosure :: TimState -> TimState
statUpdAllcdclosure (instr, frame, usedslot, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, totalclosure, maxstkdepth))
  = (instr, frame, usedslot, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, totalclosure_, maxstkdepth))
    where
      tempList = map (hLookup heap) (hAddresses heap)
      subFunc (FClosure fcl) = length fcl
      curTotalclosure = sum $ map subFunc tempList
      totalclosure_ | curTotalclosure > totalclosure = curTotalclosure
                    | otherwise                      = totalclosure

statGetAllcdclosure :: TimStats -> Int
statGetAllcdclosure (steps, exctime, totalheap, totalclosure, maxstkdepth)
  = totalclosure

statUpdMaxstkdpth :: TimState -> TimState
statUpdMaxstkdpth (instr, frame, usedslot, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, totalclosure, maxstkdepth))
  = (instr, frame, usedslot, stack, vstack, dump, heap, cstore, (steps, exctime, totalheap, totalclosure, maxstkdepth'))
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
     initialUsdSltNmbrs,     -- Used slot numbers
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

initialUsdSltNmbrs = []

initialArgStack = []

initialValueStack = DummyTimValueStack
initialDump = DummyTimDump

compiledPrimitives = []

type TimCompilerEnv = [(Name, TimAMode)]

compileSC :: TimCompilerEnv -> CoreScDefn -> (Name, [Instruction])  -- SCスキーム
compileSC env (name, args, body)
  -- = (name, Take (length args) : instructions)
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
      (_, _, _, _, _, _, heap, _, _) = step state
      -- next_state = (statUpdMaxstkdpth . statUpdAllcdheap . statUpdExectime . doAdmin) (step state)
      next_state' | hSize heap >= 1 = gc (step state)
                  | otherwise       = step state
      next_state = (statUpdMaxstkdpth . statUpdAllcdclosure . statUpdAllcdheap . statUpdExectime . doAdmin) next_state'

doAdmin :: TimState -> TimState
doAdmin state = applyToStats statIncSteps state

timFinal ([], frame, usedslot, stack, vstack, dump, heap, cstore, stats) = True
timFinal state                                                 = False

applyToStats :: (TimStats -> TimStats) -> TimState -> TimState
applyToStats stats_fun (instr, frame, usedslot, stack, vstack,
                        dump, heap, cstore, stats)
  = (instr, frame, usedslot, stack, vstack, dump, heap, cstore, stats_fun stats)

step ((Take n : instr), fptr, usdsltnum, stack, vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.1)
  | length stack >= n = (instr, fptr', usdsltnum, drop n stack, vstack, dump, heap', cstore, stats)
  | otherwise         = error "Too few args for Take instruction"
  where (heap', fptr') = fAlloc heap (take n stack)
        usdsltnum = getUsedSlotNumber instr
step ([Enter am], fptr, usdsltnum, stack, vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.6, 4.7, 4.8, 4.9)
  = (instr', fptr', usdsltnum_, stack, vstack, dump, heap, cstore, stats)
    where (instr',fptr') = amToClosure am fptr heap cstore
          usdsltnum_ = case instr_ of
                      [] -> usdsltnum
                      Take _ : _ -> case fptr' of
                                    FrameAddr addr -> take n [1..]
                                                      where n = length cl
                                                            FClosure cl = hLookup heap addr
                                    _ -> []
                      _ -> getUsedSlotNumber instr_
          instr_ = case am of
                   Arg n -> fst (fGet heap fptr n)
                   Code il -> il
                   Label l -> codeLookup cstore l
                   _ -> []
step ((Push am:instr), fptr, usdsltnum, stack, vstack, dump, heap, cstore, stats)  -- 遷移規則 (4.2, 4.3, 4.4, 4.5)
  = (instr, fptr, usdsltnum, amToClosure am fptr heap cstore : stack, vstack, dump, heap, cstore, stats)

amToClosure :: TimAMode -> FramePtr -> TimHeap -> CodeStore -> Closure
amToClosure (Arg n)      fptr heap cstore = fGet heap fptr n             -- 遷移規則 (4.2, 4.7)
amToClosure (Code il)    fptr heap cstore = (il, fptr)                   -- 遷移規則 (4.4, 4.8)
amToClosure (Label l)    fptr heap cstore = (codeLookup cstore l, fptr)  -- 遷移規則 (4.3, 4.6)
amToClosure (IntConst n) fptr heap cstore = (intCode, FrameInt n)        -- 遷移規則 (4.5, 4.9)

intCode = []  -- 今のところ、私たちのマシンは演算を行わないので、
              -- 簡単な解決策は \texttt{intCode} を空のコードシーケンスにすることです。

gc :: TimState -> TimState
gc (instr, fptr, usdsltnum, stack, vstack, dump, heap, cstore, stats)
  = (instr, fptr, usdsltnum, stack, vstack, dump, newHeap, cstore, stats)
    where
      adrlst1 = findStackRoots stack fptr heap usdsltnum
      adrlst2 = findFrmPtrRoots_ fptr heap usdsltnum
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

findStackRoots :: [Closure] -> FramePtr -> TimHeap -> [Int] -> [Addr]
findStackRoots [] fptr heap usdsltnum = []
findStackRoots ((_, FrameAddr addr) : as) fptr heap usdsltnum
  = if (FrameAddr addr) == fptr
    then
      (findFrmPtrRoots_ (FrameAddr addr) heap usdsltnum) ++ (findStackRoots as fptr heap usdsltnum)
    else
      (findFrmPtrRoots (FrameAddr addr) heap) ++ (findStackRoots as fptr heap usdsltnum)
findStackRoots ((_, _) : as) fptr heap usdsltnum  -- クロージャのフレームポインタ部分がFrameNullの場合は、ここに該当。
  = findStackRoots as fptr heap usdsltnum
{-
  findStackRootsも、フレームポインタと使用スロット番号リストを引数として受け取り、スタックに積まれているクロージャのフレームポインタを再帰的にたどる際、
  引数として受け取ったフレームポインタと同じだった場合、全スロットをたどるのではなく、使用スロット番号リストに含まれるスロットだけをたどるようにする。
-}

findFrmPtrRoots :: FramePtr -> TimHeap -> [Addr]  -- スタックに積まれているクロージャのフレームポインタをたどる関数(全スロットをたどる)
findFrmPtrRoots (FrameAddr addr) heap
  = [addr] ++ addr'
    where
      frame = hLookup heap addr
      addr' = case frame of
              FClosure cs -> tempFunc $ map (\x -> findFrmPtrRoots (snd x) heap) tempList
                             where
                               tempList = [(is, fptr_) | (is, fptr_) <- cs, fptr_ /= FrameAddr addr]
                               tempFunc [] = []
                               tempFunc (x : xs) = x ++ tempFunc xs
              _                 -> []
findFrmPtrRoots _ heap
  = []

findFrmPtrRoots_ :: FramePtr -> TimHeap -> [Int] -> [Addr] -- フレームポインタが指すフレームに含まれるクロージャのフレームポインタをたどる関数(使用するスロットだけをたどる)
findFrmPtrRoots_ (FrameAddr addr) heap usdsltnum
  = [addr] ++ addr_
    where
      usdsltnum_ = map (\x -> x - 1) usdsltnum  -- 0始まりにするために、usdsltnum の各要素から1を引く。
      frame = hLookup heap addr
      usdslt
        = case frame of
          FClosure (c : cs) -> map subFunc tempList
                               where
                                 tempList = map ((c : cs) !!) usdsltnum_
                                 subFunc (is, fptr_) = if fptr_ == FrameAddr addr
                                                       then (is, FrameNull)
                                                       else (is, fptr_)
          _                 -> []
      addr_ = case usdslt of
              []  -> []
              cs_ -> findStackRoots cs_ (FrameAddr addr) heap usdsltnum
      {-
        frame が FClosure [Closure] だったら、[Closure] から、usdsltnum に含まれるスロット番号に対応する要素だけ抽出したリストを作る。
        抽出したリストができたら、そのリストの要素(Closure)を1つずつ調べて、タプルの第2要素が FrameAddr addr__ だったら、
        FrameAddr addr__ を findFrmPtrRoots に渡して、再帰的に参照フレームをたどってアドレスを抽出する。
      -}
findFrmPtrRoots_ _ heap usdsltnum
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

getUsedSlotNumber :: [Instruction] -> [Int]
getUsedSlotNumber []
  = []
getUsedSlotNumber (i : il)
  = uniqList tempList
    where
      tempList = (getUsedSlotNumberSub i) ++ (getUsedSlotNumber il)
      getUsedSlotNumberSub i
        = case i of
          Push (Arg n) -> [n]
          Enter (Arg n) -> [n]
          Push (Code il_) -> getUsedSlotNumber il_
          _ -> []
      uniqList []     = []
      uniqList (x:xs) = (if x `elem` xs then [] else [x]) ++ (uniqList xs)
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
showSCDefns (instr, fptr, usdsltnum, stack, vstack, dump, heap, cstore, stats)
  = iInterleave iNewline (map showSC cstore)

showSC :: (Name, [Instruction]) -> Iseq
showSC (name, il)
  = iConcat [
      iStr "Code for ", iStr name, iStr ":", iNewline,
      iStr " ", showInstructions Full il, iNewline, -- iNewline
      iStr "Used slot number for ", iStr name, iStr ":", iNewline,
      iStr " ", showUsedSlotNumber il, iNewline,
      iNewline
    ]

showState :: TimState -> Iseq
showState (instr, fptr, usdsltnum, stack, vstack, dump, heap, cstore, stats)
  = iConcat [
      iStr "Code: ", showInstructions Terse instr, iNewline,
      showFrame heap fptr,
      showStack stack,
      showValueStack vstack,
      showDump dump,
      showUsedHeap heap,  -- GCデバッグ用
      showUsdSltNmbrs usdsltnum,
      iNewline
    ]

showUsedHeap :: TimHeap -> Iseq
showUsedHeap heap
  = iConcat [
      iStr "Used Heap : [",
      iIndent (iInterleave iNewline (map (showFrame heap) (map FrameAddr (hAddresses heap)))),
      iStr "]", iNewline
    ]

showUsdSltNmbrs :: [Int] -> Iseq
showUsdSltNmbrs []
  = iConcat [iStr "Used Slot Numbers : [", iStr "]"]
showUsdSltNmbrs usdsltnum
  = iConcat [iStr "Used Slot Numbers : [", (iInterleave (iStr ", ") (map iNum usdsltnum)), iStr "]"]

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
showStats (instr, fptr, usdsltnum, stack, vstack, dump, heap, code, stats)
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

showInstruction d (Take m)  = (iStr "Take ")  `iAppend` (iNum m)
showInstruction d (Enter x) = (iStr "Enter ") `iAppend` (showArg d x)
showInstruction d (Push x)  = (iStr "Push ")  `iAppend` (showArg d x)

showArg d (Arg m)      = (iStr "Arg ")      `iAppend` (iNum m)
showArg d (Code il)    = (iStr "Code ")     `iAppend` (showInstructions d il)
showArg d (Label s)    = (iStr "Label ")    `iAppend` (iStr s)
showArg d (IntConst n) = (iStr "IntConst ") `iAppend` (iNum n)

nTerse = 3

showCompiledCode :: String -> String
showCompiledCode coreprg
  = show codes
    where
      (_, _, _, _, _, _, _, codes, _) = compile $ parse coreprg

showUsedSlotNumber :: [Instruction] -> Iseq
showUsedSlotNumber []
  = iConcat [iStr "[", iStr "]"]
showUsedSlotNumber il
  = iConcat [iStr "[", (iInterleave (iStr ", ") (map iNum (getUsedSlotNumber il))), iStr "]"]
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
test_program_for_gc = "compose2 f g x = f (g x x) ; " ++
                      "main = compose2 I K 3"
test_program_for_gc' = "compose2 f g x = f (g x x) ; " ++
                       "id = I ; " ++
                       "selarg1 = K ; " ++
                       "main = compose2 id selarg1 3"
test_program_for_gc'' = "compose2 f g x = f (g x x) ; " ++
                        "id x = I x ; " ++
                        "selarg1 x y = K x y ; " ++
                        "main = compose2 id selarg1 3"
test_program_for_gc''' = "compose2 f g x = f (g x x) ; " ++
                         "func x = K1 1 x ; " ++
                         "selarg1 x y = K x y ; " ++
                         "main = compose2 func selarg1 3"
test_program_for_gc'''' = "compose2 f g x = f (g x x) ; " ++
                          "id = S K K ; " ++
                          "selarg1 = K ; " ++
                          "main = compose2 id selarg1 3"
test_program_for_gc''''' = "compose2 f g x = f (g x x) ; " ++
                           "func = K1 (S K K 3) ; " ++
                           "selarg1 = K ; " ++
                           "main = compose2 func selarg1 3"
--------------------------------
-- テストプログラム (ここまで) --
--------------------------------

main :: IO()
main = (putStrLn . fullRun) b_1_1_3
