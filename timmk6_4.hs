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
                 | PushMarker Int                     -- Mark4で追加
                 | UpdateMarkers Int                  -- Mark4で追加
                 | Switch [(Int, [Instruction])]      -- Mark5で追加
                 | ReturnConstr Int                   -- Mark5で追加
                 | Print                              -- Mark5で追加
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
              | Label    [Char] Int  -- Mark6で変更
              | Code     [Instruction]
              | IntConst Int
              | Data     Int  -- Mark5で追加
              deriving Show  -- テキストにはないけれど追加

-- Mark2で追加
data ValueAMode = FramePtr
                | IntVConst Int
                deriving Show  -- テキストにはないけれど追加

type TimState = (TimOutput,     -- Current Output  -- Mark5で追加
                 [Instruction], -- The current instruction stream
                 FramePtr,      -- Address of current frame
                 FramePtr,      -- Address of current data frame (Mark5から使用開始)
                 UsdSltNmbrs,   -- Used slot numbers
                 TimStack,      -- Stack of arguments
                 TimValueStack, -- Value stack (Mark2から使用開始)
                 TimDump,       -- Dump (not used yet)
                 TimHeap,       -- Heap of frames
                 CodeStore,     -- Heap address of global frame
                 ASSOC Name Int,  -- associative list of SC name and its index
                 TimStats)      -- Statistics

data FramePtr = FrameAddr Addr -- The address of a frame
              | FrameInt Int   -- An integer value
              | FrameNull      -- Uninitialised
              deriving (Eq, Show)  -- テキストにはないけれど追加

-- Mark5で追加
type TimOutput = [Char]

type UsdSltNmbrs = [Int]

type TimStack = [Closure]
type Closure = ([Instruction], FramePtr)

type TimValueStack = [Int]  -- Mark2で変更
type TimDump = [(FramePtr, -- The frame to be updated
                 Int,      -- Index of slot to be updated
                 TimStack) -- Old stack
               ]  -- Mark4で変更

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
fGet heap _                n = ([], FrameNull)

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

-- type CodeStore = ASSOC Name [Instruction]
-- type CodeStore = (Addr, ASSOC Name Int)  -- Mark6で変更
type CodeStore = Addr  -- Mark6で変更

-- Mark6で変更
{-
codeLookup :: CodeStore -> Name -> [Instruction]
codeLookup cstore l
  = aLookup cstore l (error ("Attempt to jump to unknown label "
                             ++ show l))
-}
codeLookup :: ASSOC Name Int -> Name -> Int
codeLookup g l
  = aLookup g l (error ("Attempt to jump to unknown label "
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
    Int, -- Maximum stack depth
    Int  -- Maximum vstack depth
   )
{-
statInitial    = 0
statIncSteps s = s+1
statGetSteps s = s
-}
statInitial    = (0, 0, 0, 0, 0, 0)
statIncSteps (steps, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth)
  = (steps + 1, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth)
statGetSteps (steps, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth)
  = steps

-- Mark5で変更 (出力情報とデータフレームポインタを追加) Mark6で変更 (CodeStore型変更対応)
statUpdExectime :: TimState -> TimState
statUpdExectime (output, instr, frame, dataframe, usedslot, stack, vstack, dump, heap, cstore, offsets, (steps, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth))
  = (output, instr, frame, dataframe, usedslot, stack, vstack, dump, heap, cstore, offsets, (steps, exctime', totalheap, totalclosure, maxstkdepth, maxvstkdepth))
    where
      curInstr | null instr = Take 0 0  -- Mark3で変更
               | otherwise  = head instr
      exctime' = case curInstr of
                 Take t n -> exctime + n  -- Mark3で変更
                 _      -> exctime + 1

statGetExectime :: TimStats -> Int
statGetExectime (steps, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth)
  = exctime

-- Mark5で変更 (出力情報とデータフレームポインタを追加) Mark6で変更 (CodeStore型変更対応)
statUpdAllcdheap :: TimState -> TimState
statUpdAllcdheap (output, instr, frame, dataframe, usedslot, stack, vstack, dump, heap, cstore, offsets, (steps, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth))
  = (output, instr, frame, dataframe, usedslot, stack, vstack, dump, heap, cstore, offsets, (steps, exctime, totalheap', totalclosure, maxstkdepth, maxvstkdepth))
    where
      totalheap' | curTotalHeap > totalheap = curTotalHeap
                 | otherwise                = totalheap
      curTotalHeap = hSize heap

statGetAllcdheap :: TimStats -> Int
statGetAllcdheap (steps, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth)
  = totalheap

-- Mark5で変更 (出力情報とデータフレームポインタを追加) Mark6で変更 (CodeStore型変更対応)
statUpdAllcdclosure :: TimState -> TimState
statUpdAllcdclosure (output, instr, frame, dataframe, usedslot, stack, vstack, dump, heap, cstore, offsets, (steps, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth))
  = (output, instr, frame, dataframe, usedslot, stack, vstack, dump, heap, cstore, offsets, (steps, exctime, totalheap, totalclosure_, maxstkdepth, maxvstkdepth))
    where
      tempList = map (hLookup heap) (hAddresses heap)
      subFunc (FClosure fcl) = length fcl
      curTotalclosure = sum $ map subFunc tempList
      totalclosure_ | curTotalclosure > totalclosure = curTotalclosure
                    | otherwise                      = totalclosure

statGetAllcdclosure :: TimStats -> Int
statGetAllcdclosure (steps, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth)
  = totalclosure

-- Mark5で変更 (出力情報とデータフレームポインタを追加) Mark6で変更 (CodeStore型変更対応)
statUpdMaxstkdpth :: TimState -> TimState
statUpdMaxstkdpth (output, instr, frame, dataframe, usedslot, stack, vstack, dump, heap, cstore, offsets, (steps, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth))
  = (output, instr, frame, dataframe, usedslot, stack, vstack, dump, heap, cstore, offsets, (steps, exctime, totalheap, totalclosure, maxstkdepth', maxvstkdepth))
    where
      maxstkdepth' | curStackDepth > maxstkdepth = curStackDepth
                   | otherwise                   = maxstkdepth
      curStackDepth = length stack

statGetMaxstkdpth :: TimStats -> Int
statGetMaxstkdpth (steps, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth)
  = maxstkdepth

-- Mark5で変更 (出力情報とデータフレームポインタを追加) Mark6で変更 (CodeStore型変更対応)
statUpdMaxvstkdpth :: TimState -> TimState
statUpdMaxvstkdpth (output, instr, frame, dataframe, usedslot, stack, vstack, dump, heap, cstore, offsets, (steps, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth))
  = (output, instr, frame, dataframe, usedslot, stack, vstack, dump, heap, cstore, offsets, (steps, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth_))
    where
      maxvstkdepth_ | curVstackDepth > maxvstkdepth = curVstackDepth
                    | otherwise                     = maxvstkdepth
      curVstackDepth = length vstack

statGetMaxvstkdpth :: TimStats -> Int
statGetMaxvstkdpth (steps, exctime, totalheap, totalclosure, maxstkdepth, maxvstkdepth)
  = maxvstkdepth

-- :a util.lhs -- heap data type and other library functions
-------------------------------
-- データ型の定義 (ここまで) --
-------------------------------

---------------------------------------
-- プログラムのコンパイル (ここから) --
---------------------------------------
compile program
  = ("",   -- Mark5で追加
     [Enter (Label "main" k)], -- Initial instructions  -- Mark6で変更
     FrameNull,              -- Null frame pointer
     FrameNull,              -- Null data frame pointer  -- Mark5で追加
     initialUsdSltNmbrs,     -- Used slot numbers
     initialArgStack,        -- Argument stack
     initialValueStack,      -- Value stack
     initialDump,            -- Dump
     initialHeap,            -- Initial heap  -- Mark6で変更
     codeStore,              -- Heap address of global frame  -- Mark6で変更
     offsets,                -- associative list of supercombinator name and its index  -- Mark6で追加
     statInitial)            -- Initial statistics
    where
      sc_defs = preludeDefs ++ program
      compiled_sc_defs = map (compileSC initial_env) sc_defs
      compiled_code = compiledPrimitives ++ compiled_sc_defs
      initial_env = [(name, Label name index) | (name, args, body) <- sc_defs, (name_, index) <- offsets, name == name_] ++
                    [(name, Label name index) | (name, code) <- compiledPrimitives, (name_, index) <- offsets, name == name_]
      (initialHeap, codeStore, offsets) = allocateInitialHeap compiled_code
      k = snd $ head [(name, offset) | (name, offset) <- offsets, name == "main"]

initialUsdSltNmbrs = []

initialArgStack = [([], FrameNull)]  -- Mark2で変更

initialValueStack = []  -- Mark2で変更
initialDump = []  -- Mark4で変更

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
      ("topCont",  [Switch [(1, []), (2, [Move 1 (Data 1), Move 2 (Data 2), Push (Label "headCont" 2), Enter (Arg 1)])]])
    , ("headCont", [Print, Push (Label "topCont" 1), Enter (Arg 2)])
    ]

-- Mark6で追加
allocateInitialHeap :: [(Name, [Instruction])] -> (TimHeap, CodeStore, ASSOC Name Int)
allocateInitialHeap compiled_code
  = (heap, global_frame_addr, offsets)
    where
      indexed_code = zip2 [1..] compiled_code
      offsets  = [(name, offset) | (offset, (name, code)) <- indexed_code]
      -- closures = [(PushMarker offset : code, global_frame_addr) | (offset, (name, code)) <- indexed_code]
      -- closures = [(PushMarker offset : code, FrameAddr global_frame_addr) | (offset, (name, code)) <- indexed_code]  -- 型不一致を回避するためテキストの内容を書き換えた。
      closures = map subFuncForAllocateInitialHeap [(offset, code) | (offset, (name, code)) <- indexed_code]  -- 型不一致を回避するためテキストの内容を書き換えた。
      subFuncForAllocateInitialHeap (offset, []) = ([], FrameAddr global_frame_addr)
      subFuncForAllocateInitialHeap (offset, c : cs) = case c of
                                                       UpdateMarkers _ -> (c : cs, FrameAddr global_frame_addr)
                                                                          {-
                                                                            UpdateMarkers n 命令は全ての Take t n 命令の直前に付加される。
                                                                            したがって、UpdateMarkers n 命令の直後には Take t n 命令が存在する。
                                                                            また、UpdateMarkers 0 命令は生成されないようにした。
                                                                            したがって、UpdateMarkers n 命令の直後の Take t n 命令の n は0より大きい。
                                                                            よって、この場合はCAFに該当せず、PushMarker offset 命令を付加する必要はないはず。
                                                                          -}
                                                       Take _ n -> if n > 0
                                                                   then (                    c : cs, FrameAddr global_frame_addr)
                                                                   else (PushMarker offset : c : cs, FrameAddr global_frame_addr)
                                                       _        -> (PushMarker offset : c : cs, FrameAddr global_frame_addr)
      -- (heap, global_frame_addr) = fAlloc hInitial closures
      (heap, FrameAddr global_frame_addr) = fAlloc hInitial closures  -- 型不一致を回避するためテキストの内容を書き換えた。

type TimCompilerEnv = [(Name, TimAMode)]

compileSC :: TimCompilerEnv -> CoreScDefn -> (Name, [Instruction])  -- SCスキーム
compileSC env (name, args, body)
  | lenRqdSlts == 0 = (name, instructions)
  | lenArgs    == 0 = (name, Take lenRqdSlts lenArgs : instructions)  -- Mark4で変更
  | otherwise       = (name, UpdateMarkers lenArgs : Take lenRqdSlts lenArgs : instructions)  -- Mark4で変更
    where
      (lenRqdSlts, instructions) = compileR body new_env lenArgs  -- Mark3で変更
      -- new_env = (zip2 args (map Arg [1..])) ++ env
      -- new_env = (zip2 args (map mkUpdIndMode [1..])) ++ env  -- Mark4で変更
      new_env = (zip2 args (map Arg [1..])) ++ env  -- Mark6で再変更(引数があるスーパーコンビネータのコンパイル時には mkUpdIndMode 関数は不要。)
      lenArgs = length args

compileR :: CoreExpr -> TimCompilerEnv -> Int -> (Int, [Instruction])  -- Rスキーム  Mark5で変更
compileR (EConstr t a) env d  -- Mark5で追加
  | a >  0    = (d, [UpdateMarkers a, Take a a, ReturnConstr t])
  | a == 0    = (d, [ReturnConstr t])
  | otherwise = error "Pack arity error."

compileR (ECase e alts) env d  -- Mark5で追加
  = (d_, Push (Code [Switch (map snd brchs)]) : is_e)
    where
      brchs = map (\alt -> compileE alt env d) alts
      max_d = maximum $ map fst brchs
      (d_, is_e) = compileR e env max_d

compileR (ELet recursion defs e) env d  -- Mark3で追加
  = (d_, mvinstrs ++ is)
    where
      defs1 = map fst defs
      defs2 = map snd defs
      subFunc1 dd ee
        | recursion == True = compileU ee (dd - length defs + 1) env_ dd  -- letrec の場合
        | otherwise         = compileU ee (dd - length defs + 1) env  dd  -- let    の場合
      (dn, ams) = mapAccuml subFunc1 (d + length defs) defs2
      env_  = (zip2 defs1 (map mkIndMode [(d+1)..])) ++ env  -- Mark4で変更
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
  | otherwise         = case e2 of
                        EVar v -> (d1, Push am : is)  -- Mark4で変更
                                  where am = compileA (EVar v) env
                                        (d1, is) = compileR (EAp (EVar op) e1) env d
                        ENum n -> (d1, Push am : is)  -- Mark4で変更
                                  where am = compileA (ENum n) env
                                        (d1, is) = compileR (EAp (EVar op) e1) env d
                        _      -> (d2, Move (d + 1) am1 : Push (mkIndMode (d + 1)) : is)  -- Mark4で変更
                                  where (d1, am1) = compileU e2 (d + 1) env (d + 1)  -- Mark4で変更
                                        (d2, is)  = compileR (EAp (EVar op) e1) env d1  -- Mark4で変更
  where op_list = ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "~="]
compileR (EAp (EVar "negate") e) env d  -- Mark3で変更
  = compileB (EAp (EVar "negate") e) env d [Return]  -- Mark3で変更
compileR (EAp e (EVar v)) env d = (d1, Push am : is)  -- Mark4で変更
                                  where am = compileA (EVar v) env
                                        (d1, is) = compileR e env d
compileR (EAp e (ENum n)) env d = (d1, Push am : is)  -- Mark4で変更
                                  where am = compileA (ENum n) env
                                        (d1, is) = compileR e env d
compileR (EAp e1 e2) env d = (d2, Move (d + 1) am : Push (mkIndMode (d + 1)) : is)  -- Mark4で変更
                             where (d1, am) = compileU e2 (d + 1) env (d + 1)  -- Mark4で変更
                                   (d2, is) = compileR e1 env d1  -- Mark4で変更
compileR (EVar v)    env d = (d, is)  -- Mark4で変更
                             where am = compileA (EVar v) env
                                   is = mkEnter am
compileR (ENum n)    env d = compileB (ENum n) env d [Return]  -- Mark3で変更
compileR e           env d = (d, is)   -- Mark4で変更
                             where am = compileA e env  -- Mark4で変更
                                   is = mkEnter am
{-
  このコードでは、compileRの最後の定義式は使われないかもしれない。
-}

compileA :: CoreExpr -> TimCompilerEnv -> TimAMode  -- Aスキーム  Mark4で変更
compileA (EVar v) env = aLookup env v (error ("Unknown variable " ++ v))  -- Mark4で変更
compileA (ENum n) env = IntConst n  -- Mark4で変更
compileA _        env = error "CoreExpr not supported by compileA. (e.g. ELam)"

-- Mark2で追加
compileB :: CoreExpr -> TimCompilerEnv -> Int -> [Instruction] -> (Int, [Instruction])  -- Bスキーム  Mark3で変更
compileB (EAp (EAp (EVar op) e1) e2) env d cont  -- Mark3で変更
  = (d3, is2)  -- Mark3で変更
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
      -- (d2, is2) = compileB e2 env d1 is1  -- Mark3で変更 (これだと、e1とe2が必要とするスロットが、個別に領域確保される。)
      (d2, is2) = compileB e2 env d is1  -- Mark3で変更 (これだと、e1とe2が必要とするスロットが、共通に領域確保される。)
      d3 = max d1 d2
compileB (EAp (EVar "negate") e) env d cont  -- Mark3で変更
  = compileB e env d (Op Neg : cont)  -- Mark3で変更
compileB (ENum n) env d cont  -- Mark3で変更
  = (d, PushV (IntVConst n) : cont)
compileB e env d cont  -- Mark3で変更
  = (d_, Push (Code cont) : is)  -- Mark3で変更
    where (d_, is) = compileR e env d  -- Mark3で変更

-- Mark4で追加
compileU :: CoreExpr -> Int -> TimCompilerEnv -> Int -> (Int, TimAMode)
compileU (ENum n) u env d
  = (d, IntConst n)
compileU e u env d
  = (d_, Code (PushMarker u : is))
    where (d_, is) = compileR e env d

-- Mark5で追加
-- compileE :: (Int, [Name], CoreExpr) -> TimCompilerEnv -> Int -> (Int, (Int, [Instruction]))
compileE :: CoreAlt -> TimCompilerEnv -> Int -> (Int, (Int, [Instruction]))
compileE (t, args, expr) env d
  = (d_, (t, is_moves ++ is_body))
    where
      n = length args
      -- is_moves = map (\x -> Move (d + x) (Data x)) [1..n]  -- ここを変更する。もしも args のm番目の要素が、expr に EVar が付いた形で出現しなければ、Move (d + m) (Data m) を is_moves に含めないようにする。
      tempBoolList = checkFunc expr args  -- checkFunc は、args に含まれる各引数名 arg が expr に EVar arg の形で出現するかどうかを調べて True/False を並べたリストを返す関数。
      checkFunc e [] = []
      checkFunc e (x : xs) = case e of
                             EVar    v                 -> if v == x
                                                          then True  : checkFunc e xs
                                                          else False : checkFunc e xs
                             ENum    _                 -> False : checkFunc e xs
                             EConstr _     _           -> False : checkFunc e xs
                             EAp     f     a           -> ((head (checkFunc f [x])) || (head (checkFunc a [x]))) : checkFunc e xs
                             ELet    _     locals body -> if (not (x `elem` localvars)) && ((foldr (||) False tempBoolListELet) || (head (checkFunc body [x])))
                                                          then True  : checkFunc e xs
                                                          else False : checkFunc e xs
                                                          where
                                                            localvars = map fst locals
                                                            subFuncForELet defn = head $ checkFunc defn [x]
                                                            tempBoolListELet = map subFuncForELet $ map snd locals
                                                          -- x が locals に含まれる局所変数名と同じでなくて、局所変数の定義式か body に出現したら True。それ以外はFalse。
                             ECase   cond  alts        -> if (head (checkFunc cond [x])) || (foldr (||) False tempBoolListECase)
                                                          then True  : checkFunc e xs
                                                          else False : checkFunc e xs
                                                          where
                                                            subFuncForECase (_, args_, body_) = (not (x `elem` args_)) && (head (checkFunc body_ [x]))
                                                            tempBoolListECase = map subFuncForECase alts
                                                          -- x が cond に出現するか、x が alts に含まれる分岐先の引数名と同じでなくて、分岐先の定義式に出現したら True。それ以外はFalse。
                             ELam    _     _           -> error "Lambda expressions are not supported by TIM"
      is_moves = mkMovFunc $ zip2 tempBoolList [1..n] -- mkMovFunc は、tempBoolList の要素を見て、True だったら、そのインデックス(1始まり) m を使って、Move (d + m) (Data m) 命令を返す関数。
      mkMovFunc [] = []
      mkMovFunc ((x, m) : xs) | x == True = Move (d + m) (Data m) : mkMovFunc xs
                              | otherwise  = mkMovFunc xs
      -- Mark5で変更 (もしも expr が EVar <argsのm番目の要素> という式だったら、(d + n, [Enter (Data m)]) を返すようにする。)
      (d_, is_body) = case expr of
                      EVar var -> if var `elem` args
                                  then (d + n, [Enter (Data m)])
                                  else compileR expr env_ (d + n)
                                  where tempFunc x []       idx = idx
                                        tempFunc x (y : ys) idx | x == y = idx
                                                                | otherwise = tempFunc x ys (idx + 1)
                                        m = tempFunc var args 1
                      _ -> compileR expr env_ (d + n)
      env_  = (zip2 args (map Arg [(d+1)..(d+n)])) ++ env

-- Mark2で追加
mkIndMode :: Int -> TimAMode
mkIndMode n = Code [Enter (Arg n)]

-- Mark4で追加
mkUpdIndMode :: Int -> TimAMode
mkUpdIndMode n = Code [PushMarker n, Enter (Arg n)]

-- Mark4で追加
mkEnter :: TimAMode -> [Instruction]
mkEnter (Code i) = i
mkEnter other_am = [Enter other_am]
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
      (_, _, _, _, _, _, _, _, heap, _, _, _) = step state  -- Mark5で変更 (出力情報とデータフレームポインタを追加) Mark6で変更 (CodeStore型変更対応)
      -- next_state = (statUpdMaxstkdpth . statUpdAllcdclosure . statUpdAllcdheap . statUpdExectime . doAdmin) (step state)
      -- next_state' | hSize heap >= 1 = gc (step state)  -- gc有効化
      next_state' | hSize heap >= 1 = step state  -- gc無効化
                  | otherwise       = step state
      next_state = (statUpdMaxvstkdpth . statUpdMaxstkdpth . statUpdAllcdclosure . statUpdAllcdheap . statUpdExectime . doAdmin) next_state'

doAdmin :: TimState -> TimState
doAdmin state = applyToStats statIncSteps state

-- Mark5で変更 (出力情報とデータフレームポインタを追加) Mark6で変更 (CodeStore型変更対応)
timFinal (output, [], frame, dataframe, usedslot, stack, vstack, dump, heap, cstore, offsets, stats) = True
timFinal state                                                                                       = False

-- Mark5で変更 (出力情報とデータフレームポインタを追加) Mark6で変更 (CodeStore型変更対応)
applyToStats :: (TimStats -> TimStats) -> TimState -> TimState
applyToStats stats_fun (output, instr, frame, dataframe, usedslot, stack, vstack, dump, heap, cstore, offsets, stats)
  = (output, instr, frame, dataframe, usedslot, stack, vstack, dump, heap, cstore, offsets, stats_fun stats)

-- Mark5で変更 (出力情報とデータフレームポインタを追加) Mark6で変更 (CodeStore型変更対応)
step (output, (Take t n : instr), fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.x)  Mark3で変更
  | (t >= n) && (length stack >= n) = (output, instr, fptr', dfptr, usdsltnum_, drop n stack, vstack, dump, heap', cstore, offsets, stats)
  | otherwise                       = error "Too small alloc area or too few args for Take instruction"
  where (heap', fptr') = fAlloc heap tmpFrame
        tmpFrame = (take n stack) ++ (take (t - n) (repeat ([], FrameNull)))
        usdsltnum_ = getUsedSlotNumber instr
step (output, [Enter am], fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.6, 4.7, 4.8, 4.9)
  = (output, instr', fptr', dfptr, usdsltnum_, stack, vstack, dump, heap, cstore, offsets, stats)
    where (instr',fptr') = amToClosure am fptr dfptr heap cstore
          usdsltnum_ = case instr_ of
                       [] -> usdsltnum
                       Take _ _ : _ -> case fptr' of
                                       FrameAddr addr -> take n [1..]
                                                         where n = length cl
                                                               FClosure cl = hLookup heap addr
                                       _ -> []
                       _ -> getUsedSlotNumber instr_
          instr_ = fst (amToClosure am fptr dfptr heap cstore)
step (output, (Push am:instr), fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.2, 4.3, 4.4, 4.5, 4.21, 4.22)
  = (output, instr, fptr, dfptr, usdsltnum, amToClosure am fptr dfptr heap cstore : stack, vstack, dump, heap, cstore, offsets, stats)
step (output, [Return], fptr, dfptr, usdsltnum, [], n : vstack, (fptru, x, stack) : dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.16)  Mark4で追加
  = (output, [Return], fptr, dfptr, usdsltnum, stack, n : vstack, dump, newHeap, cstore, offsets, stats)
    where newHeap = fUpdate heap fptru x (intCode, FrameInt n)
step (output, [Return], fptr, dfptr, usdsltnum, (instr', fptr') : stack, vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.16)  Mark4で番号のみ変更
  = (output, instr', fptr', dfptr, usdsltnum_, stack, vstack, dump, heap, cstore, offsets, stats)
    where usdsltnum_ = case instr' of
                       [] -> usdsltnum
                       Take _ _ : _ -> case fptr of
                                       FrameAddr addr -> take n [1..]
                                                         where n = length cl
                                                               FClosure cl = hLookup heap addr
                                       _ -> []
                       othrs -> if isPushCodeInvolved othrs
                                then
                                  case fptr' of
                                  FrameAddr addr -> take n [1..]
                                                    where n = length cl
                                                          FClosure cl = hLookup heap addr
                                  _ -> []
                                else
                                  getUsedSlotNumber instr'
                                where
                                  isPushCodeInvolved [] = False
                                  isPushCodeInvolved (oth_ : othrs_) = case oth_ of
                                                                       Push (Code _) -> True
                                                                       _ -> isPushCodeInvolved othrs_
                                {-
                                  instr_ に Push (Code _) が含まれていたら、fptr' が指すフレームに含まれる全スロットを usdsltnum_ にする。
                                  そうでなかったら、getUsedSlotNumber instr_ の戻り値を usdsltnum_ にする。
                                -}
step (output, (PushV (IntVConst n) : instr), fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.14)
  = (output, instr, fptr, dfptr, usdsltnum, stack, n : vstack, dump, heap, cstore, offsets, stats)
step (output, (PushV FramePtr : instr), (FrameInt n), dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.12)
  = (output, instr, (FrameInt n), dfptr, usdsltnum, stack, n : vstack, dump, heap, cstore, offsets, stats)
step (output, (Op Neg : instr), fptr, dfptr, usdsltnum, stack, n : vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.10)
  = (output, instr, fptr, dfptr, usdsltnum, stack, -n : vstack, dump, heap, cstore, offsets, stats)
step (output, (Op op : instr), fptr, dfptr, usdsltnum, stack, n1 : n2 : vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.10)
  = (output, instr, fptr, dfptr, usdsltnum, stack, result : vstack, dump, heap, cstore, offsets, stats)
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
step (output, [Cond i1 i2], fptr, dfptr, usdsltnum, stack, n : vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.13)
  = (output, i, fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)
    where i | n == 0    = i1
            | otherwise = i2
step (output, (Move i (Data n) : instr), fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.x)  Mark5で追加する必要ありそう。
  = (output, instr, fptr, dfptr, usdsltnum, stack, vstack, dump, newHeap, cstore, offsets, stats)
    where
      curFrameSize = case fptr of
                     FrameAddr addr -> length f
                                       where
                                         FClosure f = hLookup heap addr
                     _ -> 0
      curDataFrameSize = case dfptr of
                         FrameAddr addr -> length df
                                           where
                                             FClosure df = hLookup heap addr
                         _ -> 0
      newHeap | i <= curFrameSize && n <= curDataFrameSize = fUpdate heap fptr i (fGet heap dfptr n)
              | otherwise = error ("Move instruction argument slot number " ++ show i ++ " or data slot number " ++ show n ++
                                   " exceeds frame size " ++ show curFrameSize ++ " or data frame size " ++ show curDataFrameSize)
step (output, (Move i a : instr), fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.x)  Mark3で追加
  = (output, instr, fptr, dfptr, usdsltnum, stack, vstack, dump, newHeap, cstore, offsets, stats)
    where
      curFrameSize = case fptr of
                     FrameAddr addr -> length f
                                       where
                                         FClosure f = hLookup heap addr
                     _ -> 0
      -- newHeap | i <= curFrameSize = fUpdate heap fptr i (amToClosure a fptr heap cstore)
      newHeap | i <= curFrameSize = fUpdate heap fptr i (amToClosure a fptr dfptr heap cstore)
              | otherwise = error ("Move instruction argument slot number " ++ show i ++ " exceeds frame size " ++ show curFrameSize)
step (output, (PushMarker x : instr), fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.16)  Mark4で追加
  = (output, instr, fptr, dfptr, usdsltnum, [], vstack, (fptr, x, stack) : dump, heap, cstore, offsets, stats)
step (output, (UpdateMarkers n : instr), fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.17)  Mark4で追加
  | length stack >= n = (output, instr, fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)
  | otherwise         = (output, (UpdateMarkers n : instr), fptr, dfptr, usdsltnum, newStack, vstack, newDump, newHeap, cstore, offsets, stats)
                        where
                          (fptru, x, stack_) : newDump = dump
                          newStack                     = stack ++ stack_
                          newHeap                      = fUpdate heap fptru x (instr_, FrameAddr fptr_)
                          instr_                       = foldr (:) (UpdateMarkers n : instr) (map Push (map Arg (reverse [1..(length stack)])))
                          (_, (fptr_ : _), _)          = heap
step (output, [Switch brchs], fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.18)  Mark5で追加
  = (output, instr, fptr, dfptr, usdsltnum, stack, vstack_, dump, heap, cstore, offsets, stats)
    where
      t | length vstack > 0 = head vstack
        | otherwise         = error "vstack is empty."
      vstack_ = tail vstack
      tmpList = [instr_ | (t_, instr_) <- brchs, t_ == t]
      instr | length tmpList == 1 = head tmpList
            | otherwise           = error "Switch instrustion error."
step (output, [ReturnConstr t], fptr, dfptr, usdsltnum, (instr', fptr') : stack, vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.19)  Mark5で追加
  -- = (output, newInstr, fptr', fptr, usdsltnum_, stack, t : vstack, dump, heap, cstore, stats)
  = (output, newInstr, fptr', fptr, usdsltnum_, stack, t : vstack, dump, newHeap, cstore, offsets, stats)
    where usdsltnum_ = case instr' of
                       [] -> usdsltnum
                       Take _ _ : _ -> case fptr of
                                       FrameAddr addr -> take n [1..]
                                                         where n = length cl
                                                               FClosure cl = hLookup heap addr
                                       _ -> []
                       othrs -> if isPushCodeInvolved othrs
                                then
                                  case fptr' of
                                  FrameAddr addr -> take n [1..]
                                                    where n = length cl
                                                          FClosure cl = hLookup heap addr
                                  _ -> []
                                else
                                  getUsedSlotNumber instr'
                                where
                                  isPushCodeInvolved [] = False
                                  isPushCodeInvolved (oth_ : othrs_) = case oth_ of
                                                                       Push (Code _) -> True
                                                                       _ -> isPushCodeInvolved othrs_
                                {-
                                  instr_ に Push (Code _) が含まれていたら、fptr' が指すフレームに含まれる全スロットを usdsltnum_ にする。
                                  そうでなかったら、getUsedSlotNumber instr_ の戻り値を usdsltnum_ にする。
                                -}
          newInstr | length instr' == 0 && length stack == 0 = [Enter (Label "topCont" 1)]  -- Mark6で変更
                   | otherwise                               = instr'
          newHeap | length instr' == 0 && length stack == 0 = newHeap4
                  | otherwise                               = heap
                  where
                    (newHeap2, addr2) = hAlloc heap (FClosure [([], FrameNull), ([], FrameNull)])
                    -- (fg, g) = cstore
                    fg = cstore  -- Mark6で変更
                    (instrTopCont, _) = amToClosure (Label "topCont" 1) fptr dfptr heap cstore  -- Mark6で変更
                    -- newHeap3 = fUpdate newHeap2 (FrameAddr fg) (codeLookup g "topCont") (instrTopCont, (FrameAddr addr2))
                    newHeap3 = fUpdate newHeap2 (FrameAddr fg) 1 (instrTopCont, (FrameAddr addr2))  -- Mark6で変更
                    (instrHeadCont, _) = amToClosure (Label "headCont" 2) fptr dfptr newHeap3 cstore  -- Mark6で変更
                    -- newHeap4 = fUpdate newHeap3 (FrameAddr fg) (codeLookup g "headCont") (instrHeadCont, (FrameAddr addr2))
                    newHeap4 = fUpdate newHeap3 (FrameAddr fg) 2 (instrHeadCont, (FrameAddr addr2))  -- Mark6で変更
step (output, [ReturnConstr t], fptr, dfptr, usdsltnum, [], vstack, (fptru, x, stack) : dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.20)  Mark5で追加
  = (output, [ReturnConstr t], fptr, dfptr, usdsltnum, stack, vstack, dump, newHeap, cstore, offsets, stats)
    where newHeap = fUpdate heap fptru x ([ReturnConstr t], fptr)
step (output, (Print : instr), fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)  -- 遷移規則 (4.x)  Mark5で追加
  = (newOutput, instr, fptr, dfptr, usdsltnum, stack, newVstack, dump, heap, cstore, offsets, stats)
    where
      newVstack | length vstack > 0 = tail vstack
                | otherwise         = error "Print : vstack is empty."
      newOutput | length vstack > 0 = output ++ show(head vstack) ++ " "
                | otherwise         = error "Print : vstack is empty."

amToClosure :: TimAMode -> FramePtr -> FramePtr -> TimHeap -> CodeStore -> Closure
amToClosure (Arg n)      fptr dfptr heap cstore = fGet heap fptr n             -- 遷移規則 (4.2, 4.7)
amToClosure (Code il)    fptr dfptr heap cstore = (il, fptr)                   -- 遷移規則 (4.4, 4.8)
amToClosure (Label l k)  fptr dfptr heap cstore = fGet heap (FrameAddr cstore) k   -- 遷移規則 (4.3, 4.6, 4.21, 4.22)
amToClosure (IntConst n) fptr dfptr heap cstore = (intCode, FrameInt n)        -- 遷移規則 (4.5, 4.9)
amToClosure (Data n)     fptr dfptr heap cstore = fGet heap dfptr n            -- 遷移規則 (4.x)

intCode = [PushV FramePtr, Return]  -- Mark2で変更

-- Mark5で変更 (出力情報とデータフレームポインタを追加) Mark6で変更 (CodeStore型変更対応)
gc :: TimState -> TimState
gc (output, instr, fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)
  = (output, instr, fptr, dfptr, usdsltnum, stack, vstack, dump, newHeap, cstore, offsets, stats)
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
              _           -> []
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
          -- Enter (Code il__) -> getUsedSlotNumber il__
          Cond ilthn ilels -> (getUsedSlotNumber ilthn) ++ (getUsedSlotNumber ilels)
          Move n (Code il_) -> [n] ++ getUsedSlotNumber il_
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
      iStr "Output: \n", iStr outputs, iNewline, iNewline,
      showState last_state, iNewline, iNewline, showStats last_state
    ])
    where last_state = last states
          tempList = [output | (output, _, _, _, _, _, _, _, _, _, _, _) <- states]
          subFuncForShowResults [] = []
          subFuncForShowResults [x] = []
          subFuncForShowResults (x1 : x2 : xs) | length x1 < length x2 = " " ++ (drop (length x1) x2) ++ "\n" ++ subFuncForShowResults (x2 : xs)
                                               | otherwise             = "." ++ subFuncForShowResults (x2 : xs)
          outputs = subFuncForShowResults tempList

-- Mark5で変更 (出力情報とデータフレームポインタを追加) Mark6で変更 (CodeStore型変更対応)
showSCDefns :: TimState -> Iseq
showSCDefns (output, instr, fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)
  = iInterleave iNewline (map showSC xs)
    where fg = FrameAddr cstore
          xs = map subFuncForShowSCDefns offsets
          subFuncForShowSCDefns (name, index) = (name, fst $ fGet heap fg index)

showSC :: (Name, [Instruction]) -> Iseq
showSC (name, il)
  = iConcat [
      iStr "Code for ", iStr name, iStr ":", iNewline,
      iStr " ", showInstructions Full il, iNewline, -- iNewline
      iStr "Used slot number for ", iStr name, iStr ":", iNewline,
      iStr " ", showUsedSlotNumber il, iNewline,
      iNewline
    ]

-- Mark5で変更 (出力情報とデータフレームポインタを追加) Mark6で変更 (CodeStore型変更対応)
showState :: TimState -> Iseq
showState (output, instr, fptr, dfptr, usdsltnum, stack, vstack, dump, heap, cstore, offsets, stats)
  = iConcat [
      iStr "Code: ", showInstructions Terse instr, iNewline,
      showFrame heap fptr,
      iStr "A data frame ",  -- Mark5で追加
      showFrame heap dfptr,  -- Mark5で追加
      showStack stack,
      showValueStack vstack,
      showDump dump,
      showUsedHeap heap,  -- GCデバッグ用
      showUsdSltNmbrs usdsltnum, iNewline,
      showOutput output, iNewline  -- Mark5で追加
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
showValueStack vstack
  -- Mark2で変更
  = iConcat [ iStr "Value stack: [",
      iIndent (iInterleave iNewline (map iNum vstack)),
      iStr "]", iNewline
    ]

showDump :: TimDump -> Iseq
showDump dump
  = iConcat [iStr "Dump:[",
             iIndent (iInterleave iNewline
                     (map showDumpItem dump)),
             iStr "]", iNewline]  -- Mark4で変更

-- Mark4で追加
showDumpItem :: (FramePtr, Int, TimStack) -> Iseq
showDumpItem (fptr, x, stack)
  = iConcat [iStr "<",
             showFramePtr fptr, iStr ", ",
             iNum x, iStr ", ",
             iStr "[", iIndent (iInterleave iNewline (map showClosure stack)), iStr "]",
             iStr ">"]

showClosure :: Closure -> Iseq
showClosure (i,f)
  = iConcat [ iStr "(", showInstructions Terse i, iStr ", ",
      showFramePtr f, iStr ")"
    ]

showFramePtr :: FramePtr -> Iseq
showFramePtr FrameNull     = iStr "null"
showFramePtr (FrameAddr a) = iStr (show a)
showFramePtr (FrameInt n)  = iStr "int " `iAppend` iNum n

-- Mark5で変更 (出力情報とデータフレームポインタを追加) Mark6で変更 (CodeStore型変更対応)
showStats :: TimState -> Iseq
showStats (output, instr, fptr, dfptr, usdsltnum, stack, vstack, dump, heap, code, offsets, stats)
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
            , iStr "Maximum vstack depth = ", iNum (statGetMaxvstkdpth stats),
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
showInstruction d (PushMarker x) = (iStr "PushMarker ") `iAppend` (iNum x)  -- Mark4で追加
showInstruction d (UpdateMarkers n) = (iStr "UpdateMarkers ") `iAppend` (iNum n)  -- Mark4で追加
showInstruction d (Switch brchs)
  = iStr "Switch" `iAppend` (subFuncForSwitch brchs)  -- Mark5で追加
    where
      subFuncForSwitch [] = iNil
      subFuncForSwitch ((n, brch) : brchs) = (iStr " (") `iAppend` (iNum n) `iAppend` (iStr ", ") `iAppend`
                                             (showInstructions Terse brch) `iAppend` (iStr ")") `iAppend`
                                             (subFuncForSwitch brchs)
showInstruction d (ReturnConstr t) = (iStr "ReturnConstr ") `iAppend` (iNum t)  -- Mark5で追加
showInstruction d Print        = iStr "Print"                                   -- Mark5で追加

showArg d (Arg m)      = (iStr "Arg ")      `iAppend` (iNum m)
showArg d (Code il)    = (iStr "Code ")     `iAppend` (showInstructions d il)
showArg d (Label s k)  = (iStr "Label ")    `iAppend` (iStr s) `iAppend` (iStr " ") `iAppend` (iNum k)  -- Mark6で変更
showArg d (IntConst n) = (iStr "IntConst ") `iAppend` (iNum n)
showArg d (Data n)     = (iStr "Data ")     `iAppend` (iNum n)  -- Mark5で追加

nTerse = 3

showCompiledCode :: String -> String
showCompiledCode coreprg
  = iDisplay (iConcat [
      iStr "Supercombinator definitions", iNewline, iNewline,
      showSCDefns first_state, iNewline
    ])
    where
      first_state = compile $ parse coreprg

showUsedSlotNumber :: [Instruction] -> Iseq
showUsedSlotNumber []
  = iConcat [iStr "[", iStr "]"]
showUsedSlotNumber il
  = iConcat [iStr "[", (iInterleave (iStr ", ") (map iNum (getUsedSlotNumber il))), iStr "]"]

showOutput :: TimOutput -> Iseq
showOutput output
  = iConcat [iStr "Output: [ ", iStr output, iStr "]"]
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
-- letとletrecのテスト --
ex_2_11 = "pair x y f = f x y ; " ++ 
          "fst p = p K ; " ++
          "snd p = p K1 ; " ++
          "f x y = letrec a = pair x b ; " ++
                         "b = pair y a " ++
                  "in fst (snd (snd (snd a))) ; "  ++
          "main = f 3 4"
{-
  f x y
= fst (snd (snd (snd a)))
= fst (snd (snd (a K1)))
= fst (snd ((a K1) K1))
= fst (((a K1) K1) K1)
= (((a K1) K1) K1) K
= ((((pair x b) K1) K1) K1) K
= (((K1 x b) K1) K1) K
= ((b K1) K1) K
= (((pair y a) K1) K1) K
= ((K1 y a) K1) K
= (a K1) K
= ((pair x b) K1) K
= (K1 x b) K
= b K
= (pair y a) K
= K y a
= y
↓
  f 3 4
= 4
-}
ex_2_11' = "pair x y f = f x y ; " ++ 
           "fst p = p K ; " ++
           "snd p = p K1 ; " ++
           "f x y = letrec a = pair x b ; " ++
                          "b = pair y a " ++
                   "in fst (snd (snd (snd a))) ; "  ++
           "main = f (fst (pair 3 4)) (snd (pair 3 4))"
b_2_1 = "main = let id1 = I I I " ++
               "in id1 id1 3"
b_2_2 = "oct g x = let h = twice g " ++ -- gの後のスペースは必要(gとinの間にスペースを入れる必要があるため)
                  "in let k = twice h " ++ -- hの後のスペースは必要(hとinの間にスペースを入れる必要があるため)
                  "in k (k x) ;" ++
        "main = oct I 4"
b_2_2' = "oct g x = let h = twice g " ++ -- gの後のスペースは必要(gとinの間にスペースを入れる必要があるため)
                   "in let k = twice h " ++ -- hの後のスペースは必要(hとinの間にスペースを入れる必要があるため)
                   "in k (k x) ;" ++
         "inc x = x + 1 ; " ++
         "main = oct inc 4"
b_2_3 = "cons a b cc cn = cc a b ;" ++
        "nil cc cn = cn ;" ++
        "hd list = list K abort ;" ++
        "tl list = list K1 abort ;" ++
        "abort = abort ;" ++
        "infinite x = letrec xs = cons x xs " ++  -- xsの後のスペースは必要(xsとinの間にスペースを入れる必要があるため)
                     "in xs ;" ++
        "main = hd (tl (tl (infinite 4)))"
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
ex_4_15_3' = "main = (let x = 1 ; y = 2 in x + y) + (let x = 2 ; y = 1 in x - y)"
-- ex_4_15_3'' = "main = (let x = 1 ; y = 2 ; z = x + y in z) + (let x = 2 ; y = 1 ; z = x - y in z)"
ex_4_15_3'' = "main = (letrec x = 1 ; y = 2 ; z = x + y in z) + (letrec x = 2 ; y = 1 ; z = x - y in z)"
test_program_for_let1 = "main = let x = 1 " ++
                               "in " ++
                               "  let y = 2 " ++
                               "  in x + 2"
test_program_for_let1' = "main = let x = 1 " ++
                                "in " ++
                                "  let y = 2 " ++
                                "  in x + y"
-- 更新のテスト --
ex_4_16 = "f x = x + x ; " ++
          "main = f (1+2)"
ex_4_17 = "compose2 f g x = f (g x)"
ssc_4_5_4_1 = "g x y = x + y ; " ++
              "f x = g x x ; " ++
              "main = f (1+2)"
ssc_4_5_4_2 = "g x = x + x ; " ++
              "f y = g y ; " ++
              "main = f (1+2)"
ex_4_18_1 = "g x y = x + y ; " ++
            "f x = let y = x * 2 in g y y ; "  ++
            "main = f 2"
ex_4_18_2 = "g x y z = (x + y) + z ; " ++
            "f x = let y = x * 2 in g y y y ; "  ++
            "main = f 2"
ex_4_19 = "main = let x = 3 in x+x"
ex_4_20 = "factorial n = if n 1 (n * factorial (n-1)) ; " ++
          "f x = x + x ; " ++
          "main = let arg = factorial 3 in f arg"

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

ex_4_20_2 = "f x = (x + x) + x ; " ++
            "main = f (3 + 5)"
ex_4_20_3 = "factorial n = if n 1 (n * factorial (n-1)) ; " ++
            "f x = ((x + x) + x) + x ; " ++
            "main = f (factorial 3)"
ex_4_20_4 = "f x = g x ; " ++
            "g x = h x ; " ++
            "h x = i x ; " ++
            "i x = j x ; " ++
            "j x = k x ; " ++
            "k x = l x ; " ++
            "l x = x + x ; " ++
            "main = f (3 + 5)"

ex_4_21_1 = "pair x y f = f x y ; " ++
            "fst p = p K ; " ++
            "snd p = p K1 ; " ++
            "main = let w = pair 2 3 " ++
                   "in (fst w) * (snd w)"
ex_4_21_2 = "pair x y f = f x y ; " ++
            "fst p = p K ; " ++
            "snd p = p K1 ; " ++
            "main = let w = if(2 * 3 > 4) (pair 2 3) (pair 3 2) " ++
                   "in (fst w) * (snd w)"
ex_4_21_3 = "pair x y f = f x y ; " ++
            "fst p = p K ; " ++
            "snd p = p K1 ; " ++
            "main = let w = if(2 * 3 > 8) (pair 2 3) (pair 3 2) " ++
                   "in (fst w) * (snd w)"
ex_4_21_4 = "add a b = a+b ; " ++
            "twice2 f x = f (f x) ; " ++
            "g x = add (x*x) ; " ++
            "main = twice2 (g 3) 4"
ex_4_21_4_ = "add a b = a+b ; " ++
             "g x = add (x*x) ; " ++
             "main = twice (g 3) 4"
-- 構造化データのテスト
ex_4_23_00 = "cons  = Pack{2, 2} ; " ++
             "nil   = Pack{1, 0} ; " ++
             "length xs = case xs of " ++
                             "<1> -> 0 ; " ++
                             "<2> p ps -> 1 + length ps ; " ++
             "main = length nil"

ex_4_23_01 = "cons  = Pack{2, 2} ; " ++
             "nil   = Pack{1, 0} ; " ++
             "length xs = case xs of " ++
                             "<1> -> 0 ; " ++
                             "<2> p ps -> 1 + length ps ; " ++
             "main = length (cons 1 nil)"

ex_4_23_1 = "cons  = Pack{2, 2} ; " ++
            "nil   = Pack{1, 0} ; " ++
            "true  = Pack{2, 0} ; " ++
            "false = Pack{1, 0} ; " ++
            "if cond tbranch fbranch = case cond of " ++
                                            "<1> -> fbranch ; " ++
                                            "<2> -> tbranch ; " ++
            "length xs = case xs of " ++
                            "<1> -> 0 ; " ++
                            "<2> p ps -> 1 + length ps ; " ++
            "main = length (cons 1 (cons 2 nil))"

ex_4_23_1_ = "cons  = Pack{2, 2} ; " ++
             "nil   = Pack{1, 0} ; " ++
             "true  = Pack{2, 0} ; " ++
             "false = Pack{1, 0} ; " ++
             "if cond tbranch fbranch = case cond of " ++
                                             "<1> -> fbranch ; " ++
                                             "<2> -> tbranch ; " ++
             "length xs = case xs of " ++
                             "<1> -> 0 ; " ++
                             "<2> p ps -> 1 + length ps ; " ++
             "main = length (cons 1 (cons 2 (cons 3 nil)))"

ex_4_23_2 = "cons  = Pack{2, 2} ; " ++
            "nil   = Pack{1, 0} ; " ++
            "true  = Pack{2, 0} ; " ++
            "false = Pack{1, 0} ; " ++
            "if cond tbranch fbranch = case cond of " ++
                                            "<1> -> fbranch ; " ++
                                            "<2> -> tbranch ; " ++
            "length xs = case xs of " ++
                            "<1> -> 0 ; " ++
                            "<2> p ps -> 1 + length ps ; " ++
            "append xs ys = case xs of " ++
                             "<1> -> ys ; " ++
                             "<2> p ps -> cons p (append ps ys) ; " ++
            "main = let xs = append (cons 1 nil) (cons 2 nil) " ++
                   "in " ++
                   "length xs + length xs"

ex_4_23_2_ = "cons  = Pack{2, 2} ; " ++
             "nil   = Pack{1, 0} ; " ++
             "true  = Pack{2, 0} ; " ++
             "false = Pack{1, 0} ; " ++
             "if cond tbranch fbranch = case cond of " ++
                                             "<1> -> fbranch ; " ++
                                             "<2> -> tbranch ; " ++
             "length xs = case xs of " ++
                             "<1> -> 0 ; " ++
                             "<2> p ps -> 1 + length ps ; " ++
             "append xs ys = case xs of " ++
                              "<1> -> ys ; " ++
                              "<2> p ps -> cons p (append ps ys) ; " ++
             "main = let xs = append (cons 1 (cons 3 nil)) (cons 2 (cons 3 (cons 4 nil))) " ++
                    "in " ++
                    "length xs + length xs"

ex_4_23_2__ = "cons  = Pack{2, 2} ; " ++
              "nil   = Pack{1, 0} ; " ++
              "true  = Pack{2, 0} ; " ++
              "false = Pack{1, 0} ; " ++
              "if cond tbranch fbranch = case cond of " ++
                                              "<1> -> fbranch ; " ++
                                              "<2> -> tbranch ; " ++
              "length xs = case xs of " ++
                              "<1> -> 0 ; " ++
                              "<2> p ps -> 1 + length ps ; " ++
              "append xs ys = case xs of " ++
                               "<1> -> ys ; " ++
                               "<2> p ps -> cons p (append ps ys) ; " ++
              "main = let xs = append nil nil " ++
                     "in " ++
                     "length xs + length xs"

ex_4_25 = "cons  = Pack{2, 2} ; " ++
          "nil   = Pack{1, 0} ; " ++
          "true  = Pack{2, 0} ; " ++
          "false = Pack{1, 0} ; " ++
          "if cond tbranch fbranch = case cond of " ++
                                          "<1> -> fbranch ; " ++
                                          "<2> -> tbranch ; " ++
          "between n m = if (n > m) nil (cons n (between (n + 1) m)) ; " ++
          "main = between 1 4"
{-
  main = between 1 4
       = if (1 > 4) nil (cons 1 (between (1 + 1) 4))
       = cons 1 (between 2 4)
       = cons 1 (if (2 > 4) nil (cons 2 (between (2 + 1) 4)))
       = cons 1 (cons 2 (between 3 4))
       = cons 1 (cons 2 (if (3 > 4) nil (cons 3 (between (3 + 1) 4))))
       = cons 1 (cons 2 (cons 3 (between 4 4)))
       = cons 1 (cons 2 (cons 3 (if (4 > 4) nil (cons 4 (between (4 + 1) 4)))))
       = cons 1 (cons 2 (cons 3 (cons 4 (between 5 4))))
       = cons 1 (cons 2 (cons 3 (cons 4 (if (5 > 4) nil (cons 5 (between (5 + 1) 4))))))
       = cons 1 (cons 2 (cons 3 (cons 4 nil)))
-}

ex_4_25_0 = "cons  = Pack{2, 2} ; " ++
            "nil   = Pack{1, 0} ; " ++
            "true  = Pack{2, 0} ; " ++
            "false = Pack{1, 0} ; " ++
            "if cond tbranch fbranch = case cond of " ++
                                            "<1> -> fbranch ; " ++
                                            "<2> -> tbranch ; " ++
            "between n m = if (n > m) nil (cons n (between (n + 1) m)) ; " ++
            "main = between 1 0"
{-
  main = between 1 0
       = if (1 > 0) nil (cons 1 (between (1 + 1) 0))
       = nil
-}

ex_4_25_1 = "cons  = Pack{2, 2} ; " ++
            "nil   = Pack{1, 0} ; " ++
            "true  = Pack{2, 0} ; " ++
            "false = Pack{1, 0} ; " ++
            "if cond tbranch fbranch = case cond of " ++
                                            "<1> -> fbranch ; " ++
                                            "<2> -> tbranch ; " ++
            "between n m = if (n > m) nil (cons n (between (n + 1) m)) ; " ++
            "main = between 1 1"
{-
  main = between 1 1
       = if (1 > 1) nil (cons 1 (between (1 + 1) 1))
       = cons 1 (between 2 1)
       = cons 1 (if (2 > 1) nil (cons 2 (between (2 + 1) 1)))
       = cons 1 nil
-}

ex_4_25_2 = "cons  = Pack{2, 2} ; " ++
            "nil   = Pack{1, 0} ; " ++
            "true  = Pack{2, 0} ; " ++
            "false = Pack{1, 0} ; " ++
            "if cond tbranch fbranch = case cond of " ++
                                            "<1> -> fbranch ; " ++
                                            "<2> -> tbranch ; " ++
            "between n m = if (n > m) nil (cons n (between (n + 1) m)) ; " ++
            "main = between 1 2"
{-
  main = between 1 2
       = if (1 > 2) nil (cons 1 (between (1 + 1) 2))
       = cons 1 (between 2 2)
       = cons 1 (if (2 > 2) nil (cons 2 (between (2 + 1) 2)))
       = cons 1 (cons 2 (between 3 2))
       = cons 1 (cons 2 (if (3 > 2) nil (cons 3 (between (3 + 1) 2))))
       = cons 1 (cons 2 nil)
-}

ex_4_25_3 = "cons  = Pack{2, 2} ; " ++
            "nil   = Pack{1, 0} ; " ++
            "true  = Pack{2, 0} ; " ++
            "false = Pack{1, 0} ; " ++
            "if cond tbranch fbranch = case cond of " ++
                                            "<1> -> fbranch ; " ++
                                            "<2> -> tbranch ; " ++
            "between n m = if (n > m) nil (cons n (between (n + 1) m)) ; " ++
            "main = between 1 3"
{-
  main = between 1 3
       = if (1 > 3) nil (cons 1 (between (1 + 1) 3))
       = cons 1 (between 2 3)
       = cons 1 (if (2 > 3) nil (cons 2 (between (2 + 1) 3)))
       = cons 1 (cons 2 (between 3 3))
       = cons 1 (cons 2 (if (3 > 3) nil (cons 3 (between (3 + 1) 3))))
       = cons 1 (cons 2 (cons 3 (between 4 3)))
       = cons 1 (cons 2 (cons 3 (if (4 > 3) nil (cons 4 (between (4 + 1) 3)))))
       = cons 1 (cons 2 (cons 3 nil))
-}

ex_4_25_100 = "cons  = Pack{2, 2} ; " ++
              "nil   = Pack{1, 0} ; " ++
              "true  = Pack{2, 0} ; " ++
              "false = Pack{1, 0} ; " ++
              "if cond tbranch fbranch = case cond of " ++
                                              "<1> -> fbranch ; " ++
                                              "<2> -> tbranch ; " ++
              "between n m = if (n > m) nil (cons n (between (n + 1) m)) ; " ++
              "main = between 1 100"

ex_4_25_1000 = "cons  = Pack{2, 2} ; " ++
               "nil   = Pack{1, 0} ; " ++
               "true  = Pack{2, 0} ; " ++
               "false = Pack{1, 0} ; " ++
               "if cond tbranch fbranch = case cond of " ++
                                               "<1> -> fbranch ; " ++
                                               "<2> -> tbranch ; " ++
               "between n m = if (n > m) nil (cons n (between (n + 1) m)) ; " ++
               "main = between 1 1000"

ssc_4_6_5 = "cons  = Pack{2, 2} ; " ++
            "nil   = Pack{1, 0} ; " ++
            "tempList = cons 1 (cons 2 (cons 3 nil)) ; " ++
            "head xs = case xs of " ++
            -- "               <1>      -> error ; " ++
            "               <1>      -> 999 ; " ++
            "               <2> y ys -> y ; " ++
            "main = head tempList"

ssc_4_6_5_ = "cons  = Pack{2, 2} ; " ++
             "nil   = Pack{1, 0} ; " ++
             "tempList = cons 1 (cons 2 nil) ; " ++
             "sum xs = case xs of " ++
             "                <1> -> 0 ; " ++
             "                <2> y ys -> y + sum ys ; " ++
             "main = sum tempList"
ssc_4_6_5_EAp = "cons  = Pack{2, 2} ; " ++
                "nil   = Pack{1, 0} ; " ++
                "tempList = cons 1 (cons 2 (cons 3 nil)) ; " ++
                "head_ xs = case xs of " ++
                -- "               <1>      -> error ; " ++
                "                <1>      -> 999 ; " ++
                "                <2> y ys -> y + 2 ; " ++
                "main = head_ tempList"
{-
  (
    \"head_\",
    [
      UpdateMarkers 1,
      Take 3 1,
      Push (Code [Switch [(1, [PushV (IntVConst 999),
                               Return
                              ]
                          ),
                          (2, [Move 2 (Data 1),
                               PushV (IntVConst 2),
                               Push (Code [Op Add,
                                           Return
                                          ]
                                    ),
                               Enter (Arg 2)
                              ]
                          )
                         ]
                 ]
           ),
      PushMarker 1,
      Enter (Arg 1)
    ]
  )
  -- case の分岐 <2> の引数 ys のために、Data 2 を Move する命令は生成されていない模様。
-}
ssc_4_6_5_ELet = "cons  = Pack{2, 2} ; " ++
                 "nil   = Pack{1, 0} ; " ++
                 "tempList = cons 1 (cons 2 (cons 3 nil)) ; " ++
                 "head_ xs = case xs of " ++
                 -- "               <1>      -> error ; " ++
                 "                <1>      -> 999 ; " ++
                 "                <2> y ys -> let y = 2 in y + 2 ; " ++
                 "main = head_ tempList"
{-
  (
    \"head_\",
    [
      UpdateMarkers 1,
      Take 4 1,
      Push (Code [Switch [(1, [PushV (IntVConst 999),
                               Return
                              ]
                          ),
                          (2, [Move 4 (IntConst 2),  -- 局所変数 y に代入された 2
                               PushV (IntVConst 2),  -- 本体部分で y に加算される 2
                               Push (Code [Op Add,
                                           Return
                                          ]
                                    ),
                               Enter (Arg 4)
                              ]
                          )
                         ]
                 ]
           ),
      PushMarker 1,
      Enter (Arg 1)
    ]
  )
  -- case の分岐 <2> の引数 y, ys のために、Data 1, Data 2 を Move する命令は生成されていない模様。
-}
ssc_4_6_5_EConstr = "cons  = Pack{2, 2} ; " ++
                    "nil   = Pack{1, 0} ; " ++
                    "tempList = cons 1 (cons 2 (cons 3 nil)) ; " ++
                    "head_ xs = case xs of " ++
                    -- "               <1>      -> error ; " ++
                    "                <1>      -> 999 ; " ++
                    "                <2> y ys -> nil ; " ++
                    "main = head_ tempList"
{-
  (
    \"head_\",
    [
      UpdateMarkers 1,
      Take 3 1,
      Push (Code [Switch [(1, [PushV (IntVConst 999),
                               Return
                              ]
                          ),
                          (2, [Enter (Label \"nil\")])
                         ]
                 ]
           ),
      PushMarker 1,
      Enter (Arg 1)
    ]
  )
  -- case の分岐 <2> の引数 y, ys のために、Data 1, Data 2 を Move する命令は生成されていない模様。
-}
ssc_4_6_5_ECase = "cons  = Pack{2, 2} ; " ++
                  "nil   = Pack{1, 0} ; " ++
                  "tempList = cons 1 (cons 2 (cons 3 nil)) ; " ++
                  "head_ xs = case xs of " ++
                  -- "               <1>      -> error ; " ++
                  "                <1>      -> 999 ; " ++
                  "                <2> y ys -> case y of " ++
                  "                                 <1> -> y + 1 ; " ++
                  "                                 <2> -> y + 2 ; " ++
                  "main = head_ tempList"
{-
    (
        \"head_\",
        [
            UpdateMarkers 1,
            Take 3 1,
            Push (Code [Switch [(1, [PushV (IntVConst 999),
                                     Return
                                    ]
                                ),
                                (2, [Move 2 (Data 1),
                                     Push (Code [Switch [(1, [PushV (IntVConst 1),
                                                              Push (Code [Op Add,Return]),
                                                              Enter (Arg 2)
                                                             ]
                                                         ), 
                                                         (2, [PushV (IntVConst 2),
                                                              Push (Code [Op Add,
                                                                          Return
                                                                         ]
                                                                   ),
                                                              Enter (Arg 2)
                                                             ]
                                                         )
                                                        ]
                                                ]
                                          ),
                                     Enter (Arg 2)
                                    ]
                                )
                               ]
                       ]
                 ),
            PushMarker 1,
            Enter (Arg 1)
        ]
    )
  -- case の分岐 <2> の引数 ys のために、Data 2 を Move する命令は生成されていない模様。
-}

ex_4_28_1 = "fac n = if (n==0) 1 (n * fac (n-1)) ;" ++
            "x = fac 3 ; " ++
            "y = x + 2 ; " ++
            "z = x - 3 ; " ++
            "main = y * z"

ex_4_28_2 = "fac n = if (n==0) 1 (n * fac (n-1)) ;" ++
            "f x = fac x ; " ++
            "y = f 3 + 2 ; " ++
            "z = f 3 - 3 ; " ++
            "main = y * z"

ex_4_28_3 = "fac n = if (n==0) 1 (n * fac (n-1)) ;" ++
            "f x = fac x ; " ++
            "x = f 3 + 2 ; " ++  -- x = 3! + 2 =  6 + 2 =  8
            "y = f 4 + 3 ; " ++  -- y = 4! + 3 = 24 + 3 = 27
            "z = f 3 - 1 ; " ++  -- z = 3! - 1 =  6 - 1 =  5
            "main = x * z - y"   -- main = 8 * 5 - 27 = 40 - 27 = 13

--------------------------------
-- テストプログラム (ここまで) --
--------------------------------

main :: IO()
main = (putStrLn . fullRun) ex_4_25
