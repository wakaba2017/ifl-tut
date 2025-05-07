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
-- PGM Mark1で追加
type PgmState = (PgmGlobalState,   -- Current global state
                 [PgmLocalState])  -- Current states of processors

-- PGM Mark1で追加
pgmGetOutput :: PgmState -> GmOutput
pgmGetOutput ((o, heap, globals, sparks, unUsedIdNumList, stats), locals) = o

-- PGM Mark1で追加
pgmGetHeap :: PgmState -> GmHeap
pgmGetHeap ((o, heap, globals, sparks, unUsedIdNumList, stats), locals) = heap

-- PGM Mark1で追加
pgmGetGlobals :: PgmState -> GmGlobals
pgmGetGlobals ((o, heap, globals, sparks, unUsedIdNumList, stats), locals) = globals

-- PGM Mark1で追加
type PgmGlobalState = (GmOutput,   -- output stream
                       GmHeap,     -- Heap of nodes
                       GmGlobals,  -- Global addresses in heap
                       GmSparks,   -- Sparked task pool
                       GmUnUsedIdNumList,  -- Un used task id number list
                       GmStats)    -- Statisti

-- PGM Mark1で追加
type PgmLocalState = (GmIdNum,   -- Id number
                      GmCode,    -- Instruction stream
                      GmStack,   -- Pointer stack
                      GmDump,    -- Stack of dump items
                      GmVStack,  -- Value stack
                      GmClock)   -- Number of ticks the task has been active

-- PGM Mark1で変更
type GmState = (PgmGlobalState, PgmLocalState)

data Instruction
   = Unwind
   | Pushglobal Name
   | Pushint Int
   | Push Int
   | Mkap
   | Update Int  -- SGM Mark2で追加
   | Pop Int     -- SGM Mark2で追加
   | Slide Int   -- SGM Mark3で復活
   | Alloc Int   -- SGM Mark3で追加
   | Eval        -- SGM Mark4で追加
   | Add | Sub | Mul | Div | Neg  -- SGM Mark4で追加
   | Eq | Ne | Lt | Le | Gt | Ge  -- SGM Mark4で追加
   | Cond GmCode GmCode  -- SGM Mark4で追加、Mark6で削除、Mark7で復活
   | Pack Int Int              -- SGM Mark6で追加
   | Casejump [(Int, GmCode)]  -- SGM Mark6で追加
   | Split Int                 -- SGM Mark6で追加
   | Print                     -- SGM Mark6で追加
   | Pushbasic Int             -- SGM Mark7で追加
   | Mkbool                    -- SGM Mark7で追加
   | Mkint                     -- SGM Mark7で追加
   | Get                       -- SGM Mark7で追加
   | And | Or | Not            -- SGM Mark7で追加
   | Return                    -- SGM Mark7で追加
   | UpdateInt Int             -- SGM Mark7で追加
   | UpdateBool Int            -- SGM Mark7で追加
   | Par  -- PGM Mark1で追加
  deriving Show  -- テキストにはないけれど追加
instance Eq Instruction
  where
    Unwind       == Unwind       = True
    Pushglobal a == Pushglobal b = a == b
    Pushint    a == Pushint    b = a == b
    Push       a == Push       b = a == b
    Mkap         == Mkap         = True
    Update     a == Update     b = a == b  -- SGM Mark2で追加
    Pop        a == Pop        b = a == b  -- SGM Mark2で追加
    Slide      a == Slide      b = a == b  -- SGM Mark3で復活
    Alloc      a == Alloc      b = a == b  -- SGM Mark3で追加
    Eval         == Eval         = True    -- SGM Mark4で追加
    Add          == Add          = True    -- SGM Mark4で追加
    Sub          == Sub          = True    -- SGM Mark4で追加
    Mul          == Mul          = True    -- SGM Mark4で追加
    Div          == Div          = True    -- SGM Mark4で追加
    Neg          == Neg          = True    -- SGM Mark4で追加
    Eq           == Eq           = True    -- SGM Mark4で追加
    Ne           == Ne           = True    -- SGM Mark4で追加
    Lt           == Lt           = True    -- SGM Mark4で追加
    Le           == Le           = True    -- SGM Mark4で追加
    Gt           == Gt           = True    -- SGM Mark4で追加
    Ge           == Ge           = True    -- SGM Mark4で追加
    Cond a b     == Cond c d     = (a == c) && (b == d)  -- SGM Mark4で追加
    Pack a b     == Pack c d     = (a == c) && (b == d)  -- SGM Mark6で追加
    Casejump   a == Casejump   b = a == b                -- SGM Mark6で追加
    Split      a == Split      b = a == b                -- SGM Mark6で追加
    Print        == Print        = True                  -- SGM Mark6で追加
    Pushbasic  a == Pushbasic b  = a == b                -- SGM Mark7で追加
    Mkbool       == Mkbool       = True                  -- SGM Mark7で追加
    Mkint        == Mkint        = True                  -- SGM Mark7で追加
    Get          == Get          = True                  -- SGM Mark7で追加
    And          == And          = True                  -- SGM Mark7で追加
    Or           == Or           = True                  -- SGM Mark7で追加
    Not          == Not          = True                  -- SGM Mark7で追加
    Return       == Return       = True                  -- SGM Mark7で追加
    UpdateInt  a == UpdateInt  b = a == b                -- SGM Mark7で追加
    UpdateBool a == UpdateBool b = a == b                -- SGM Mark7で追加
    Par          == Par          = True  -- PGM Mark1で追加
    _            == _            = False

-- SGM Mark6で追加
type GmOutput = [Char]

-- SGM Mark6で追加
getOutput :: GmState -> GmOutput
getOutput ((o, heap, globals, sparks, unUsedIdNumList, stats), local) = o

-- SGM Mark6で追加
putOutput :: GmOutput -> GmState -> GmState
putOutput o ((_, heap, globals, sparks, unUsedIdNumList, stats), local)
  = ((o, heap, globals, sparks, unUsedIdNumList, stats), local)

type GmCode = [Instruction]

getCode :: GmState -> GmCode
getCode (global, (idNum, code, stack, dump, vstack, clock)) = code

putCode :: GmCode -> GmState -> GmState
putCode code (global, (idNum, _, stack, dump, vstack, clock)) = (global, (idNum, code, stack, dump, vstack, clock))

type GmStack = [Addr]

getStack :: GmState -> GmStack
getStack (global, (idNum, code, stack, dump, vstack, clock)) = stack

putStack :: GmStack -> GmState -> GmState
putStack stack (global, (idNum, code, _, dump, vstack, clock)) = (global, (idNum, code, stack, dump, vstack, clock))

-- SGM Mark4で追加
type GmDump = [GmDumpItem]
type GmDumpItem = (GmCode, GmStack)

-- SGM Mark4で追加
getDump :: GmState -> GmDump
getDump (global, (idNum, code, stack, dump, vstack, clock)) = dump

-- SGM Mark4で追加
putDump :: GmDump -> GmState -> GmState
putDump dump (global, (idNum, code, stack, _, vstack, clock)) = (global, (idNum, code, stack, dump, vstack, clock))

-- SGM Mark7で追加
type GmVStack = [Int]

-- SGM Mark7で追加
getVStack :: GmState -> GmVStack
getVStack (global, (idNum, code, stack, dump, vstack, clock)) = vstack

-- SGM Mark7で追加
putVStack :: GmVStack -> GmState -> GmState
putVStack vstack (global, (idNum, code, stack, dump, _, clock)) = (global, (idNum, code, stack, dump, vstack, clock))

type GmHeap = Heap Node

getHeap :: GmState -> GmHeap
getHeap ((o, heap, globals, sparks, unUsedIdNumList, stats), local) = heap

putHeap :: GmHeap -> GmState -> GmState
putHeap heap ((o, _, globals, sparks, unUsedIdNumList, stats), local)
  = ((o, heap, globals, sparks, unUsedIdNumList, stats), local)

-- Node 型の定義を、とりあえずシーケンシャルGマシンから引っ張ってきた。
data Node
  = NNum Int           -- Numbers
  | NAp Addr Addr      -- Applications
  | NGlobal Int GmCode -- Globals
  | NInd Addr          -- Indirections  -- SGM Mark2で追加
  | NConstr Int [Addr] -- Constructor   -- SGM Mark6で追加
  | NLAp Addr Addr Int      -- Locked Applications  -- PGM Mark2で追加
  | NLGlobal Int GmCode Int -- Locked Globals       -- PGM Mark2で追加
 deriving Show  -- テキストにはないけれど追加
instance Eq Node
  where
    NNum    a   == NNum    b   = a == b -- needed to check conditions
    NAp     a b == NAp     c d = False  -- not needed
    NGlobal a b == NGlobal c d = False  -- not needed
    NInd    a   == NInd    b   = False  -- not needed
    NConstr a b == NConstr c d = a == c && b == d  -- needed to compare boolean  -- SGM Mark6で追加
    NLAp     a b e == NLAp     c d f = False  -- not needed  -- PGM Mark2で追加
    NLGlobal a b e == NLGlobal c d f = False  -- not needed  -- PGM Mark2で追加

type GmGlobals = ASSOC Name Addr

getGlobals :: GmState -> GmGlobals
getGlobals ((o, heap, globals, sparks, unUsedIdNumList, stats), local) = globals

putGlobals :: GmGlobals -> GmState -> GmState
putGlobals globals ((o, heap, _, sparks, unUsedIdNumList, stats), local)
  = ((o, heap, globals, sparks, unUsedIdNumList, stats), local)

-- PGM Mark1で追加
type GmSparks = [(Addr, Int)]

-- PGM Mark1で追加
pgmGetSparks :: PgmState -> GmSparks
pgmGetSparks ((o, heap, globals, sparks, unUsedIdNumList, stats), locals) = sparks

getSparks :: GmState -> GmSparks
getSparks ((o, heap, globals, sparks, unUsedIdNumList, stats), local) = sparks

putSparks :: GmSparks -> GmState -> GmState
putSparks sparks ((o, heap, globals, _, unUsedIdNumList, stats), local)
  = ((o, heap, globals, sparks, unUsedIdNumList, stats), local)

type GmClock = Int

getClock :: GmState -> GmClock
getClock (global, (idNum, code, stack, dump, vstack, clock)) = clock

putClock :: GmClock -> GmState -> GmState
putClock clock (global, (idNum, code, stack, dump, vstack, _)) = (global, (idNum, code, stack, dump, vstack, clock))

type GmStats = [Int]

getStats :: GmState -> GmStats
getStats ((o, heap, globals, sparks, unUsedIdNumList, stats), local) = stats

putStats :: GmStats -> GmState -> GmState
putStats stats ((o, heap, globals, sparks, unUsedIdNumList, _), local)
  = ((o, heap, globals, sparks, unUsedIdNumList, stats), local)

pgmGetStats :: PgmState -> GmStats
pgmGetStats ((o, heap, globals, sparks, unUsedIdNumList, stats), locals) = stats

type GmUnUsedIdNumList = [Int]

getUnUsedIdNumList :: GmState -> GmUnUsedIdNumList
getUnUsedIdNumList ((o, heap, globals, sparks, unUsedIdNumList, stats), local) = unUsedIdNumList

putUnUsedIdNumList :: GmUnUsedIdNumList -> GmState -> GmState
putUnUsedIdNumList unUsedIdNumList ((o, heap, globals, sparks, _, stats), local)
  = ((o, heap, globals, sparks, unUsedIdNumList, stats), local)

pgmGetUnUsedIdNumList :: PgmState -> GmUnUsedIdNumList
pgmGetUnUsedIdNumList ((o, heap, globals, sparks, unUsedIdNumList, stats), locals) = unUsedIdNumList

type GmIdNum = (Int, Int)

getIdNum :: GmState -> GmIdNum
getIdNum (global, (idNum, code, stack, dump, vstack, clock)) = idNum

putIdNum :: GmIdNum -> GmState -> GmState
putIdNum idNum (global, (_, code, stack, dump, vstack, clock))
  = (global, (idNum, code, stack, dump, vstack, clock))
-------------------------------
-- データ型の定義 (ここまで) --
-------------------------------

-----------------------
-- 評価器 (ここから) --
-----------------------
eval :: PgmState -> [PgmState]
eval state = state: restStates
             where
               restStates | gmFinal state = []
                          | otherwise = eval (doAdmin (steps state))

-- Mark3 PGM で変更
steps :: PgmState -> PgmState
steps state
  = scheduler global' local'
    where ((out, heap, globals, sparks, unUsedIdNumList, stats), local) = state
          sparks_ = zip2 sparks unUsedIdNumList
          newtasks = [makeTask a | a <- sparks_]
          newUnUsedIdNumList = drop (length sparks_) unUsedIdNumList
          global' = (out, heap, globals, [], newUnUsedIdNumList, stats)
          local' = local ++ newtasks

-- Mark3 PGM で追加
machineSize :: Int
machineSize = 4

-- Mark3 PGM で追加
scheduler :: PgmGlobalState -> [PgmLocalState] -> PgmState
scheduler global tasks
  = (global', nonRunning ++ tasks')
    where running = map tick (take machineSize nonBlokked)
          nonRunning = (drop machineSize nonBlokked) ++ blokked
          (global', tasks') = mapAccuml step global running
          {-
            tasks の各要素について、スタックトップのヒープアドレスの中身を調べる。
            NLAp, NLGlobal の場合、コードキューの中身も調べる。
            Unwind 命令だったら、その要素には tick を適用せず、nonRunning に追加。
            上記の条件に非該当の要素のうち、machineSize からあふれた要素も同様に扱う。
            残った各要素に tick を適用して、running とする。
          -}
          filter_func :: PgmLocalState -> Bool
          filter_func (_, code, stack, _, _, _)
            = case stkTopNode of
              node | isTargetNode node -> code == [Unwind]
              _ -> False
              where
                stkTopNode = hLookup (pgmGetHeap (global, tasks)) (head stack)
                isTargetNode (NLAp _ _ _) = True
                isTargetNode (NLGlobal _ _ _) = True
                isTargetNode _ = False
          blokked    = [task | task <- tasks, filter_func task == True ]
          nonBlokked = [task | task <- tasks, filter_func task == False]

-- makeTask :: Addr -> PgmLocalState
-- makeTask addr = ([Unwind], [addr], [], [], 0)  -- for G-machine mark 2 to 3
makeTask :: ((Addr, Int), Int) -> PgmLocalState
makeTask ((addr, pidnum), idnum) = ((idnum, pidnum), [Eval],   [addr], [], [], 0)  -- for G-machine mark 4 to 7


tick (idNum, i, stack, dump, vstack, lock) = (idNum, i, stack, dump, vstack, lock+1)

gmFinal :: PgmState -> Bool
gmFinal s = second s == [] && pgmGetSparks s == []

step :: PgmGlobalState -> PgmLocalState -> GmState
step global local = dispatch i (putCode is state)
                    where (i:is) = getCode state
                          state = (global, local)

doAdmin :: PgmState -> PgmState
doAdmin ((out, heap, globals, sparks, unUsedIdNumList, stats), local)
  = ((out, heap, globals, sparks, unUsedIdNumList, stats'), local')
    where (local', stats') = foldr filter ([], stats) local
          filter (idNum, i, stack, dump, vstack, lock) (local, stats)
            | i == [] = (local, lock:stats)
            | otherwise = ((idNum, i, stack, dump, vstack, lock): local, stats)

dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint    n) = pushint n
dispatch Mkap           = mkap
dispatch (Push       n) = push n
dispatch (Slide      n) = slide n   -- SGM Mark3で復活
dispatch Unwind         = unwind
dispatch (Update     n) = update n  -- SGM Mark2で追加
dispatch (Pop        n) = pop n     -- SGM Mark2で追加
dispatch (Alloc      n) = alloc n   -- SGM Mark3で追加
dispatch Eval           = eval'     -- SGM Mark4で追加
dispatch Add            = add       -- SGM Mark4で追加
dispatch Sub            = sub       -- SGM Mark4で追加
dispatch Mul            = mul       -- SGM Mark4で追加
dispatch Div            = div'      -- SGM Mark4で追加
dispatch Neg            = neg       -- SGM Mark4で追加
dispatch Eq             = eq        -- SGM Mark4で追加
dispatch Ne             = ne        -- SGM Mark4で追加
dispatch Lt             = lt        -- SGM Mark4で追加
dispatch Le             = le        -- SGM Mark4で追加
dispatch Gt             = gt        -- SGM Mark4で追加
dispatch Ge             = ge        -- SGM Mark4で追加
dispatch (Cond     a b) = cond a b  -- SGM Mark4で追加
dispatch (Pack     t n) = pack t n     -- SGM Mark6で追加
dispatch (Casejump  cs) = casejump cs  -- SGM Mark6で追加
dispatch (Split      n) = split n      -- SGM Mark6で追加
dispatch Print          = print'       -- SGM Mark6で追加
dispatch (Pushbasic  n) = pushbasic n  -- SGM Mark7で追加
dispatch Mkbool         = mkbool       -- SGM Mark7で追加
dispatch Mkint          = mkint        -- SGM Mark7で追加
dispatch Get            = get          -- SGM Mark7で追加
dispatch And            = and'         -- SGM Mark7で追加
dispatch Or             = or'          -- SGM Mark7で追加
dispatch Not            = not'         -- SGM Mark7で追加
dispatch Return         = return'      -- SGM Mark7で追加
dispatch (UpdateInt  n) = updateint n   -- SGM Mark7で追加
dispatch (UpdateBool n) = updatebool n  -- SGM Mark7で追加
dispatch Par            = par  -- PGM Mark1で追加

pushglobal :: Name -> GmState -> GmState  -- 遷移規則 (3.5, 3.36, 3.38)
pushglobal f state
  | (take 5 f == "Pack{") && (elem ',' (init (drop 5 f))) && (last f == '}')
    = case temp of
      [] -> putGlobals ((f, addr') : globals) (putHeap newHeap (putStack (addr' : getStack state) state))  -- 遷移規則 (3.38)
      _  -> putStack ((snd $ hd temp) : getStack state) state  -- 遷移規則 (3.37)
      where globals = getGlobals state
            temp = [(name, addr) | (name, addr) <- globals, name == f]
            t_and_n = init (drop 5 f)
            subFunc (',' : xs) = 1
            subFunc (x   : xs) = 1 + subFunc xs
            (ts, ns) = splitAt (subFunc t_and_n) t_and_n
            ti = read (init ts) :: Int
            ni = read ns :: Int
            (newHeap, addr') = hAlloc (getHeap state) (NGlobal ni [Pack ti ni, Update 0, Unwind])
pushglobal f state
  | otherwise
    = putStack (a: getStack state) state  -- 遷移規則 (3.5)
      where a = aLookup (getGlobals state) f (error ("Undeclared global " ++ f))

-- SGM Mark1の改良版で追加
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
getArg (NLAp a1 a2 id) = a2  -- PGM Mark2で追加
getArg (NInd n) = n  -- PGM Mark1で追加

slide :: Int -> GmState -> GmState  -- 遷移規則 (3.9)
slide n state
  = putStack (a: drop n as) state
    where (a:as) = getStack state

unwind :: GmState -> GmState  -- 遷移規則 (3.11, 3.17, 3.19, 3.22, 3.29, 3.35)
unwind state
  = newState (hLookup heap a)
    where
      (a:as) = getStack state
      heap = getHeap state
      newState (NNum n) = putCode i' (putStack (a : s') (putDump d state))  -- 遷移規則 (3.22)
        where ((i', s') : d) = getDump state
      {-
      newState (NAp a1 a2) = putCode [Unwind] (putStack (a1:a:as) state)  -- 遷移規則 (3.11)
      -}
      {-
        hLookup heap a1 の結果が、(NGlobal n' c')で、length (a:as) < n' が成り立つなら、[Unwind]の代わりに[Return]をputCodeしてもいいはず。
      -}
      newState (NLAp a1 a2 id) = putCode [Unwind] state  -- PGM Mark2で追加
      -- newState (NAp a1 a2) = putCode newCommand (putStack (a1:a:as) state)  -- 遷移規則 (3.11)
      newState (NAp a1 a2) = lock a $ putCode newCommand (putStack (a1:a:as) state)  -- 遷移規則 (3.11) -- (5.2) PGM Mark2で変更
        where e1 = hLookup heap a1
              newCommand = case e1 of
                           (NGlobal n' _) -> if length (a:as) < n'
                                             then [Return]
                                             else [Unwind]
                           _ -> [Unwind]
      newState (NLGlobal 0 c id) = putCode [Unwind] state  -- PGM Mark2で追加
      newState (NGlobal 0 c)
        = lock a $ putCode c (putStack (a:as) state)  -- 遷移規則 (5.2) PGM Mark2で追加
      newState (NGlobal n c)
        | length as < n = putCode i (putStack (ak : s) (putDump d state))  -- 遷移規則 (3.29)
        | otherwise     = putCode c (putStack as' state)  -- 遷移規則 (3.19)
          where ((i, s) : d) = getDump state
                ak           = last (getStack state)  -- asが空リストの場合、last asではエラーとなるため、改めてスタックを取得している。
                as'          = rearrange n (getHeap state) (getStack state)
      newState (NInd n) = putCode [Unwind] (putStack (n : as) state)  -- 遷移規則 (3.17)
      {-
      {-
        hLookup heap n の結果が、(NNum n')だったら、[Unwind]の代わりに[Return]をputCodeしてもいいはず。
      -}
      newState (NInd n) = putCode newCommand (putStack (n : as) state)  -- 遷移規則 (3.17)
        where n' = hLookup heap n
              newCommand = case n' of
                           (NNum _) -> [Return]
                           _        -> [Unwind]
      -}
      newState (NConstr t as) = putCode i' (putStack (a : s') (putDump d state))  -- 遷移規則 (3.35)
        where ((i', s') : d) = getDump state

-- SGM Mark3で追加
rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as
  = take n as' ++ drop n as
    where as' = map (getArg . hLookup heap) (tl as)

-- SGM Mark2で追加
update :: Int -> GmState -> GmState  -- 遷移規則 (3.15)
  {-
  stateからスタックを取り出す。
  取り出したスタックの先頭要素aを抜き出す。
  先頭要素を抜き出したスタックのn+1番目の値a_nを読み取る。
  stateからヒープを取り出す。
  取り出したヒープのアドレスa_nの内容を(NInd a)に置き換える。
  -}
update n state
  -- = putHeap newHeap (putStack as state)
  = putCode newCode $ putHeap newHeap (putStack as state)
    where (a : as) = getStack state
          an = as !! n
          -- (size, free, cts) = getHeap state
          -- newHeap = hUpdate (size, free, cts) an (NInd a)
          tempState = unlockAll an state  -- unlockAll 関数で再帰的にロック解除
          currentHeap = getHeap tempState
          -- newHeap = hUpdate (getHeap tempState) an (NInd a)
          newHeap = hUpdate currentHeap an (NInd a)
          {-
            スタックトップに格納されたヒープアドレス a の中身が NNum ノードで、
            以下の条件を満たしていたら、Update により実質的な処理は終わるので、
            命令キューの中身を空にしてローカルマシンを早めに終了させる。
            ・タスク ID が 1 でない(最上位のタスクでない)。
            ・ダンプの内容が初期値と同じ(ダンプに退避されている処理内容が残っていない)。
            ・命令キューの内容が、Pop n, Unwind である(後は、間接参照ノードをアンワインドするだけ)。
          -}
          stkTopNode = hLookup currentHeap a
          currentDump = getDump tempState
          currentCode = getCode tempState
          idNum = fst $ getIdNum tempState
          newCode = case stkTopNode of
                    NNum _ -> if idNum /= 1 && currentDump == [([], [])] && currentCode == [Pop (length as - 1), Unwind]
                              then []
                              else currentCode
                    _ -> currentCode

-- SGM Mark2で追加
pop :: Int -> GmState -> GmState  -- 遷移規則 (3.16)
pop n state
  = putStack (drop n (getStack state)) state

-- SGM Mark3で追加
alloc :: Int -> GmState -> GmState  -- 遷移規則 (3.20)
alloc n state
  = putHeap newHeap (putStack newStack state)
    where (newHeap, as) = allocNodes n (getHeap state)
          newStack = as ++ (getStack state)

-- SGM Mark3で追加
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

-- SGM Mark4で追加
eval' :: GmState -> GmState  -- 遷移規則 (3.23)
eval' state
  = putCode [Unwind] (putStack [a] (putDump newDump state))
    where (a : s) = getStack state
          newDump = (getCode state, s) : (getDump state)

-- SGM Mark4で追加
add :: GmState -> GmState  -- 遷移規則 (3.24)
add state
  = arithmetic2 (+) state

-- SGM Mark4で追加
sub :: GmState -> GmState  -- 遷移規則 (3.24)
sub state
  = arithmetic2 (-) state

-- SGM Mark4で追加
mul :: GmState -> GmState  -- 遷移規則 (3.24)
mul state
  = arithmetic2 (*) state

-- SGM Mark4で追加
div' :: GmState -> GmState  -- 遷移規則 (3.24)
div' state
  = arithmetic2 div state

-- SGM Mark4で追加
neg :: GmState -> GmState  -- 遷移規則 (3.25)
neg state
  = arithmetic1 (* (-1)) state

-- SGM Mark4で追加
eq :: GmState -> GmState  -- 遷移規則 (3.26)
eq state
  = comparison (==) state

-- SGM Mark4で追加
ne :: GmState -> GmState  -- 遷移規則 (3.26)
ne state
  = comparison (/=) state

-- SGM Mark4で追加
lt :: GmState -> GmState  -- 遷移規則 (3.26)
lt state
  = comparison (<) state

-- SGM Mark4で追加
le :: GmState -> GmState  -- 遷移規則 (3.26)
le state
  = comparison (<=) state

-- SGM Mark4で追加
gt :: GmState -> GmState  -- 遷移規則 (3.26)
gt state
  = comparison (>) state

-- SGM Mark4で追加
ge :: GmState -> GmState  -- 遷移規則 (3.26)
ge state
  = comparison (>=) state

-- SGM Mark4で追加
cond :: GmCode -> GmCode -> GmState -> GmState  -- 遷移規則 (3.46, 3.47)
cond i1 i2 state
  = putCode newCode (putVStack v state)
    where
      (t : v) = getVStack state
      i = getCode state 
      newCode | t == 2 = i1 ++ i  -- 遷移規則 (3.46) (Mark7で復活)
              | t == 1 = i2 ++ i  -- 遷移規則 (3.47) (Mark7で復活)
              | otherwise = error ("Cond vstack contents error" ++ (show t))

-- SGM Mark4で追加
boxInteger :: Int -> GmState -> GmState
boxInteger n state
  = putStack (a: getStack state) (putHeap h' state)
    where (h', a) = hAlloc (getHeap state) (NNum n)

-- SGM Mark4で追加
unboxInteger :: Addr -> GmState -> Int
unboxInteger a state
  = ub (hLookup (getHeap state) a)
    where ub (NNum i) = i
          ub (NConstr i []) = i  -- Mark6で変更
          ub n        = error "Unboxing a non-integer"

-- SGM Mark4で追加
primitive1 :: (b -> GmState -> GmState) -- boxing function
              -> (Addr -> GmState -> a) -- unbixing function
              -> (a -> b) -- operator
              -> (GmState -> GmState) -- state transition
primitive1 box unbox op state
  = box (op (unbox a state)) (putStack as state)
    where (a:as) = getStack state

-- SGM Mark4で追加
primitive2 :: (b -> GmState -> GmState) -- boxing function
              -> (Addr -> GmState -> a) -- unboxing function
              -> (a -> a -> b) -- operator
              -> (GmState -> GmState) -- state transition
primitive2 box unbox op state
  = box (op (unbox a0 state) (unbox a1 state)) (putStack as state)
    where (a0:a1:as) = getStack state

-- SGM Mark4で追加、Mark7で変更
arithmetic1 :: (Int -> Int)            -- arithmetic operator
               -> (GmState -> GmState) -- state transition
arithmetic1 op state
  = putVStack (op a : as) state
    where (a : as) = getVStack state

-- SGM Mark4で追加、Mark7で変更
arithmetic2 :: (Int -> Int -> Int)     -- arithmetic operation
               -> (GmState -> GmState) -- state transition
arithmetic2 op state
  = putVStack (op a0 a1 : as) state
    where (a0 : a1 : as) = getVStack state

-- SGM Mark4で追加
boxBoolean :: Bool -> GmState -> GmState
boxBoolean b state
  = putStack (a: getStack state) (putHeap h' state)
    where (h',a) = hAlloc (getHeap state) (NConstr b' [])
          b' | b         = 2  -- 2 is tag of True  (Mark6で変更)
             | otherwise = 1  -- 1 is tag of False (Mark6で変更)

-- SGM Mark4で追加
comparison :: (Int -> Int -> Bool) -> GmState -> GmState
comparison op state
  = putVStack (result : as) state
    where (a0 : a1 : as) = getVStack state
          result | op a0 a1 = 2
                 | otherwise = 1

-- SGM Mark6で追加
pack :: Int -> Int -> GmState -> GmState  -- 遷移規則 (3.30)
pack t n state
  = putStack newStack (putHeap newHeap state)  -- 遷移規則 (3.30)
    where stack = getStack state
          (newHeap, a) = hAlloc (getHeap state) (NConstr t (take n stack))
          newStack = a : (drop n stack)

-- SGM Mark6で追加
casejump :: [(Int, GmCode)] -> GmState -> GmState  -- 遷移規則 (3.31)
casejump cs state
{-
stateからスタックを取り出す
スタックトップのアドレスがさすヒープの中身をチェックする
ヒープの中身がNConstrでなかったらエラー
ヒープの中身がNConstrだったらタグの値をチェックする
csの要素を調べて、NConstrのタグに等しい要素を見つけて、コマンドリストi'を取り出す
現在のコマンドリストiの前にi'を連結する
-}
  = newState (hLookup heap a)
    where
      (a : s) = getStack state
      heap = getHeap state
      newState (NConstr t as) = putCode newCode state  -- 遷移規則 (3.31)
        where alt = [(t', i') | (t', i') <- cs, t' == t ]
              newCode | length alt == 1 = (snd (head alt)) ++ (getCode state)
                      | otherwise       = error "Casejump tag error"
      newState _ = error "Casejump heap contents error"

-- SGM Mark6で追加
split :: Int -> GmState -> GmState  -- 遷移規則 (3.32)
split n state
  = newState (hLookup heap a)
    where
      (a : s) = getStack state
      heap = getHeap state
      newState (NConstr t as)
        | length as == n = putStack (as ++ s) state  -- 遷移規則 (3.32)
        | otherwise      = error "Split argument error"
      newState _ = error "Split heap contents error"

-- SGM Mark6で追加
print' :: GmState -> GmState  -- 遷移規則 (3.33, 3.34)
print' state
  = newState (hLookup heap a)
    where
      (a : s) = getStack state
      heap = getHeap state
      newState (NNum n) = putOutput newOutput (putStack s state)  -- 遷移規則 (3.33)
        where newOutput = (getOutput state) ++ (show n)
      newState (NConstr t as) = putCode newCode (putStack newStack state)  -- 遷移規則 (3.34)
        where newStack = as ++ s
              newCode = (subFuncForPrint (length as)) ++ (getCode state)
              subFuncForPrint 0 = []
              subFuncForPrint n = [Eval, Print] ++ (subFuncForPrint (n - 1))
      newState _ = error "Print heap contents error"

-- SGM Mark7で追加
pushbasic :: Int -> GmState -> GmState  -- 遷移規則 (3.41)
pushbasic n state
  = putVStack (n : getVStack state) state

-- SGM Mark7で追加
mkbool :: GmState -> GmState  -- 遷移規則 (3.42)
mkbool state
  = putStack (a : getStack state) (putVStack v (putHeap newHeap state))
    where (t : v) = getVStack state
          (newHeap, a) = hAlloc (getHeap state) (NConstr t [])

-- SGM Mark7で追加
mkint :: GmState -> GmState  -- 遷移規則 (3.43)
mkint state
  = putStack (a : getStack state) (putVStack v (putHeap newHeap state))
    where (n : v) = getVStack state
          (newHeap, a) = hAlloc (getHeap state) (NNum n)

-- SGM Mark7で追加
get :: GmState -> GmState  -- 遷移規則 (3.44, 3.45)
get state
  = newState (hLookup heap a)
    where
      (a : s) = getStack state
      heap = getHeap state
      newState (NConstr t []) = putVStack (t : getVStack state) (putStack s state)  -- 遷移規則 (3.44)
      newState (NNum n)       = putVStack (n : getVStack state) (putStack s state)  -- 遷移規則 (3.45)
      newState _ = error "Get heap contents error"

-- SGM Mark7で追加
and' :: GmState -> GmState
and' state
  = boolean2 (&&) state

-- SGM Mark7で追加
or' :: GmState -> GmState
or' state
  = boolean2 (||) state

-- SGM Mark7で追加
not' :: GmState -> GmState
not' state
  = boolean1 (not) state

-- SGM Mark7で追加
boolean1 :: (Bool -> Bool)          -- boolean operator
            -> (GmState -> GmState) -- state transition
boolean1 op state
  = putVStack (result : as) state
    where (a : as) = getVStack state
          a' = case a of
               2 -> True
               1 -> False
               _ -> error "invalid boolean value in vstack"
          result | op a' = 2
                 | otherwise = 1

-- SGM Mark4で追加、Mark7で変更
boolean2 :: (Bool -> Bool -> Bool)  -- boolean operation
            -> (GmState -> GmState) -- state transition
boolean2 op state
  = putVStack (result : as) state
    where (a0 : a1 : as) = getVStack state
          a0' = case a0 of
                2 -> True
                1 -> False
                _ -> error "invalid boolean value in vstack"
          a1' = case a1 of
                2 -> True
                1 -> False
                _ -> error "invalid boolean value in vstack"
          result | op a0' a1' = 2
                 | otherwise = 1

-- SGM Mark7で追加
return' :: GmState -> GmState  -- 遷移規則 (3.49)
return' state
  = putCode i (putStack (ak : s) (putDump d state))
    where ((i, s) : d) = getDump state
          ak           = last (getStack state)

-- SGM Mark7で追加
{-
遷移規則 (3.50)
    (o, UpdateInt n : i, a_0 : … : an : s, d, n' : v, h,                           m)
==> (o,               i, a_0 : … : an : s, d,      v, h[a : NNum n'; an : NInd a], m)
-}
updateint :: Int -> GmState -> GmState  -- 遷移規則 (3.50)
updateint n state
  = putVStack v (putHeap newHeap state)
    where (n' : v) = getVStack state
          ((size, free, cts), a) = hAlloc (getHeap state) (NNum n')
          an = (getStack state) !! n
          newHeap = hUpdate (size, free, cts) an (NInd a)

-- SGM Mark7で追加
{-
遷移規則 (3.51)
    (o, UpdateBool n : i, a_0 : … : a_n : s, d, t : v, h,                                 m)
==> (o,                i, a_0 : … : a_n : s, d,     v, h[a : NConstr t []; a_n : NInd a], m)
-}
updatebool :: Int -> GmState -> GmState  -- 遷移規則 (3.51)
updatebool n state
  = putVStack v (putHeap newHeap state)
    where (t : v) = getVStack state
          ((size, free, cts), a) = hAlloc (getHeap state) (NConstr t [])
          an = (getStack state) !! n
          newHeap = hUpdate (size, free, cts) an (NInd a)

-- PGM Mark1で追加
par :: GmState -> GmState
-- par s = s
par state = putSparks ((a, idnum) : t) $ putStack s state
            where (a : s) = getStack state
                  t = getSparks state
                  idnum = fst $ getIdNum state

-- PGM Mark2で追加
lock :: Addr -> GmState -> GmState
lock addr state
  = putHeap (newHeap (hLookup heap addr)) state
    where
      heap = getHeap state
      id = fst $ getIdNum state
      newHeap (NAp a1 a2) = hUpdate heap addr (NLAp a1 a2 id)
      newHeap (NGlobal n c) | n == 0 = hUpdate heap addr (NLGlobal n c id)
                            | otherwise = heap

-- PGM Mark2で追加
unlock :: Addr -> GmState -> GmState
unlock addr state
  = newState (hLookup heap addr)
    where
      heap = getHeap state
      newState (NLAp a1 a2 id)
        = unlock a1 (putHeap (hUpdate heap addr (NAp a1 a2)) state)
      newState (NLGlobal n c id)
        = putHeap (hUpdate heap addr (NGlobal n c)) state
      newState n = state

-- PGM Mark2で追加
unlockAll :: Addr -> GmState -> GmState
unlockAll addr state = unlockArg addr (unlock addr state)
  where
    unlockArg addr state = case hLookup heap addr of
      NAp _ a2      -> unlockAll a2 state
      NLAp _ a2 id  -> unlockAll a2 state
      _             -> state
      where
        heap = getHeap state
----------------------
-- 評価器 (ここまで) --
----------------------

---------------------------------------
-- プログラムのコンパイル (ここから) --
---------------------------------------
-- PGM Mark1で変更
compile :: CoreProgram -> PgmState
compile program
  = (([], heap, globals, [], [2..], []), [initialTask addr])
    where (heap, globals) = buildInitialHeap program
          addr = aLookup globals "main" (error "main undefined")

buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program
  = mapAccuml allocateSc hInitial compiled
    where compiled = map compileSc (preludeDefs ++ program ++ primitives)  -- Mark7で変更

type GmCompiledSC = (Name, Int, GmCode)

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns)
  = (heap', (name, addr))
    where (heap', addr) = hAlloc heap (NGlobal nargs instns)

-- PGM Mark1で追加
initialTask :: Addr -> PgmLocalState
initialTask addr = ((1, 0), initialCode, [addr], [], [], 0)

-- PGM Mark1で変更
initialCode :: GmCode
initialCode = [Eval, Print]

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC  -- SCスキーム
compileSc (name, env, body)
  = (name, length env, compileR body (zip2 env [0..]))

compileR :: GmCompiler  -- Rスキーム
compileR (ELet recursive defs e) args  -- SGM Mark7で追加
  | recursive = compileLetrec compileR defs e args  -- recursive が True  の場合
  | otherwise = compileLet    compileR defs e args  -- recursive が False の場合
compileR (EAp (EAp (EAp (EVar "if") e0) e1) e2) env
  = compileB e0 env ++ [Cond e1' e2']
    where e1' = compileR e1 env
          e2' = compileR e2 env
compileR (ECase e alts) env  -- SGM Mark7で追加
  = compileE e env ++ [Casejump (compileAlts compileR' alts env)]
--compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]
compileR (EAp (EAp (EVar "par") e1) e2) args  -- PGM Mark1で追加
  = compileC e2 args ++ [Push 0, Par] ++
    compileC e1 (argOffset 1 args) ++ [Mkap, Update n, Pop n, Unwind]
    where n = length args
compileR (EAp (EAp (EVar op) e0) e1) env  -- SGM Mark7で追加
  | length temp == 1 = compileB (EAp (EAp (EVar op) e0) e1) env ++ temp'
  | otherwise        = compileE (EAp (EAp (EVar op) e0) e1) env ++ [Update d, Pop d, Unwind]
    where d     = length env
          env'  = argOffset 1 env
          temp  = [(oprtr, instrctn) | (oprtr, instrctn) <- builtInDyadic, oprtr == op]
          temp' | fst (hd temp) `elem` ["+", "-", "*", "div"] = [Mkint, Update d, Pop d, Unwind]
                | otherwise = [Mkbool, Update d, Pop d, Unwind]
compileR (EAp (EVar "negate") e) env  -- SGM Mark7で追加
  = compileB (EAp (EVar "negate") e) env ++ [UpdateInt d, Pop d, Unwind]
    where d = length env
compileR e env = compileE e env ++ [Update d, Pop d, Unwind]
                 where d = length env

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

type GmEnvironment = ASSOC Name Int

compileC :: GmCompiler  -- Cスキーム
compileC (EVar v) args
  | elem v (aDomain args) = [Push n]
  | otherwise             = [Pushglobal v]
  where n = aLookup args v (error "")
compileC (ENum n)    env = [Pushint n]
compileC (EConstr t a) env  -- SGM Mark6で追加
  = if a == 0
    then [Pack t a]
    else [Pushglobal packstr]
         where packstr = "Pack{" ++ (show t) ++ "," ++ (show a) ++ "}"
compileC (EAp e1 e2) env
  = case (fst resultOfTraverse) of
    (EConstr t a) -> compileC e2 env ++ compileCForEConstrInEAp e1 (argOffset 1 env) 1 ++ snd resultOfTraverse
    _             -> compileC e2 env ++ compileC                e1 (argOffset 1 env)   ++ snd resultOfTraverse
    where resultOfTraverse = subFuncForEAp e1 False
compileC (ELet recursive defs e) args  -- SGM Mark3で追加
  | recursive = compileLetrec compileC defs e args  -- recursive が True  の場合
  | otherwise = compileLet    compileC defs e args  -- recursive が False の場合

-- SGM Mark5で追加
compileE :: GmCompiler  -- Eスキーム
compileE (ENum n) env = [Pushint n]
compileE (ELet recursive defs e) args
  | recursive = compileLetrec compileE defs e args  -- recursive が True  の場合
  | otherwise = compileLet    compileE defs e args  -- recursive が False の場合
compileE (EAp (EAp (EVar "par") e1) e2) args  -- PGM Mark1で追加
  = compileC e2 args ++ [Push 0, Par] ++
    compileC e1 (argOffset 1 args) ++ [Mkap, Eval]
compileE (EAp (EAp (EVar op) e0) e1) env
  | length temp == 1 = compileB (EAp (EAp (EVar op) e0) e1) env ++ temp'
  | otherwise        = compileC e1 env ++ compileC (EAp (EVar op) e0) env' ++ [Mkap]
    where env'  = argOffset 1 env
          temp  = [(oprtr, instrctn) | (oprtr, instrctn) <- builtInDyadic, oprtr == op]
          temp' | fst (hd temp) `elem` ["+", "-", "*", "div"] = [Mkint]
                | otherwise = [Mkbool]
compileE (EAp (EVar "negate") e) env
  = compileB (EAp (EVar "negate") e) env ++ [Mkint]
compileE (EAp (EVar "not") e) env
  = compileB (EAp (EVar "not") e) env ++ [Mkbool]
compileE (EAp (EAp (EAp (EVar "if") e0) e1) e2) env
  = compileB e0 env ++ [Cond e1' e2']
    where e1' = compileE e1 env
          e2' = compileE e2 env
compileE (ECase e alts) env  -- SGM Mark6で追加
  = compileE e env ++ [Casejump (compileAlts compileE' alts env)]
compileE (EConstr t a) env  -- SGM Mark6で追加
  = if a == 0
    then [Pack t a]
    else [Pushglobal packstr]
         where packstr = "Pack{" ++ (show t) ++ "," ++ (show a) ++ "}"
compileE (EAp e1 e2) env
  = case (fst resultOfTraverse) of
    (EConstr t a) -> compileC e2 env ++ compileCForEConstrInEAp e1 (argOffset 1 env) 1 ++ snd resultOfTraverse
    -- (EConstr t a) -> compileC e2 env ++ compileCForEConstrInEAp e1 env               1 ++ snd resultOfTraverse
    _             -> compileC e2 env ++ compileC                e1 (argOffset 1 env)   ++ snd resultOfTraverse
    where resultOfTraverse = subFuncForEAp e1 True
compileE e env = compileC e env ++ [Eval]


subFuncForEAp :: CoreExpr -> Bool -> (CoreExpr, GmCode)
subFuncForEAp e forEscm =
  case e of
  EConstr t  a -> (EConstr t a, [])
  EAp     e' _ -> (fst temp, snd temp)
                  where temp = subFuncForEAp e' forEscm
  _            -> if forEscm == True
                  then (ENum 0, [Mkap] ++ [Eval])  -- Eスキームで関数適用を処理する場合、デフォルトケースには行かなくなるようなので、ここで[Eval]をappendすることにした。
                  else (ENum 1, [Mkap])

compileCForEConstrInEAp e env eapcnt
  = case e of
    EConstr t a -> if a == eapcnt
                   then [Pack t a]
                   else let packstr = "Pack{" ++ (show t) ++ "," ++ (show a) ++ "}"
                        in [Pushglobal packstr]
    EAp e1 e2 -> compileC e2 env ++ compileCForEConstrInEAp e1 (argOffset 1 env) (eapcnt + 1)

-- SGM Mark6で追加
compileE' :: Int -> GmCompiler
compileE' offset expr env
  = [Split offset] ++ compileE expr env ++ [Slide offset]

-- SGM Mark7で追加
compileR' :: Int -> GmCompiler
compileR' offset expr env
  = [Split offset] ++ compileR expr env

-- SGM Mark7で追加
compileB :: GmCompiler  -- Bスキーム
compileB (ENum n) env = [Pushbasic n]
compileB (EVar "True") env = [Pushbasic 2]
compileB (EVar "False") env = [Pushbasic 1]
compileB (ELet recursive defs e) args
  | recursive = compileLetrecB compileB defs e args  -- recursive が True  の場合
  | otherwise = compileLetB    compileB defs e args  -- recursive が False の場合
compileB (EAp (EAp (EVar op) e0) e1) env
  | length temp == 1 = compileB e1 env ++ compileB e0 env ++ [snd (hd temp)]
  | otherwise        = compileC e1 env ++ compileC (EAp (EVar op) e0) env' ++ [Mkap]
    where env'  = argOffset 1 env
          temp  = [(oprtr, instrctn) | (oprtr, instrctn) <- builtInDyadic, oprtr == op]
compileB (EAp (EVar "negate") e) env
  = compileB e env ++ [Neg]
compileB (EAp (EVar "not") e) env
  = compileB e env ++ [Not]
compileB (EAp (EAp (EAp (EVar "if") e0) e1) e2) env
  = compileB e0 env ++ [Cond e1' e2']
    where e1' = compileB e1 env
          e2' = compileB e2 env
compileB e env = compileE e env ++ [Get]

-- SGM Mark3で追加
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

-- SGM Mark7で追加
compileLetB :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetB comp defs expr env
  = compileLet' defs env ++ comp expr env' ++ [Pop (length defs)]
    where env' = compileArgs defs env

-- SGM Mark3で追加
compileLet' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLet' [] env = []
compileLet' ((name, expr):defs) env
  = compileC expr env ++ compileLet' defs (argOffset 1 env)

-- SGM Mark3で追加
compileLetrec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetrec comp defs expr env
  = [Alloc (length defs)] ++ compileLetrec' defs env' ++ comp expr env' ++ [Slide (length defs)]
    where env' = compileArgs defs env

-- SGM Mark7で追加
compileLetrecB :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetrecB comp defs expr env
  = [Alloc (length defs)] ++ compileLetrec' defs env' ++ comp expr env' ++ [Pop (length defs)]
    where env' = compileArgs defs env

-- SGM Mark3で追加
compileLetrec' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLetrec' [] env = []
compileLetrec' ((name, expr):defs) env
  = compileC expr env ++ [Update (length defs)] ++ compileLetrec' defs env

-- SGM Mark3で追加
compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env
  = zip (map first defs) [n-1, n-2 .. 0] ++ argOffset n env
    where n = length defs

-- SGM Mark6で追加
compileAlts :: (Int -> GmCompiler) -- compiler for alternative bodies
               -> [CoreAlt]        -- the list of alternatives
               -> GmEnvironment    -- the current environment
               -> [(Int, GmCode)]  -- list of alternative code sequences
compileAlts comp alts env
  = [(tag, comp (length names) body (zip names [0..] ++ argOffset (length names) env)) | (tag, names, body) <- alts]

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v,m) <- env]

-- SGM Mark7で追加
primitives :: [(Name,[Name],CoreExpr)]
primitives
  = [
     ("+", ["x","y"], (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))),
     ("-", ["x","y"], (EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))),
     ("*", ["x","y"], (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))),
     ("/", ["x","y"], (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))),
     ("negate", ["x"], (EAp (EVar "negate") (EVar "x"))),
     ("==", ["x","y"], (EAp (EAp (EVar "==") (EVar "x")) (EVar "y"))),
     ("~=", ["x","y"], (EAp (EAp (EVar "~=") (EVar "x")) (EVar "y"))),
     (">=", ["x","y"], (EAp (EAp (EVar ">=") (EVar "x")) (EVar "y"))),
     (">",  ["x","y"], (EAp (EAp (EVar ">") (EVar "x")) (EVar "y"))),
     ("<=", ["x","y"], (EAp (EAp (EVar "<=") (EVar "x")) (EVar "y"))),
     ("<",  ["x","y"], (EAp (EAp (EVar "<") (EVar "x")) (EVar "y"))),
     ("if", ["c","t","f"],
            (EAp (EAp (EAp (EVar "if") (EVar "c")) (EVar "t")) (EVar "f"))),
     ("True", [], (EConstr 2 0)),
     ("False", [], (EConstr 1 0)),
     ("&",  ["x","y"], (EAp (EAp (EVar "&") (EVar "x")) (EVar "y"))),
     ("|",  ["x","y"], (EAp (EAp (EVar "|") (EVar "x")) (EVar "y"))),
     ("not",  ["x"], (EAp (EAp (EVar "not") (EVar "x")) (EVar "y"))),
     ("par", ["x","y"], (EAp (EAp (EVar "par") (EVar "x")) (EVar "y")))  -- PGM Mark1で追加
    ]

-- SGM Mark5で追加
builtInDyadic :: ASSOC Name Instruction
builtInDyadic
  = [("+", Add), ("-", Sub), ("*", Mul), ("div", Div),
     ("==", Eq), ("~=", Ne),
     (">=", Ge), (">", Gt),
     ("<=", Le), ("<", Lt),
     ("&", And), ("|", Or)  -- SGM Mark7で追加
     ]
---------------------------------------
-- プログラムのコンパイル (ここまで) --
---------------------------------------

--------------------------
-- 結果の表示 (ここから) --
--------------------------
showResults :: [PgmState] -> [Char]
showResults states
  = iDisplay (iConcat [iStr "Supercombinator definitions", iNewline,
                       iInterleave iNewline (map (showSC s) (pgmGetGlobals s)),
                       iNewline, iNewline, iStr "State transitions", iNewline, iNewline,
                       iLayn (map showState states),
                       iNewline, iNewline,
                       showStats (last states)])
    where (s:ss) = states

showSC :: PgmState -> (Name, Addr) -> Iseq
showSC s (name, addr)
  = iConcat [ iStr "Code for ", iStr name, iNewline,
              showInstructions code, iNewline, iNewline]
    where (NGlobal arity code) = (hLookup (pgmGetHeap s) addr)

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
showInstruction (Slide      n) = (iStr "Slide ") `iAppend` (iNum n)   -- SGM Mark3で復活
showInstruction (Update     n) = (iStr "Update ") `iAppend` (iNum n)  -- SGM Mark2で追加
showInstruction (Pop        n) = (iStr "Pop ") `iAppend` (iNum n)     -- SGM Mark2で追加
showInstruction (Alloc      n) = (iStr "Alloc ") `iAppend` (iNum n)   -- SGM Mark3で追加
showInstruction Eval           = iStr "Eval"                          -- SGM Mark4で追加
showInstruction Add            = iStr "Add"                           -- SGM Mark4で追加
showInstruction Sub            = iStr "Sub"                           -- SGM Mark4で追加
showInstruction Mul            = iStr "Mul"                           -- SGM Mark4で追加
showInstruction Div            = iStr "Div"                           -- SGM Mark4で追加
showInstruction Neg            = iStr "Neg"                           -- SGM Mark4で追加
showInstruction Eq             = iStr "Eq"                            -- SGM Mark4で追加
showInstruction Ne             = iStr "Ne"                            -- SGM Mark4で追加
showInstruction Lt             = iStr "Lt"                            -- SGM Mark4で追加
showInstruction Le             = iStr "Le"                            -- SGM Mark4で追加
showInstruction Gt             = iStr "Gt"                            -- SGM Mark4で追加
showInstruction Ge             = iStr "Ge"                            -- SGM Mark4で追加
showInstruction (Cond     a b)
  = (iStr "Cond ") `iAppend`
    (shortShowInstructions 3 a) `iAppend`
    (iStr " ") `iAppend`
    (shortShowInstructions 3 b)
showInstruction (Pack     t a) = (iStr "Pack ") `iAppend` (iNum t) `iAppend` (iStr " ") `iAppend` (iNum a)  -- SGM Mark6で追加
showInstruction (Casejump  cs)
  = iStr "Casejump" `iAppend` (subFuncForCasejump cs)  -- SGM Mark6で追加
    where
      subFuncForCasejump [] = iNil
      subFuncForCasejump ((n, c) : cs) = (iStr " (") `iAppend` (iNum n) `iAppend` (iStr ", ") `iAppend`
                                          (shortShowInstructions 3 c) `iAppend` (iStr ")") `iAppend`
                                          (subFuncForCasejump cs)
showInstruction (Split      n) = (iStr "Split ") `iAppend` (iNum n)                    -- SGM Mark6で追加
showInstruction Print          = iStr "Print"                                          -- SGM Mark6で追加
showInstruction (Pushbasic  n) = (iStr "Pushbasic ") `iAppend` (iNum n)                -- SGM Mark7で追加
showInstruction Mkbool         = iStr "Mkbool"                                         -- SGM Mark7で追加
showInstruction Mkint          = iStr "Mkint"                                          -- SGM Mark7で追加
showInstruction Get            = iStr "Get"                                            -- SGM Mark7で追加
showInstruction And            = iStr "And"                                            -- SGM Mark7で追加 
showInstruction Or             = iStr "Or"                                             -- SGM Mark7で追加
showInstruction Not            = iStr "Not"                                            -- SGM Mark7で追加
showInstruction Return         = iStr "Return"                                         -- SGM Mark7で追加
showInstruction (UpdateInt  n) = (iStr "UpdateInt ") `iAppend` (iNum n)                -- SGM Mark7で追加
showInstruction (UpdateBool n) = (iStr "UpdateBool ") `iAppend` (iNum n)               -- SGM Mark7で追加
showInstruction Par            = (iStr "Par")  -- PDM Mark1で追加

-- SGM Mark6で追加
showOutput :: GmOutput -> Iseq
showOutput o = iConcat [iStr " Output:\"", iStr o, iStr "\""]

-- PGM Mark1で変更
showState :: PgmState -> Iseq
showState s
  = iConcat [iStr " Global:[",
             iIndent (showStateGlobal s),
             iStr "]", iNewline,
             iStr " Local:[",
             iIndent (iInterleave iNewline
                     (map showStateLocal ss)),
             iStr "]"]
    where ss = map (\x -> (fst s, x)) $ snd s

-- PGM Mark1で追加
showStateGlobal :: PgmState -> Iseq
showStateGlobal s
  = iConcat [showOutput (pgmGetOutput s), iNewline,
             showSparks (pgmGetSparks s), iNewline
             ]

-- PGM Mark1で追加
showStateLocal :: GmState -> Iseq
showStateLocal s
  = iConcat [showIdNum s, iNewline,
             showStack s, iNewline,
             showDump s, iNewline,
             showVStack s, iNewline,  -- SGM Mark7で追加
             showInstructions (getCode s),
             showClocks s, iNewline]

showIdNum :: GmState -> Iseq
showIdNum s
  = iConcat [iStr " -- Task ", iNum idnum, parentTaskId, iStr " --"]
    where
      (idnum, pidnum) = getIdNum s
      parentTaskId | pidnum == 0 = iStr ""
                   | otherwise   = iConcat [iStr " (Parent Task ", iNum pidnum, iStr ")"]

showStack :: GmState -> Iseq
showStack s
  = iConcat [iStr " Stack:[",
             iIndent (iInterleave iNewline
                     (map (showStackItem s) (reverse (getStack s)))),
             iStr "]"]

showStackItem :: GmState -> Addr -> Iseq
showStackItem s a
  = iConcat [iStr (showaddr a), iStr ": ",
--              showNode s a (hLookup (getHeap s) a)]
             showNode s a (hLookup (getHeap s) a),
             iStr "  -- ",
             showContent s a (hLookup (getHeap s) a)]

-- SGM Mark4で追加
showDump :: GmState -> Iseq
showDump s
  = iConcat [iStr " Dump:[",
             iIndent (iInterleave iNewline
                     (map showDumpItem (reverse (getDump s)))),
             iStr "]"]

-- SGM Mark4で追加
showDumpItem :: GmDumpItem -> Iseq
showDumpItem (code, stack)
  = iConcat [iStr "<",
             shortShowInstructions 3 code, iStr ", ",
             shortShowStack stack, iStr ">"]

-- SGM Mark7で追加
showVStack :: GmState -> Iseq
showVStack s
  = iConcat [iStr " Vstack:[",
             iInterleave (iStr ", ") (map iNum (getVStack s)),
             iStr "]"]

-- SGM Mark4で追加
shortShowInstructions :: Int -> GmCode -> Iseq
shortShowInstructions number code
  = iConcat [iStr "{", iInterleave (iStr "; ") dotcodes, iStr "}"]
    where codes = map showInstruction (take number code)
          dotcodes | length code > number = codes ++ [iStr "..."]
                   | otherwise            = codes

-- SGM Mark4で追加
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
showNode s a (NInd n) = iConcat [iStr "Ind ", iStr(showaddr n)]  -- SGM Mark2で追加
showNode s a (NConstr t as)  -- SGM Mark6で追加
  = iConcat [iStr "Cons ", iNum t, iStr " [",
             iInterleave (iStr ", ") (map (iStr.showaddr) as), iStr "]"]
showNode s a (NLGlobal n g id) = iConcat [iStr "Locked Global (", iNum id, iStr ") ", iStr v]
                               where v = head [n | (n,b) <- getGlobals s, a==b]  -- PGM Mark2で追加
showNode s a (NLAp a1 a2 id) = iConcat [iStr "Locked Ap (", iNum id, iStr ") ", iStr (showaddr a1),
                                        iStr " ", iStr (showaddr a2)]  -- PGM Mark2で追加

showContent :: GmState -> Addr -> Node -> Iseq
showContent s a (NNum n) = iNum n
showContent s a (NGlobal n g) = iStr $ head [n | (n,b) <- getGlobals s, a==b]
showContent s a (NAp a1 a2) = iConcat [iStr "(",
                                       showContent s a1 (hLookup (getHeap s) a1),
                                       iStr " ",
                                       showContent s a2 (hLookup (getHeap s) a2),
                                       iStr ")"]
showContent s a (NInd n) = iConcat [iStr "#(",
                                    showContent s n (hLookup (getHeap s) n),
                                    iStr ")"]
showContent s a (NConstr t as) = iStr "----"
showContent s a (NLGlobal n g id) = iConcat [iStr "L", iNum id, iStr "(",
                                             iStr (head [n | (n,b) <- getGlobals s, a==b]),
                                             iStr ")"]
showContent s a (NLAp a1 a2 id) = iConcat [iStr "L", iNum id, iStr "(",
                                           showContent s a1 (hLookup (getHeap s) a1),
                                           iStr " ",
                                           showContent s a2 (hLookup (getHeap s) a2),
                                           iStr ")"]

-- PGM Mark1で変更
showStats :: PgmState -> Iseq
showStats pgmstate
  = iConcat [ iStr "Steps taken :[",
              iIndent (iInterleave iNewline
                      (map iNum (pgmGetStats pgmstate))),
              iStr "]"]

-- PGM Mark1で追加
showSparks :: GmSparks -> Iseq
showSparks sparks
  = iConcat [iStr " Sparks:[",
             iIndent (iInterleave iNewline
                     (map subFuncForShowSparks sparks)),
             iStr "]"]
    where
      subFuncForShowSparks (addr, idnum)
        = iConcat [iStr "(", iNum addr, iStr ", ", iNum idnum, iStr ")"]

-- PGM Mark1で追加
showClocks :: GmState -> Iseq
showClocks s
  = iConcat [iStr " Clocks:[",
             iNum (getClock s),
             iStr "]"]
--------------------------
-- 結果の表示 (ここまで) --
--------------------------


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
                   "(if (n==1) 1 " ++  -- 内側の if は、自分の実装では、正しい結果を得るためには括弧で囲む必要がありそう。(仕様として括弧が必要かは未確認。)
                   "(nfib (n-1) + nfib (n-2))) ;" ++
          "main = nfib 8"
          {-
            b_3_2_3 だと、
            nfib 0 = 0 (26ステップ)
            nfib 1 = 1 (32ステップ)
            nfib 2 = 1 (107ステップ)
            nfib 3 = 2 (191ステップ)
            nfib 4 = 3 (352ステップ)
            nfib 5 = 5 (597ステップ)
            nfib 6 = 8 (1003ステップ)
            nfib 7 = 13 (1654ステップ)
            nfib 8 = 21 (2711ステップ)→(7875ステップ)　※"if"がCondからCasejumpに置き換えられたことによる。
            nfib 9 = 34 (未確認)
            nfib 10 = 55 (未確認)
          -}
b_3_2_3' = "nfib n = if (n<2) 1 " ++
                    "(nfib (n-1) + nfib (n-2)) ;" ++
           "main = nfib 10"  -- このように書き換えたら、正しい値89が得られた。かかったステップは6184ステップ→14771ステップ(Cond→Casejump)。

test_program2 =
  "True  = Pack{2,0} ; " ++
  "False = Pack{1,0} ; " ++
  "isThree x = if (x == 3) True False ; " ++
  "main = isThree 3"

test_program3 =
  "True  = Pack{2,0} ; " ++
  "False = Pack{1,0} ; " ++
  "isThree x = if (x == 3) True False ; " ++
  "main = isThree 2"

test_program4 =
  "cons = Pack{2,2} ; " ++
  "nil  = Pack{1,0} ; " ++
  "main = cons 10 nil"

test_program5 =
  "True  = Pack{2,0} ; " ++
  "False = Pack{1,0} ; " ++
  "isThree x = if True x (x+1) ; " ++
  "main = isThree 3"

test_program5' =
  "True  = Pack{2,0} ; " ++
  "False = Pack{1,0} ; " ++
  "isThree x = if False x (x+1) ; " ++
  "main = isThree 3"

test_program6 =
  "nil   = Pack{1,0} ; " ++
  "cons  = Pack{2,2} ; " ++
  "main = nil"

test_program7 =
  "nil   = Pack{1,0} ; " ++
  "cons  = Pack{2,2} ; " ++
  "main = cons 1 nil"

test_program8 =
  "nil   = Pack{1,0} ; " ++
  "cons x xs = Pack{2,2} x xs ; " ++
  "main = cons 104 (cons 102 nil)"

-- データ構造のテスト
b_4_1 = "cons = Pack{2,2} ; " ++
        "nil = Pack{1,0} ; " ++
        "downfrom n = if (n == 0) " ++
        "             nil " ++
        "             (cons n (downfrom (n-1))) ; " ++
        "main = downfrom 4"

b_4_1' = "cons x xs = Pack{2,2} x xs ; " ++
         "nil = Pack{1,0} ; " ++
         "downfrom n = if (n == 0) " ++
         "             nil " ++
         "             (cons n (downfrom (n-1))) ; " ++
         "main = downfrom 4"

b_4_2 = "cons = Pack{2,2} ; " ++
        "nil = Pack{1,0} ; " ++
        "downfrom n = if (n == 0) " ++
        "             nil " ++
        "             (cons n (downfrom (n-1))) ; " ++
        "length xs = case xs of " ++
        "                   <1> -> 0; " ++
        "                   <2> y ys -> 1 + length ys ; " ++
        "main = length (downfrom 4)"

test_program_3_8_7_1  = "f x = Pack{2,2} (case x of <1> -> 1; <2> -> 2) Pack{1,0}"

test_program_3_8_7_1' = "f x = Pack{2,2} (g x) Pack{1,0} ; " ++
                        "g x = case x of <1> -> 1; <2> -> 2"

test_program_3_8_7_2  = "prefix p xs = map (Pack{2,2} p) xs"

test_program_3_8_7_2' = "prefix p xs = map (f p) xs ; " ++
                        "f p x = Pack{2,2} p x"

test_program_for_boolean1 = "main = False"

test_program_for_boolean2 = "main = True"

test_program_for_and1 = "main = False & False"

test_program_for_and2 = "main = False & True"

test_program_for_and3 = "main = True & False"

test_program_for_and4 = "main = True & True"

test_program_for_and5 = "main = (1 == 1) & (1 == 2)"

test_program_for_and6 = "main = (1 == 1) & (1 == 1)"

test_program_for_or1 = "main = False | False"

test_program_for_or2 = "main = False | True"

test_program_for_or3 = "main = True | False"

test_program_for_or4 = "main = True | True"

test_program_for_or5 = "main = (1 == 1) | (1 == 2)"

test_program_for_or6 = "main = (1 == 3) | (1 == 2)"

test_program_for_not1 = "main = not False"

test_program_for_not2 = "main = not True"

test_program_for_not3 = "main = not (1 == 3)"

test_program_for_not4 = "main = not (1 == 1)"

test_program_for_and_or_not = "main = not (1 == 3 & 1 == 1) | (1 == 2)"

test_program_for_and_or_not' = "main = (1 == 3 & 1 == 1) | (1 == 2)"

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

ex_5_6_1 = "main = par (S K K) (S K K 3)"

ex_5_6_2 = "main = S K K (S K K 3)"

ex_5_7_1 = "main = par I (I 3)"

ex_5_7_2 = "main = I (I 3)"

ex_5_10 = "twice_ f x = par f (f x) ; " ++
          "main = twice_ (twice_ (twice_ (S K K))) 3"

ex_5_10_2 = "twice_ f x = par f (f x) ; " ++
            "inc x = x + 1 ; " ++
            "main = twice_ (twice_ (twice_ inc)) 0"

ex_5_10_3 = "twice_ f x = f (f x) ; " ++
            "inc x = x + 1 ; " ++
            "main = twice_ (twice_ (twice_ inc)) 0"
---------------------------------
-- テストプログラム (ここまで) --
---------------------------------

main :: IO()
-- main = (putStrLn . runProg) ex_5_10_2
main = (putStrLn . runProg) ex_5_10
-- main = (putStrLn . runProg) ex_4_20_2_3_4
