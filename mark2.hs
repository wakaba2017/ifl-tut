module Template where
import Language
import Utils

-- 状態を表す型の定義
type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

-- スタックを表す型の定義
type TiStack = [Addr]

-- ダンプを表す型の定義
data TiDump = DummyTiDump
              deriving (Show)

initialTiDump = DummyTiDump

-- ヒープを表す型の定義
type TiHeap = Heap Node

data Node = NAp Addr Addr                   -- Application
          | NSupercomb Name [Name] CoreExpr -- Supercombinator
          | NNum Int                        -- A number
          deriving (Show)

-- グローバルを表す型の定義
type TiGlobals = ASSOC Name Addr

-- 統計情報を表す型の定義
type TiStats = Int

tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s + 1

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

compile :: CoreProgram -> TiState
compile program
  = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
    where
      sc_defs                 = program ++ preludeDefs ++ extraPreludeDefs
      (initial_heap, globals) = buildInitialHeap sc_defs
      initial_stack           = [address_of_main]
      address_of_main         = aLookup globals "main" (error "main is not defined")

extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs
  = mapAccuml allocateSc hInitial sc_defs

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body)
  = (heap', (name, addr))
    where
      (heap', addr) = hAlloc heap (NSupercomb name args body)

eval :: TiState -> [TiState]
eval state
  = state : rest_states
    where
      rest_states | tiFinal state = []
                  | otherwise     = eval next_state
      next_state  = doAdmin (step state)

tiFinal :: TiState -> Bool
tiFinal ([sole_addr], dump, heap, globals, stats) = isDataNode (hLookup heap sole_addr)
tiFinal ([],          dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False --Stack contains more than one item

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node     = False

doAdmin :: TiState -> TiState
doAdmin state
  = applyToStats tiStatIncSteps state

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats)
  = (stack, dump, heap, sc_defs, stats_fun stats)

-- ある状態を、その後続状態に写像する関数 step
-- BEGIN
step :: TiState -> TiState
step state
  = dispatch (hLookup heap (hd stack))
      where
        (stack, dump, heap, globals, stats) = state
        dispatch (NNum n)                   = numStep state n
        dispatch (NAp a1 a2)                = apStep  state a1 a2  -- 状態遷移ルール 2.1
        dispatch (NSupercomb sc args body)  = scStep  state sc args body -- 状態遷移ルール 2.2
-- END

numStep :: TiState -> Int -> TiState
numStep state n
  = error "Number applied as a function!"

-- 状態遷移ルール 2.1 を行う関数 apStep
-- BEGIN
apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2
  = (a1 : stack, dump, heap, globals, stats)
-- END

-- 状態遷移ルール 2.2 を行う関数 scStep
-- BEGIN
scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body
  = (new_stack, dump, new_heap, globals, stats)
    where
      new_stack               = result_addr : (drop (length arg_names + 1) stack)
      (new_heap, result_addr) = instantiate body heap env
      env                     = arg_bindings ++ globals
      arg_bindings            = zip2 arg_names (getargs heap stack)
-- END

-- スーバーコンビネータをインスタンス化する関数 instantiate
-- BEGIN
instantiate :: CoreExpr           -- Body of supercombinator
               -> TiHeap          -- Heap before instantiation
               -> ASSOC Name Addr -- Association of names to addresses
               -> (TiHeap, Addr)  -- Heap after instantiation, and address of root of instance
instantiate (ENum n)               heap env = hAlloc heap  (NNum n)
instantiate (EAp e1 e2)            heap env = hAlloc heap2 (NAp a1 a2)
                                              where
                                                (heap1, a1) = instantiate e1 heap  env
                                                (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v)               heap env = (heap, aLookup env v (error ("Undefined name " ++ show v)))
instantiate (EConstr tag arity)    heap env = instantiateConstr tag arity heap env
instantiate (ECase e alts)         heap env = error "Can't instantiate case exprs"
{-
Pattern match is redundant の問題は、

instantiate (ELet recursive defs body) heap env
  = instantiate body newheap newenv
      ...

instantiate (ELet nonRecursive defs body) heap env
  = instantiate body newheap newenv
      ...

の様にしていたことが原因だった。
(recursiveとnonRecursiveがただの仮引数とみなされて、TrueとFalseの置き換えということが認識されず、同一パターンと認識されてしまった模様。)
とりあえず、

  instantiate (ELet isRec defs body) heap env
    | isRec == nonRecursive
      = instantiate body newheap newenv
        ...

  instantiate (Elet isRec defs body) heap env
    | isRec == recursive
      = instantiate body newheap newenv
        ...

とすることで問題を解消できた。
-}

instantiate (ELet isRec defs body) heap env
  | isRec == recursive
    = instantiate body newheap newenv
      where
        {-
          letrec対応版は、山下様の実装を、letrec専用にして引用させていただいた。
        -}
        (newheap, extraBindings) = mapAccuml instantiateRhs heap defs
        {-
          mapAccuml                :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c]) は、
          instantiateRhs           :: (TiHeap -> (String, Expr String) -> (TiHeap, (String, Int))) に、
          heap                     :: TiHeap と、
          defs                     :: [(String, Expr String)] の要素を一つずつを渡して、
          (newheap, extraBindings) :: (TiHeap, [(String, Int)]) を得る。
        -}
        newenv = extraBindings ++ env
        instantiateRhs heap (name, rhs)  -- instantiateRhs :: TiHeap -> (String, Expr String) -> (TiHeap, (String, Int))
          = (heap1, (name, addr))
            where
              (heap1, addr) =instantiate rhs heap newenv
              {-
                instantiate   :: Expr String -> TiHeap -> [(String, Int)] -> (TiHeap, Int) は、
                rhs           :: Expr String と、
                heap          :: TiHeap と、
                newenv        :: [(String, Int)] を受け取り、
                (heap1, addr) :: (TiHeap, Int) を返す。
              -}

instantiate (ELet isRec defs body) heap env
  | isRec == nonRecursive
    = instantiate body newheap newenv
    where
        {-
          山下様の実装では、let/letrecr両方に対応されているが、とりあえず現状はletrec専用として引用させていただき、let用はそのまま残した。
        -}
      auxfunc [] heap env = (heap, env)
      auxfunc (def : defs) heap env
        = auxfunc defs heap' env'
          where
            (heap', addr) = instantiate (snd def) heap env
            env' = ((fst def), addr) : env
      (newheap, newenv) = auxfunc defs heap env
-- END

instantiateConstr tag arity heap env
  = error "Can't instantiate constructors yet"

-- スーパーコンビネータの引数のアドレスを取得してリストにまとめて返す関数 getargs
-- BEGIN
getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc : stack)
  = map get_arg stack
    where
      get_arg addr = arg
                     where
                       (NAp fun arg) = hLookup heap addr
-- END

showResults :: [TiState] -> [Char]
showResults states
  = iDisplay (iConcat [ iLayn (map showState states),
                        showStats (last states)
                      ])

showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats)
  = iConcat [ showStack heap stack, iNewline ]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack
  = iConcat [ iStr "stk [",
              iIndent (iInterleave iNewline (map show_stack_item stack)),
              iStr " ]"
            ]
    where
      show_stack_item addr
        = iConcat [ showFWAddr addr,
                    iStr ": ",
                    showStkNode heap (hLookup heap addr)
                  ]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fun_addr arg_addr)
  = iConcat [ iStr "NAp ",
              showFWAddr fun_addr,
              iStr " ",
              showFWAddr arg_addr,
              iStr " (",
              showNode (hLookup heap arg_addr),
              iStr ")"
            ]
-- showStkNode heap node = showNode node  -- これではパースエラーとなる模様。
showStkNode heap (NSupercomb name args body) = showNode (NSupercomb name args body)
showStkNode heap (NNum n) = showNode (NNum n)

showNode :: Node -> Iseq
showNode (NAp a1 a2)
  = iConcat [ iStr "NAp ",
              showAddr a1,
              iStr " ",
              showAddr a2
            ]
showNode (NSupercomb name args body)
  = iStr ("NSupercomb " ++ name)
showNode (NNum n)
  = (iStr "NNum ") `iAppend` (iNum n)

showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq -- Show address in field of width 4
showFWAddr addr
  = iStr (space (4 - length str) ++ str)
    where
      str = show addr

showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats)
  = iConcat [ iNewline,
              iNewline,
              iStr "Total number of steps = ",
              iNum (tiStatGetSteps stats)
            ]

runProg :: [Char] -> [Char] -- name changed to not conflict
runProg = showResults . eval . compile . parse -- "run": name conflict

ex_2_11 = "pair x y f = f x y ; fst p = p K ; snd p = p K1 ; f x y = letrec a = pair x b ; b = pair y a in fst (snd (snd (snd a))) ; main = f 3 4"

sampleProg  = "oct g x = let h = twice g in let k = twice h in k (k x) ; main = oct I 4"
