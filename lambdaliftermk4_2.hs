module Lambda where
import Utils
import Language
import Data.List (isPrefixOf)
import Control.Monad.State

-------------------------------
-- データ型の定義 (ここから) --
-------------------------------
type AnnExpr a b = (b, AnnExpr' a b)

data AnnExpr' a b = AVar Name
                  | ANum Int
                  | AConstr Int Int
                  | AAp (AnnExpr a b) (AnnExpr a b)
                  | ALet Bool [AnnDefn a b] (AnnExpr a b)
                  | ACase (AnnExpr a b) [AnnAlt a b]
                  | ALam [a] (AnnExpr a b)
                  deriving (Show)

isAtomicAnnExpr :: AnnExpr a b -> Bool
isAtomicAnnExpr (ant, (AVar v)) = True
isAtomicAnnExpr (ant, (ANum n)) = True
isAtomicAnnExpr e               = False

type AnnDefn a b = (a, AnnExpr a b)

type AnnAlt a b = (Int, [a], (AnnExpr a b))

type AnnProgram a b = [(Name, [a], AnnExpr a b)]

-- ここから、ジョンソンスタイル・ラムダリフタの定義
type M a = State NameSupply a

type UnusedArg = (Name, [Int])
type UnusedArgs = [UnusedArg]

-- ここから、完全遅延評価ラムダリフタの定義
type Level = Int
type FloatedDefns = [(Level, IsRec, [(Name, Expr Name)])]
type ArityEnv = ASSOC Name Int  -- Int -> Maybe Int ?
-- type ArityEnv = ASSOC Name (Maybe Int)  -- これだと NG になった。
-------------------------------
-- データ型の定義 (ここまで) --
-------------------------------

----------------------------
-- pprint 関連 (ここまで) --
----------------------------
pprint :: CoreProgram -> String
pprint prog = pprintGen iStr prog

pprintGen :: (a -> Iseq)   -- function from binders to iseq
             -> Program a  -- the program to be formatted
             -> [Char]     -- result string
pprintGen ppr prog
  = iDisplay (pprProgramGen ppr prog)

pprProgramGen :: (a -> Iseq)   -- function from binders to iseq
                 -> Program a
                 -> Iseq
pprProgramGen ppr prog
  = iInterleave sep scDefns
    where
      sep = iConcat [iStr " ; ", iNewline]
      scDefns = map (pprScDefnGen ppr) prog

pprScDefnGen :: (a -> Iseq)   -- function from binders to iseq
                -> ScDefn a
                -> Iseq
pprScDefnGen ppr (name, args, expr)
  = iConcat [ iStr name,
      iStr " ",
      iInterleave (iStr " ") (map ppr args),
      iStr " = ",
      pprExprGen ppr expr
    ]

pprExprGen :: (a -> Iseq)   -- function from binders to iseq
              -> Expr a
              -> Iseq
pprExprGen ppr (ENum n)
  = iStr (show n)
pprExprGen ppr (EVar v)
  = iStr v
pprExprGen ppr (EAp (EAp (EVar op) e1) e2)
  | op `elem` op_list = iConcat [ pprAExprGen ppr e1,
                                  iStr " ", iStr op, iStr " ",
                                  pprAExprGen ppr e2
                                ]
  | otherwise = (pprExprGen ppr (EAp (EVar op) e1)) `iAppend` (iStr " ") `iAppend` (pprAExprGen ppr e2)
  where
    op_list = ["+", "-", "*", "/", "==", "~=", ">=", ">", "<=", "<", "&", "|"]
pprExprGen ppr (EAp e1 e2)
  = (pprExprGen ppr e1) `iAppend` (iStr " ") `iAppend` (pprAExprGen ppr e2)
pprExprGen ppr (ELet isrec defns expr)
  = iConcat [ iStr keyword,
              iNewline,
              iStr " ",
              iIndent (pprDefnsGen ppr defns),
              iNewline,
              iStr "in ",
              pprExprGen ppr expr
            ]
            where
              keyword | not isrec = "let"
                      | isrec     = "letrec"
pprExprGen ppr (ECase cond alts)
  = iConcat [ iStr "case ",
              pprExprGen ppr cond,
              iStr " of",
              iNewline,
              iIndent (pprCoreAltsGen ppr alts)
            ]
pprExprGen ppr (ELam lvs body)
  = iConcat [ iStr "\\",
              iInterleave (iStr " ") (map ppr lvs),
              iStr ". ",
              pprExprGen ppr body
            ]
pprExprGen ppr (EConstr tag arity)
  = iConcat [ iStr "Pack{",
              iNum tag,
              iStr " ",
              iNum arity,
              iStr "}"
            ]

pprAExprGen :: (a -> Iseq)   -- function from binders to iseq
               -> Expr a
               -> Iseq
pprAExprGen ppr e | isAtomicExpr e = pprExprGen ppr e
                  | otherwise      = iConcat [ iStr "(",
                                               pprExprGen ppr e,
                                               iStr ")"
                                             ]

pprDefnsGen :: (a -> Iseq)   -- function from binders to iseq
               -> [(a, Expr a)]
               -> Iseq
pprDefnsGen ppr defns
  = iInterleave sep (map (pprDefnGen ppr) defns)
    where
      sep = iConcat [ iStr ";", iNewline ]

pprDefnGen :: (a -> Iseq)   -- function from binders to iseq
              -> (a, Expr a)
              -> Iseq
pprDefnGen ppr (name, expr)
  = iConcat [ -- iStr name, iStr " = ",
              ppr name, iStr " = ",
              iIndent (pprExprGen ppr expr)
            ]

pprCoreAltsGen :: (a -> Iseq)   -- function from binders to iseq
                  -> [Alter a]
                  -> Iseq
pprCoreAltsGen ppr alts
  = iInterleave sep (map (pprCoreAltGen ppr) alts)
    where
     sep = iNewline

pprCoreAltGen :: (a -> Iseq)   -- function from binders to iseq
                 -> Alter a
                 -> Iseq
pprCoreAltGen ppr (tag, lvs, expr)
  = iConcat [ iStr "<", iNum tag, iStr "> ",
              iInterleave (iStr " ") (map ppr lvs),
              iStr " -> ",
              pprExprGen ppr expr
            ]

pprintAnn :: (a -> Iseq)        -- function from binders to iseq
             -> (b -> Iseq)     -- function from annotations to iseq
             -> AnnProgram a b  -- program to be displayed
             -> [Char]          -- result string
pprintAnn pprbnd pprant prog
  = iDisplay (pprAnnProgram pprbnd pprant prog)

iSet :: Set Name -> Iseq
iSet set
  = iInterleave (iStr ",") tmpList
    where
      tmpList = map iStr $ setToList set

{-
-- 使用例 --
*Lambda> (putStrLn . pprintAnn iStr iSet . freeVars . parse) test_program_1
f x = [x](let
 g = [x](\y. [x,y]([x]([](+) ([x]([x]([](*) [x](x)) [x](x)))) [y](y)))
in [g]([g]([](+) ([g]([g](g) [](3)))) ([g]([g](g) [](4))))) ;
main  = []([](f) [](6))
-}

pprAnnProgram :: (a -> Iseq)        -- function from binders to iseq
                 -> (b -> Iseq)     -- function from annotations to iseq
                 -> AnnProgram a b  -- program to be displayed
                 -> Iseq            -- result sequence
pprAnnProgram pprbnd pprant prog
  = iInterleave sep annDefns
    where
      sep = iConcat [iStr " ; ", iNewline]
      annDefns = map (pprAnnScDefn pprbnd pprant) prog

pprAnnScDefn :: (a -> Iseq)        -- function from binders to iseq
                -> (b -> Iseq)     -- function from annotations to iseq
                -> (Name, [a], AnnExpr a b)
                -> Iseq
pprAnnScDefn pprbnd pprant (name, args, expr)
  = iConcat [ iStr name,
              iStr " ",
              iInterleave (iStr " ") (map pprbnd args),
              iStr " = ",
              pprAnnExpr pprbnd pprant expr
            ]

pprAnnExpr :: (a -> Iseq)        -- function from binders to iseq
              -> (b -> Iseq)     -- function from annotations to iseq
              -> AnnExpr a b
              -> Iseq
pprAnnExpr pprbnd pprant (ant, ANum n)
  = iConcat [ iStr "[", pprant ant, iStr "](",
              iStr (show n),
              iStr ")"
            ]
pprAnnExpr pprbnd pprant (ant, AVar v)
  = iConcat [ iStr "[", pprant ant, iStr "](",
              iStr v,
              iStr ")"
            ]
pprAnnExpr pprbnd pprant (ant, (AAp e1 e2))
  = iConcat [ iStr "[", pprant ant, iStr "](",
              (pprAnnExpr pprbnd pprant e1) `iAppend` (iStr " ") `iAppend` (pprAnnAExpr pprbnd pprant e2),
              iStr ")"
            ]
pprAnnExpr pprbnd pprant (ant, (ALet isrec defns expr))
  = iConcat [ iStr "[", pprant ant, iStr "](",
              iStr keyword,
              iNewline,
              iStr " ",
              iIndent (pprAnnDefns pprbnd pprant defns),
              iNewline,
              iStr "in ",
              pprAnnExpr pprbnd pprant expr,
              iStr ")"
            ]
            where
              keyword | not isrec = "let"
                      | isrec     = "letrec"
pprAnnExpr pprbnd pprant (ant, (ACase cond alts))
  = iConcat [ iStr "[", pprant ant, iStr "](",
              iStr "case ",
              pprAnnExpr pprbnd pprant cond,
              iStr " of",
              iNewline,
              iIndent (pprAnnAlts pprbnd pprant alts),
              iStr ")"
            ]
pprAnnExpr pprbnd pprant (ant, (ALam lvs body))
  = iConcat [ iStr "[", pprant ant, iStr "](",
              iStr "\\",
              iInterleave (iStr " ") (map pprbnd lvs),
              iStr ". ",
              pprAnnExpr pprbnd pprant body,
              iStr ")"
            ]
pprAnnExpr pprbnd pprant (ant, AConstr tag arity)
  = iConcat [ iStr "[", pprant ant, iStr "],(",
              iStr "Pack{",
              iNum tag,
              iStr " ",
              iNum arity,
              iStr "}",
              iStr ")"
            ]

pprAnnAExpr :: (a -> Iseq)        -- function from binders to iseq
               -> (b -> Iseq)     -- function from annotations to iseq
               -> AnnExpr a b
               -> Iseq
pprAnnAExpr pprbnd pprant e
  | isAtomicAnnExpr e = pprAnnExpr pprbnd pprant e
  | otherwise         = iConcat [ iStr "(",
                                  pprAnnExpr pprbnd pprant e,
                                  iStr ")"
                                ]

pprAnnDefns :: (a -> Iseq)        -- function from binders to iseq
               -> (b -> Iseq)     -- function from annotations to iseq
               -> [AnnDefn a b]
               -> Iseq
pprAnnDefns pprbnd pprant defns
  = iInterleave sep (map (pprAnnDefn pprbnd pprant) defns)
    where
      sep = iConcat [ iStr ";", iNewline ]

pprAnnDefn :: (a -> Iseq)        -- function from binders to iseq
              -> (b -> Iseq)     -- function from annotations to iseq
              -> AnnDefn a b
              -> Iseq
pprAnnDefn pprbnd pprant (bnd, expr)
  = iConcat [ pprbnd bnd, iStr " = ",
              iIndent (pprAnnExpr pprbnd pprant expr)
            ]

pprAnnAlts :: (a -> Iseq)        -- function from binders to iseq
              -> (b -> Iseq)     -- function from annotations to iseq
              -> [AnnAlt a b]
              -> Iseq
pprAnnAlts pprbnd pprant alts
  = iInterleave sep (map (pprAnnAlt pprbnd pprant) alts)
    where
     sep = iNewline

pprAnnAlt :: (a -> Iseq)        -- function from binders to iseq
             -> (b -> Iseq)     -- function from annotations to iseq
             -> AnnAlt a b
             -> Iseq
pprAnnAlt pprbnd pprant (tag, lvs, expr)
  = iConcat [ iStr "<", iNum tag, iStr "> ",
              iInterleave (iStr " ") (map pprbnd lvs),
              iStr " -> ",
              pprAnnExpr pprbnd pprant expr
            ]
----------------------------
-- pprint 関連 (ここまで) --
----------------------------

-------------------------
-- 全体構造 (ここから) --
-------------------------
lambdaLift :: CoreProgram -> CoreProgram
lambdaLift = collectSCs . rename . abstract . freeVars

-- runS = pprint . lambdaLift . parse
runS = Lambda.pprint . lambdaLift . parse

-- ここから、ジョンソンスタイル・ラムダリフタの定義
initialUnusedInfo :: UnusedArgs
initialUnusedInfo = []

lambdaLiftJ prog
  = let
      coreWithSCs
        = evalState (do
            prog1 <- renameM prog       -- State モナド版 rename
            let ann = freeVars prog1    -- ここは純粋関数のままでOK
            prog2 <- abstractJM ann     -- State モナド版 abstractJ
            pure prog2
          ) initialNameSupply
      scs = collectSCs coreWithSCs
      (unusedInfo, scs') = collectUnusedArgs scs
      scs'' = rewriteEAp unusedInfo scs'
    in
      scs''

-- runJ = pprint . lambdaLiftJ . parse
runJ = Lambda.pprint . lambdaLiftJ . parse

-- ここから、完全遅延評価ラムダリフタの定義
-- fullyLazyLift = float . renameL . identifyMFEs . addLevels . separateLams
fullyLazyLift :: CoreProgram -> CoreProgram
fullyLazyLift prog
  = let
      -- ① まだカリー化されていない段階で arityEnv を作る
      arityEnv = buildScArity prog
      -- ② いつものパイプライン（ただし identifyMFEs には arityEnv を渡す）
      annProg  = addLevels (separateLams prog)
      progWithMFEs = identifyMFEs arityEnv annProg
      -- ③ その後は従来どおり
      lifted = renameL progWithMFEs
    in
      float lifted

buildScArity :: CoreProgram -> ArityEnv
buildScArity prog
  = [ (name, length args)
    | (name, args, _rhs) <- prog
    ]

-- runF          = pprint . lambdaLift . fullyLazyLift . parse
runF          = Lambda.pprint . lambdaLift . fullyLazyLift . parse
-------------------------
-- 全体構造 (ここまで) --
-------------------------

------------------------------
-- freeVars 関連 (ここから) --
------------------------------
freeVars :: CoreProgram -> AnnProgram Name (Set Name)
freeVars prog = [ (name, args, freeVars_e (setFromList args) body)
                | (name, args, body) <- prog
                ]

freeVars_e :: (Set Name)                 -- Candidates for free variables
              -> CoreExpr                -- Expression to annotate
              -> AnnExpr Name (Set Name) -- Annotated result
freeVars_e lv (ENum k)
  = (setEmpty, ANum k)
freeVars_e lv (EVar v)
  | setElementOf v lv = (setSingleton v, AVar v)
  | otherwise         = (setEmpty, AVar v)
freeVars_e lv (EAp e1 e2)
  = (setUnion (freeVarsOf e1') (freeVarsOf e2'), AAp e1' e2')
    where e1' = freeVars_e lv e1
          e2' = freeVars_e lv e2
freeVars_e lv (ELam args body)
  = (setSubtraction (freeVarsOf body') (setFromList args), ALam args body')
    where body'  = freeVars_e new_lv body
          new_lv = setUnion lv (setFromList args)
freeVars_e lv (ELet is_rec defns body)
  = (setUnion defnsFree bodyFree, ALet is_rec defns' body')
    where binders      = bindersOf defns
          binderSet    = setFromList binders
          body_lv      = setUnion lv binderSet
          rhs_lv | is_rec    = body_lv
                 | otherwise = lv
          rhss'        = map (freeVars_e rhs_lv) (rhssOf defns)
          defns'       = zip2 binders rhss'
          freeInValues = setUnionList (map freeVarsOf rhss')
          defnsFree | is_rec    = setSubtraction freeInValues binderSet
                    | otherwise = freeInValues
          body'        = freeVars_e body_lv body
          bodyFree     = setSubtraction (freeVarsOf body') binderSet
freeVars_e lv (ECase e alts)
  = freeVars_case lv e alts
freeVars_e lv (EConstr t a)
  = (setEmpty, AConstr t a)

freeVars_case :: (Set Name)                 -- Candidates for free variables
                 -> CoreExpr                -- Expression to annotate
                 -> [CoreAlt]               -- Expression to annotate
                 -> AnnExpr Name (Set Name) -- Annotated result
freeVars_case lv e alts
  = (setUnion cndFree altsFree, ACase cnd' alts')
    where cnd' = freeVars_e lv e
          cndFree = freeVarsOf cnd'
          new_lv = setUnion lv (freeVarsOf cnd')
          alts' = map (freeVars_alt new_lv) alts
          altsFree = setUnionList $ map freeVarsOf_alt alts'

freeVars_alt :: (Set Name)                -- Candidates for free variables
                -> CoreAlt                -- Expression to annotate
                -> AnnAlt Name (Set Name) -- Annotated result
freeVars_alt lv (tag, args, rhs)
  = (tag, args, freeVars_e lv rhs)

freeVarsOf :: AnnExpr Name (Set Name) -> Set Name
freeVarsOf (free_vars, expr) = free_vars

freeVarsOf_alt :: AnnAlt Name (Set Name) -> Set Name
freeVarsOf_alt (tag, args, rhs)
  = setSubtraction (freeVarsOf rhs) (setFromList args)
------------------------------
-- freeVars 関連 (ここまで) --
------------------------------

------------------------------
-- abstract 関連 (ここから) --
------------------------------
abstract :: AnnProgram Name (Set Name) -> CoreProgram
abstract prog = [ (sc_name, args, abstract_e rhs)
                | (sc_name, args, rhs) <- prog
                ]

abstract_e :: AnnExpr Name (Set Name) -> CoreExpr
abstract_e (free, AVar v)
  = EVar v
abstract_e (free, ANum k)
  = ENum k
abstract_e (free, AAp e1 e2)
  = EAp (abstract_e e1) (abstract_e e2)
abstract_e (free, ALet is_rec defns body)
  = ELet is_rec [(name, abstract_e body) | (name, body) <- defns] (abstract_e body)
abstract_e (free, ALam args body)
  = foldll EAp sc (map EVar fvList)
    where
      fvList = setToList free
      sc     = ELet nonRecursive [("sc", sc_rhs)] (EVar "sc")
      sc_rhs = ELam (fvList ++ args) (abstract_e body)
abstract_e (free, AConstr t a)
  = EConstr t a
abstract_e (free, ACase e alts)
  = abstract_case free e alts

abstract_case free e alts
  = ECase e' alts'
    where
      (annCond, exprCond) = e  -- case 式の条件式部分を、自由変数候補集合と計算式部分に分解
      e' = abstract_e (setUnion free annCond, exprCond)  -- 引数 free と条件式の自由変数候補集合の和集合と、条件式の計算式部分を組み合わせて再帰処理
      alts' = map abstract_alt alts
      abstract_alt (tag, args, rhs)
        = (tag, args, abstract_e (freeAlt, exprAlt))  -- 新たに構成し直した自由変数集合と、ブランチの計算式部分を組み合わせて再帰処理
          where
            (annAlt, exprAlt) = rhs  -- case 式のブランチ部分を、自由変数候補集合と計算式部分に分解
            freeBase = setUnion free annCond  -- 念のため、case 式の条件式に現れる自由変数が、ブランチの式に影響する可能性を考慮
            freeAlt = setSubtraction (setUnion freeBase annAlt) (setFromList args)
              -- 引数 free と case 式の条件式とブランチの自由変数候補集合の和集合とブランチの束縛変数の差分を自由変数候補集合とする

-- ここから、ジョンソンスタイル・ラムダリフタの定義
abstractJM :: AnnProgram Name (Set Name) -> M CoreProgram
abstractJM prog
  = mapM abstractOne prog
    where
      abstractOne (name, args, rhs)
        = do
            body' <- abstractJ_eM [] rhs
            pure (name, args, body')

abstractJ_eM :: ASSOC Name [Name]         -- Maps each new SC to the free vars of its group
             -> AnnExpr Name (Set Name)   -- Input expression
             -> M CoreExpr                -- Result expression
abstractJ_eM env (free, ANum n)
  = pure (ENum n)
abstractJ_eM env (free, AConstr t a)
  = pure (EConstr t a)
abstractJ_eM env (free, AAp e1 e2)
  = do
      e1' <- abstractJ_eM env e1
      e2' <- abstractJ_eM env e2
      pure (EAp e1' e2')
abstractJ_eM env (free, AVar g)
  = pure (foldll EAp (EVar g) (map EVar (aLookup env g [])))
abstractJ_eM env (free, ALam args body)
  = do
      let fv_list = actualFreeList env free
      scName <- newSCName
      body'  <- abstractJ_eM env body
      let sc_rhs = ELam (fv_list ++ args) body'
          sc     = ELet nonRecursive [(scName, sc_rhs)] (EVar scName)
      pure (foldll EAp sc (map EVar fv_list))
abstractJ_eM env (free, ALet isrec defns body)
  = do
      let fun_defns = [(name,rhs) | (name,rhs) <- defns, isALam rhs]
          var_defns = [(name,rhs) | (name,rhs) <- defns, not (isALam rhs)]
          fun_names = bindersOf fun_defns
          free_in_funs =
            setSubtraction
              (setUnionList [freeVarsOf rhs | (name,rhs) <- fun_defns])
              (setFromList fun_names)
          vars_to_abstract = actualFreeList env free_in_funs
          body_env = [(fun_name, vars_to_abstract) | fun_name <- fun_names] ++ env
          rhs_env | isrec   = body_env
                  | otherwise = env
      fun_defns' <- mapM
        (\(name, (freeLam, ALam args bodyLam)) ->
            do
              bodyLam' <- abstractJ_eM rhs_env bodyLam
              pure (name, ELam (vars_to_abstract ++ args) bodyLam')
        )
        fun_defns
      var_defns' <- mapM
        (\(name, rhs) ->
            do
              rhs' <- abstractJ_eM rhs_env rhs
              pure (name, rhs')
        )
        var_defns
      body' <- abstractJ_eM body_env body
      pure (ELet isrec (fun_defns' ++ var_defns') body')
abstractJ_eM env (free, ACase e alts)
  = abstractJ_caseM env free e alts

abstractJ_caseM :: ASSOC Name [Name]
                -> Set Name
                -> AnnExpr Name (Set Name)
                -> [AnnAlt Name (Set Name)]
                -> M CoreExpr
abstractJ_caseM env free e alts
  = do
      let (annCond, exprCond) = e
      e' <- abstractJ_eM env (setUnion free annCond, exprCond)
      alts' <- mapM (abstractJ_altM env annCond free) alts
      pure (ECase e' alts')

abstractJ_altM :: ASSOC Name [Name]
               -> Set Name  -- annCond
               -> Set Name  -- free
               -> (Int, [Name], AnnExpr Name (Set Name))
               -> M CoreAlt
abstractJ_altM env annCond free (tag, args, rhs)
  = do
      let (annAlt, exprAlt) = rhs
          freeBase = setUnion free annCond
          freeAlt  = setSubtraction (setUnion freeBase annAlt) (setFromList args)
      rhs' <- abstractJ_eM env (freeAlt, exprAlt)
      pure (tag, args, rhs')

actualFreeList :: ASSOC Name [Name] -> Set Name -> [Name]
actualFreeList env free
  = setToList (setUnionList [ setFromList (aLookup env name [name])
                            | name <- setToList free
                            ])

isALam :: AnnExpr a b -> Bool
isALam (free, ALam args body) = True
isALam other = False

newSCName :: M Name
newSCName
  = do
      ns <- get
      put (ns + 1)
      pure (makeName "sc" ns)
------------------------------
-- abstract 関連 (ここまで) --
------------------------------

----------------------------
-- rename 関連 (ここから) --
----------------------------
rename :: CoreProgram -> CoreProgram
rename prog
  = second (mapAccuml rename_sc initialNameSupply prog)
    where
      rename_sc ns (sc_name, args, rhs)
        = (ns2, (sc_name, args', rhs'))
          where
            (ns1, args', env) = newNames ns args
            (ns2, rhs') = rename_e env ns1 rhs

newNames :: NameSupply -> [Name] -> (NameSupply, [Name], ASSOC Name Name)
newNames ns old_names
  = (ns', new_names, env)
    where
      (ns', new_names) = getNames ns old_names
      env = zip2 old_names new_names

rename_e :: ASSOC Name Name           -- Binds old names to new
            -> NameSupply             -- Name supply
            -> CoreExpr               -- Input expression
            -> (NameSupply, CoreExpr) -- Depleted supply and result
rename_e env ns (EVar v)
  = (ns, EVar (aLookup env v v))
rename_e env ns (ENum n)
  = (ns, ENum n)
rename_e env ns (EAp e1 e2)
  = (ns2, EAp e1' e2')
    where
      (ns1, e1') = rename_e env ns e1
      (ns2, e2') = rename_e env ns1 e2
rename_e env ns (ELam args body)
  = (ns2, ELam args' body')
    where
      (ns1, args', env') = newNames ns args
      (ns2, body') = rename_e (env' ++ env) ns1 body
rename_e env ns (ELet is_rec defns body)
  = (ns3, ELet is_rec (zip2 binders' rhss') body')
    where
      (ns1, body') = rename_e body_env ns body
      binders = bindersOf defns
      (ns2, binders', env') = newNames ns1 binders
      body_env = env' ++ env
      (ns3, rhss') = mapAccuml (rename_e rhsEnv) ns2 (rhssOf defns)
      rhsEnv | is_rec = body_env
             | otherwise = env
rename_e env ns (EConstr t a)
  = (ns, EConstr t a)
rename_e env ns (ECase e alts)
  = rename_case env ns e alts

rename_case env ns e alts
  = (ns_final, ECase e' alts')
    where
      (ns1, e') = rename_e env ns e
      (ns_final, alts') = rename_alts ns1 alts
      rename_alts ns [] = (ns, [])
      rename_alts ns ((tag, args, rhs) : alts)
        = (ns3, (tag, args', rhs') : alts')
          where
            (ns1, args', env') = newNames ns args
            rhs_env = env' ++ env
            (ns2, rhs') = rename_e rhs_env ns1 rhs
            (ns3, alts') = rename_alts ns2 alts

testExpr = ECase (EVar "x")
                 [ (1, ["x"], EAp (EVar "x") (EVar "z"))
                 , (2, [], EVar "y") ]

-- ここから、ジョンソンスタイル・ラムダリフタの定義
renameM :: CoreProgram -> M CoreProgram
renameM prog
  = do
      ns <- get
      -- ここで「今までの rename と同じ処理」を、initialNameSupply ではなく ns から始める
      let (ns', prog') = mapAccuml rename_sc ns prog
      put ns'
      pure prog'
      where
        rename_sc ns (sc_name, args, rhs)
          = (ns2, (sc_name, args', rhs'))
            where
              (ns1, args', env) = newNames ns args
              (ns2, rhs')       = rename_e env ns1 rhs
----------------------------
-- rename 関連 (ここまで) --
----------------------------

-------------------------------
-- collectSC 関連 (ここから) --
-------------------------------
collectSCs :: CoreProgram -> CoreProgram
collectSCs prog
  = concat (map collect_one_sc prog)
    where
      collect_one_sc (sc_name, args, rhs)
        = (sc_name, args', rhs'') : scs'
          where
            (scs, rhs') = collectSCs_e rhs
            (args', rhs'', scs')
              = case rhs' of
                EVar s -> if isPrefixOf "sc_" s  -- rhs' が EVar _ の場合
                          then  -- rhs' が EVar s で、s が "sc_" で始まる場合
                            let
                              scList = [(name, args, body) | (name, args, body) <- scs, name == s]
                              (_, scArgs, scBody)
                                | length scList > 0 = hd scList
                                | otherwise = error "No applicable SC definition"
                              newScs = [(name, args, body) | (name, args, body) <- scs, name /= s]
                            in
                              (scArgs, scBody, newScs)
                          else  -- rhs' が EVar s で、s が "sc_" で始まらない場合
                            (args, rhs', scs)
                _ -> (args, rhs', scs)  -- rhs' が EVar _ でない場合

collectSCs_e :: CoreExpr -> ([CoreScDefn], CoreExpr)
collectSCs_e (ENum k)
  = ([], ENum k)
collectSCs_e (EVar v)
  = ([], EVar v)
collectSCs_e (EAp e1 e2)
  = (scs1 ++ scs2, EAp e1' e2')
    where
      (scs1, e1') = collectSCs_e e1
      (scs2, e2') = collectSCs_e e2
collectSCs_e (ELam args body)
  = (scs, ELam args body')
    where
      (scs, body') = collectSCs_e body
collectSCs_e (EConstr t a)
  = ([], EConstr t a)
collectSCs_e (ECase e alts)
  = (scs_e ++ scs_alts, ECase e' alts')
    where
      (scs_e, e') = collectSCs_e e
      (scs_alts, alts') = mapAccuml collectSCs_alt [] alts
      collectSCs_alt scs (tag, args, rhs) = (scs ++ scs_rhs, (tag, args, rhs'))
                                            where
                                              (scs_rhs, rhs') = collectSCs_e rhs
collectSCs_e (ELet is_rec defns body)
  = (rhss_scs ++ body_scs ++ local_scs, mkELet is_rec non_scs' body')
    where
      (rhss_scs, defnsTemp) = mapAccuml collectSCs_d [] defns
      (bodyTemp, defnsTemp') = mapAccuml replaceUselessLvar body defnsTemp
      defns' = [(name, rhs) | (name, rhs) <- defnsTemp, not (isEVarSc rhs)]
      scs'     = [(name, rhs) | (name, rhs) <- defns', isELam rhs]
      non_scs' = [(name, rhs) | (name, rhs) <- defns', not (isELam rhs)]
      local_scs = [(name, args, body) | (name, ELam args body) <- scs']
      (body_scs, body') = collectSCs_e bodyTemp
      collectSCs_d scs (name, rhs) = (scs ++ rhs_scs, (name, rhs'))
                                     where
                                       (rhs_scs, rhs') = collectSCs_e rhs
      replaceUselessLvar :: CoreExpr -> (Name, CoreExpr) -> (CoreExpr, (Name, CoreExpr))
      replaceUselessLvar body (name, rhs)
        | isEVarSc rhs = (replaceVar name (extractVar rhs) body, (name, rhs))
        | otherwise    = (body, (name, rhs))
        where
          extractVar (EVar v) = v
          extractVar _        = error "Expected EVar in rhs"

-- EVar name を EVar newName に置き換える関数
replaceVar :: Name -> Name -> CoreExpr -> CoreExpr
replaceVar old new expr
  = case expr of
    EVar v
      | v == old  -> EVar new
      | otherwise -> EVar v
    ENum n        -> ENum n
    ELam args body ->
      ELam args (replaceVar old new body)
    {-
    -- rename による変数名のユニーク化前にこの関数を使用する場合は以下を使用
    ELam args body
      | old `elem` args -> ELam args body  -- 置換しない
      | otherwise       -> ELam args (replaceVar old new body)
    -}
    EAp fun arg ->
      EAp (replaceVar old new fun) (replaceVar old new arg)
    ELet isRec defs body ->
      let
        defs' = [(n, replaceVar old new rhs) | (n, rhs) <- defs]
        body' = replaceVar old new body
      in
        ELet isRec defs' body'
    ECase cond alts ->
      ECase (replaceVar old new cond)
        [(tag, args, replaceVar old new altExpr) | (tag, args, altExpr) <- alts]
        {-
        -- rename による変数名のユニーク化前にこの関数を使用する場合は以下を使用
        [ if old `elem` args then (tag, args, altExpr)
                             else (tag, args, replaceVar old new altExpr)
        | (tag, args, altExpr) <- alts ]
        -}

mkELet is_rec defns body
  | length defns == 0 = body
  | otherwise = ELet is_rec defns body

isELam :: Expr a -> Bool
isELam (ELam args body) = True
isELam other = False

isEVarSc :: Expr a -> Bool
isEVarSc (EVar v)
  | isPrefixOf "sc_" v = True
  | otherwise = False
isEVarSc other = False
-------------------------------
-- collectSC 関連 (ここまで) --
-------------------------------

-- ここから、ジョンソンスタイル・ラムダリフタの定義
---------------------------------------
-- collectUnusedArgs 関連 (ここから) --
---------------------------------------
collectUnusedArgs :: CoreProgram -> (UnusedArgs, CoreProgram)
collectUnusedArgs scs
  = mapAccuml collectUnusedArgsAux initialUnusedInfo scs
    where
      collectUnusedArgsAux :: UnusedArgs -> ScDefn Name -> (UnusedArgs, ScDefn Name)
      collectUnusedArgsAux unusedInfo (sc_name, args, rhs) =
        let
          indexedArgs :: [(Int, Name)]
          indexedArgs = zip [0..] args
          -- それぞれの引数が rhs で使われているかどうか
          usedFlags :: [(Int, Name, Bool)]
          usedFlags = [ (i, a, isUsedArg a rhs)
                      | (i, a) <- indexedArgs
                      ]
          -- 残す引数
          args' :: [Name]
          args' = [ a
                  | (_i, a, used) <- usedFlags
                  , used
                  ]
          -- 未使用引数のインデックス
          unusedIdxs :: [Int]
          unusedIdxs = [ i
                       | (i, _a, used) <- usedFlags
                       , not used
                       ]
          -- 未使用引数があれば unusedInfo に追加
          unusedInfo' :: UnusedArgs
          unusedInfo' = case unusedIdxs of
                        [] -> unusedInfo
                        _  -> (sc_name, unusedIdxs) : unusedInfo
          rhs' = rhs  -- rhs には未使用引数は自由変数として出てこないので、そのままでよい
        in
          (unusedInfo', (sc_name, args', rhs'))

-- EVar name が CoreExpr に含まれるかをチェックする関数
isUsedArg :: Name -> CoreExpr -> Bool
isUsedArg arg expr
  = case expr of
    EVar v
      | v == arg  -> True
      | otherwise -> False
    ENum _ -> False
    ELam args body
      -- λarg. body の中の arg は束縛されているので、自由変数としては出現しない
      | arg `elem` args -> False
      | otherwise       -> isUsedArg arg body
    EAp fun arg' ->
      isUsedArg arg fun || isUsedArg arg arg'
    ELet isRec defs body ->
      let
        boundNames = map fst defs
        -- 定義の右辺に arg が自由変数として出現するか？
        -- ただし、定義名が arg と同じなら、その右辺での arg は束縛されるのでカウントしない
        usedInDefs = any (\(name, rhs) ->
                            if name == arg
                            then False
                            else isUsedArg arg rhs
                         ) defs
        -- 本体での出現。ただし arg が let で束縛されていれば、自由変数としては出現しない
        usedInBody
          | arg `elem` boundNames = False
          | otherwise             = isUsedArg arg body
      in
        usedInDefs || usedInBody
    ECase cond alts ->
      let
        usedInCond = isUsedArg arg cond
        usedInAlts = any (\(_tag, vars, altExpr) ->
                            -- パターン変数に arg が含まれていれば、その altExpr 内での arg は束縛される
                            if arg `elem` vars
                            then False
                            else isUsedArg arg altExpr
                         ) alts
      in
        usedInCond || usedInAlts
    EConstr _ _ -> False
---------------------------------------
-- collectUnusedArgs 関連 (ここまで) --
---------------------------------------

--------------------------------
-- rewriteEAp 関連 (ここから) --
--------------------------------
rewriteEAp :: UnusedArgs -> CoreProgram -> CoreProgram
rewriteEAp unusedInfo scs
  = [ (name, args, rewriteExpr rhs)
    | (name, args, rhs) <- scs
    ]
    where
      rewriteExpr :: CoreExpr -> CoreExpr
      rewriteExpr expr =
        case expr of
          EVar _ -> expr
          ENum _ -> expr
          EConstr _ _ -> expr
          ELam args body ->
            ELam args (rewriteExpr body)
          EAp _ _ ->
            -- まず fun と args に分解
            let
              (fun, args) = collectApps expr
              fun'        = rewriteExpr fun
              args'       = map rewriteExpr args
            in
              case fun' of
              EVar f ->
                let
                  unusedIdxs = lookupUnused f unusedInfo
                  -- インデックスが unusedIdxs に含まれないものだけ残す
                  keptArgs = [ a
                             | (i, a) <- zip [0..] args'
                             , i `notElem` unusedIdxs
                             ]
                in
                  rebuildApps fun' keptArgs
                -- 関数部分が変数でない場合は、そのまま再構成
              _ ->
                rebuildApps fun' args'
          ELet isRec defs body ->
            let
              defs' = [ (name, rewriteExpr rhs)
                      | (name, rhs) <- defs
                      ]
              body' = rewriteExpr body
            in
              ELet isRec defs' body'
          ECase cond alts ->
            let
              cond' = rewriteExpr cond
              alts' = [ (tag, vars, rewriteExpr altExpr)
                      | (tag, vars, altExpr) <- alts
                      ]
            in
              ECase cond' alts'

-- unused 情報の lookup
lookupUnused :: Name -> UnusedArgs -> [Int]
lookupUnused f unusedInfo
  = case lookup f unusedInfo of
    Just idxs -> idxs
    Nothing   -> []

-- EAp チェーンの分解と再構成

-- f a b c という形の EAp チェーンを (f, [a,b,c]) に分解
collectApps :: CoreExpr -> (CoreExpr, [CoreExpr])
collectApps expr
  = go expr []
    where
      go (EAp fun arg) acc = go fun (arg : acc)
      go fun           acc = (fun, acc)

-- 関数と引数リストから EAp チェーンを再構成
rebuildApps :: CoreExpr -> [CoreExpr] -> CoreExpr
rebuildApps fun args = foldl EAp fun args
--------------------------------
-- rewriteEAp 関連 (ここまで) --
--------------------------------

-- ここから、完全遅延評価ラムダリフタの定義
----------------------------------
-- separateLams 関連 (ここから) --
----------------------------------
separateLams :: CoreProgram -> CoreProgram
separateLams prog = [ (name, [], mkSepLams args (separateLams_e rhs))
                    | (name, args, rhs) <- prog
                    ]

separateLams_e :: CoreExpr -> CoreExpr
separateLams_e (EVar v) = EVar v
separateLams_e (EConstr t a) = EConstr t a
separateLams_e (ENum n) = ENum n
separateLams_e (EAp e1 e2)
  = EAp (separateLams_e e1) (separateLams_e e2)
separateLams_e (ECase e alts)
  = ECase (separateLams_e e) [ (tag, args, separateLams_e e)
                             | (tag, args, e) <- alts
                             ]
separateLams_e (ELam args body)
  = mkSepLams args (separateLams_e body)
separateLams_e (ELet is_rec defns body)
  = ELet is_rec [(name, separateLams_e rhs) | (name,rhs) <- defns]
               (separateLams_e body)

mkSepLams args body
  = foldr mkSepLam body args
    where
      mkSepLam arg body = ELam [arg] body
----------------------------------
-- separateLams 関連 (ここまで) --
----------------------------------

-------------------------------
-- addLevels 関連 (ここから) --
-------------------------------
addLevels :: CoreProgram -> AnnProgram (Name, Level) Level
addLevels = freeToLevel . freeVars

freeToLevel prog = map freeToLevel_sc prog

freeToLevel_sc (sc_name, [], rhs) = (sc_name, [], freeToLevel_e 0 [] rhs)

freeToLevel_e :: Level ->                    -- Level of context
                 ASSOC Name Level ->         -- Level of in-scope names
                 AnnExpr Name (Set Name) ->  -- Input expression
                 AnnExpr (Name, Level) Level -- Result expression
freeToLevel_e level env (free, ANum k) = (0, ANum k)
freeToLevel_e level env (free, AVar v) = (aLookup env v 0, AVar v)
freeToLevel_e level env (free, AConstr t a) = (0, AConstr t a)
freeToLevel_e level env (free, AAp e1 e2)
  = (max (levelOf e1') (levelOf e2'), AAp e1' e2')
    where
      e1' = freeToLevel_e level env e1
      e2' = freeToLevel_e level env e2
freeToLevel_e level env (free, ALam args body)
  = (freeSetToLevel env free, ALam args' body')
    where
      body' = freeToLevel_e (level + 1) (args' ++ env) body
      args' = [(arg, level+1) | arg <- args]
freeToLevel_e level env (free, ALet is_rec defns body)
  = (levelOf new_body, ALet is_rec new_defns new_body)
    where
      binders = bindersOf defns
      rhss = rhssOf defns
      new_binders = [(name, max_rhs_level) | name <- binders]
      new_rhss = map (freeToLevel_e level rhs_env) rhss
      new_defns = zip2 new_binders new_rhss
      new_body = freeToLevel_e level body_env body
      free_in_rhss = setUnionList [free | (free, rhs) <- rhss]
      max_rhs_level = freeSetToLevel level_rhs_env free_in_rhss
      body_env = new_binders ++ env
      rhs_env | is_rec          = body_env
              | otherwise       = env
      level_rhs_env | is_rec    = [(name, 0) | name <- binders] ++ env
                    | otherwise = env
freeToLevel_e level env (free, ACase e alts)
  = freeToLevel_case level env free e alts
freeToLevel_case free e alts = error "freeToLevel_case: not yet written"

freeSetToLevel :: ASSOC Name Level -> Set Name -> Level
freeSetToLevel env free
  = foldll max 0 [aLookup env n 0 | n <- setToList free]
    -- 自由変数がない場合はレベル0を返す

levelOf :: AnnExpr a Level -> Level
levelOf (level, e) = level
-------------------------------
-- addLevels 関連 (ここまで) --
-------------------------------

----------------------------------
-- identifyMFEs 関連 (ここから) --
----------------------------------
identifyMFEs :: ArityEnv -> AnnProgram (Name, Level) Level -> Program (Name, Level)
identifyMFEs arityEnv prog = [ (sc_name, [], fst (identifyMFEs_e arityEnv 0 rhs))
                             | (sc_name, [], rhs) <- prog
                             ]

identifyMFEs_e :: ArityEnv
                  -> Level                        -- Level of context
                  -> AnnExpr (Name, Level) Level  -- Input expression
                  -> (Expr (Name, Level), Bool)   -- Result (タプル第2要素の Bool = 子で MFE が発生したか)
identifyMFEs_e arityEnv ctx (level, ANum n)
  = (ENum n, False)
identifyMFEs_e arityEnv ctx (level, AVar v)
  = (EVar v, False)
identifyMFEs_e arityEnv ctx (level, AConstr t a)
  = (EConstr t a, False)
identifyMFEs_e arityEnv ctx (level, AAp e1 e2)
  = let
      (e1', m1) = identifyMFEs_e arityEnv ctx e1
      (e2', m2) = identifyMFEs_e arityEnv ctx e2
      childHasMFE = m1 || m2
      expr = EAp e1' e2'
    in
      if childHasMFE
      then (expr, True)
      else
        if level < ctx && okAsMFERoot arityEnv expr
        then (transformMFE level expr, True)
        else (expr, False)
identifyMFEs_e arityEnv ctx (level, ALam args body)
  = let
      arg_level = snd (head args)
      (body', m) = identifyMFEs_e arityEnv arg_level body
    in
      (ELam args body', m)
identifyMFEs_e arityEnv ctx (level, ALet is_rec defns body)
  = let
      processDef ((name, rhs_level), rhs)
        = let
            (rhs', m) = identifyMFEs_e arityEnv rhs_level rhs
          in
            (((name, rhs_level), rhs'), m)
      (defns', ms1) = unzip (map processDef defns)
      (body', m2)   = identifyMFEs_e arityEnv ctx body
      childHasMFE   = or (m2 : ms1)
    in
      (ELet is_rec defns' body', childHasMFE)
identifyMFEs_e arityEnv ctx (level, ACase e alts)
  = error "identifyMFEs_case: not implemented"

notMFECandidate (AConstr t a) = True
notMFECandidate (ANum k)      = True
notMFECandidate (AVar v)      = True
notMFECandidate ae            = False -- 今のところ、他のすべては候補です

transformMFE level e
  = ELet nonRecursive [(("v",level), e)] (EVar "v")

okAsMFERoot :: ArityEnv -> Expr (Name, Level) -> Bool
okAsMFERoot arityEnv e
  = case splitAp e of
    -- (EVar (name, _lvl), args)
    (EVar name, args)
      | Just n <- primOpArity name
      -> length args == n
      | Just n <- lookupArity arityEnv name
      -> length args == n
    _ -> True

splitAp :: Expr (Name, Level) -> (Expr (Name, Level), [Expr (Name, Level)])
splitAp (EAp f a)
  = let
      (h, args) = splitAp f
    in
      (h, args ++ [a])
splitAp e = (e, [])

primOpArity :: Name -> Maybe Int
primOpArity "*"  = Just 2
primOpArity "+"  = Just 2
primOpArity "-"  = Just 2
primOpArity "/"  = Just 2
primOpArity ">"  = Just 2
primOpArity ">=" = Just 2
primOpArity "<"  = Just 2
primOpArity "<=" = Just 2
primOpArity "==" = Just 2
primOpArity "~=" = Just 2
primOpArity "negate" = Just 1
primOpArity "if"     = Just 3
primOpArity _        = Nothing

lookupArity :: ArityEnv -> Name -> Maybe Int
lookupArity env name = lookup name env
---------------------------------
-- identifyMFEs 関連 (ここまで) --
----------------------------------

-----------------------------
-- renameL 関連 (ここから) --
-----------------------------
-- renameL :: Program (Name, Level) -> Program (Name, Level)
renameL :: Program (Name, a) -> Program (Name, a)
renameL prog = renameGen newNamesL prog

-- renameGen :: (NameSupply -> [(Name, Level)] -> (NameSupply, [(Name, Level)], ASSOC Name Name))
--                                       -- New-binders function
--              -> Program (Name, Level) -- Program to be renamed
--              -> Program (Name, Level) -- Resulting program
renameGen :: (NameSupply -> [(Name, a)] -> (NameSupply, [(Name, a)], ASSOC Name Name))
                                      -- New-binders function
             -> Program (Name, a) -- Program to be renamed
             -> Program (Name, a) -- Resulting program
renameGen new_binders prog
  = second (mapAccuml rename_sc initialNameSupply prog)
    where
      rename_sc ns (sc_name, args, rhs)
        = (ns2, (sc_name, args', rhs'))
          where
            (ns1, args', env) = new_binders ns args
            (ns2, rhs') = renameGen_e new_binders env ns1 rhs

-- newNamesL :: NameSupply -> [(Name, Level)] -> (NameSupply, [(Name, Level)], ASSOC Name Name)
newNamesL :: NameSupply -> [(Name, a)] -> (NameSupply, [(Name, a)], ASSOC Name Name)
newNamesL ns old_binders
  = (ns', new_binders, env)
    where
      old_names = [name | (name, level) <- old_binders]
      levels = [level | (name, level) <- old_binders]
      (ns', new_names) = getNames ns old_names
      new_binders = zip2 new_names levels
      env = zip2 old_names new_names

-- renameGen_e :: (NameSupply -> [(Name, Level)] -> (NameSupply, [(Name, Level)], ASSOC Name Name))
--                                                    -- New-binders function
--                -> ASSOC Name Name                  -- Maps old names to new ones
--                -> NameSupply                       -- Name supply
--                -> Expr (Name, Level)               -- Expression to be renamed
--                -> (NameSupply, Expr (Name, Level)) -- Depleted name supply and result expression
renameGen_e :: (NameSupply -> [(Name, a)] -> (NameSupply, [(Name, a)], ASSOC Name Name))
                                               -- New-binders function
               -> ASSOC Name Name              -- Maps old names to new ones
               -> NameSupply                   -- Name supply
               -> Expr (Name, a)               -- Expression to be renamed
               -> (NameSupply, Expr (Name, a)) -- Depleted name supply and result expression
renameGen_e new_binders env ns (EVar v)
  = (ns, EVar (aLookup env v v))
renameGen_e new_binders env ns (ENum n)
  = (ns, ENum n)
renameGen_e new_binders env ns (EAp e1 e2)
  = (ns2, EAp e1' e2')
    where
      (ns1, e1') = renameGen_e new_binders env ns  e1
      (ns2, e2') = renameGen_e new_binders env ns1 e2
renameGen_e new_binders env ns (ELam args body)
  = (ns2, ELam args' body')
    where
      (ns1, args', env') = new_binders ns args
      (ns2, body') = renameGen_e new_binders (env' ++ env) ns1 body
renameGen_e new_binders env ns (ELet is_rec defns body)
  = (ns3, ELet is_rec (zip2 binders' rhss') body')
    where
      (ns1, body') = renameGen_e new_binders body_env ns body
      binders = bindersOf defns
      (ns2, binders', env') = new_binders ns1 binders
      body_env = env' ++ env
      (ns3, rhss') = mapAccuml (renameGen_e new_binders rhsEnv) ns2 (rhssOf defns)
      rhsEnv | is_rec = body_env
             | otherwise = env
renameGen_e new_binders env ns (EConstr t a)
  = (ns, EConstr t a)
renameGen_e new_binders env ns (ECase e alts)
  = renameGen_case new_binders env ns e alts

renameGen_case new_binders env ns e alts
  = error "renameGen_case: not written"

test_program_1_for_renameL_1 :: Program (Name, Level)
test_program_1_for_renameL_1
  = [
      (
        "f",
        [],
        ELam [("x",1)]
          (
            ELet False
              [
                (
                  ("g",1),
                  ELam [("y",2)]
                    (
                      EAp
                        (
                          ELet False
                            [
                              (
                                ("v",1),
                                EAp (EVar "+") (EAp (EAp (EVar "*") (EVar "x")) (EVar "x"))
                              )
                            ]
                            (EVar "v")
                        )
                        (EVar "y")
                    )
                )
              ]
              (EAp (EAp (EVar "+") (EAp (EVar "g") (ENum 3))) (EAp (EVar "g") (ENum 4)))
          )
      ),
      ("main",[],EAp (EVar "f") (ENum 6))
    ]

test_program_1_for_renameL_2 :: Program (Name, Level)
test_program_1_for_renameL_2
  = [
      (
        "f",
        [],
        ELam [("x",1)]
          (
            ELet False
              [
                (
                  ("g",1),
                  ELam [("y",2)]
                    (
                      EAp
                        (
                          EAp
                            (EVar "+") 
                            (
                              ELet False
                                [
                                  (
                                    ("v",1),
                                    (EAp (EAp (EVar "*") (EVar "x")) (EVar "x"))
                                  )
                                ]
                                (EVar "v")
                            )
                        )
                        (EVar "y")
                    )
                )
              ]
              (EAp (EAp (EVar "+") (EAp (EVar "g") (ENum 3))) (EAp (EVar "g") (ENum 4)))
          )
      ),
      ("main",[],EAp (EVar "f") (ENum 6))
    ]
-----------------------------
-- renameL 関連 (ここまで) --
-----------------------------

---------------------------
-- float 関連 (ここから) --
---------------------------
float :: Program (Name, Level) -> CoreProgram
float prog = concat (map float_sc prog)

float_sc (name, [], rhs)
  = [(name, [], rhs')] ++ concat (map to_scs fds)
    where
      (fds, rhs') = float_e rhs
      to_scs (level, is_rec , defns) = map make_sc defns
      make_sc (name, rhs) = (name, [], rhs)

float_e :: Expr (Name, Level) -> (FloatedDefns, Expr Name)
float_e (EVar v) = ([], EVar v)
float_e (EConstr t a) = ([], EConstr t a)
float_e (ENum n) = ([], ENum n)
float_e (EAp e1 e2) = (fd1 ++ fd2, EAp e1' e2')
                      where
                        (fd1, e1') = float_e e1
                        (fd2, e2') = float_e e2
float_e (ELam args body)
  = (fd_outer, ELam args' (install fd_this_level body'))
    where
      args' = [arg | (arg,level) <- args]
      (first_arg,this_level) = hd args
      (fd_body, body') = float_e body
      (fd_outer, fd_this_level) = partitionFloats this_level fd_body
float_e (ELet is_rec defns body)
  = (rhsFloatDefns ++ [thisGroup] ++ bodyFloatDefns, body')
    where
      (bodyFloatDefns, body') = float_e body
      (rhsFloatDefns, defns') = mapAccuml float_defn [] defns
      thisGroup = (thisLevel, is_rec, defns')
      (name,thisLevel) = hd (bindersOf defns)
float_e (ECase e alts) = float_case e alts

float_defn floatedDefns ((name,level), rhs)
  = (rhsFloatDefns ++ floatedDefns, (name, rhs'))
    where
      (rhsFloatDefns, rhs') = float_e rhs

float_case e alts = error "float_case: not yet written"

partitionFloats :: Level -> FloatedDefns -> (FloatedDefns, FloatedDefns)
partitionFloats this_level fds
  = (filter is_outer_level fds, filter is_this_level fds)
    where
      is_this_level (level,is_rec ,defns) = level >= this_level
      is_outer_level (level,is_rec ,defns) = level < this_level

install :: FloatedDefns -> Expr Name -> Expr Name
install defnGroups e
  = foldr installGroup e defnGroups
    where
      installGroup (level, is_rec, defns) e = ELet is_rec defns e

test_program_1_for_float_1
  = [
      (
        "f",
        [],
        ELam [("x_0", 1)]
          (ELet False
             [
               (("g_1", 1),
               ELam [("y_2", 2)]
                 (EAp
                    (ELet False
                       [(("v_3", 1), EAp (EVar "+") (EAp (EAp (EVar "*") (EVar "x_0")) (EVar "x_0")))]
                       (EVar "v_3")
                    ) (EVar "y_2"))
                 )
             ]
             (EAp (EAp (EVar "+") (EAp (EVar "g_1") (ENum 3))) (EAp (EVar "g_1") (ENum 4)))
          )
      ),
      ("main",[],EAp (EVar "f") (ENum 6))
    ]

test_program_1_for_float_2 :: Program (Name, Level)
test_program_1_for_float_2
  = [
      (
        "f",
        [],
        ELam [("x_0", 1)]
          (ELet False
             [
               (("g_1", 1),
               ELam [("y_2", 0)]
                 (EAp
                    (ELet False
                       [(("v_3", 1), EAp (EVar "+") (EAp (EAp (EVar "*") (EVar "x_0")) (EVar "x_0")))]
                       (EVar "v_3")
                    ) (EVar "y_2"))
                 )
             ]
             (EAp (EAp (EVar "+") (EAp (EVar "g_1") (ENum 3))) (EAp (EVar "g_1") (ENum 4)))
          )
      ),
      ("main",[],EAp (EVar "f") (ENum 6))
    ]

{-
*Lambda> float test_program_1_for_float_2
[
  (
    "f",
    [],
    ELam ["x_0"]
      (ELet False
         [
           (
             "g_1",
             ELam ["y_2"]
               (ELet False
                  [
                    (
                      "v_3",
                      EAp (EVar "+") (EAp (EAp (EVar "*") (EVar "x_0")) (EVar "x_0"))
                    )
                  ]
                  (EAp (EVar "v_3") (EVar "y_2"))
               )
           )
         ]
         (EAp (EAp (EVar "+") (EAp (EVar "g_1") (ENum 3))) (EAp (EVar "g_1") (ENum 4)))
      )
  ),
  ("main",[],EAp (EVar "f") (ENum 6))
]
-}
{-
*Lambda> (Lambda.pprint . lambdaLift . float) test_program_1_for_float_2
f x_0_1 = let
            g_1_2 = sc_3 x_0_1
          in
            (g_1_2 3) + (g_1_2 4) ;
sc_3 x_0_4 y_2_5 = let
                     v_3_6 = + (x_0_4 * x_0_4)
                   in
                     v_3_6 y_2_5 ;
main  = f 6
-}
---------------------------
-- float 関連 (ここまで) --
---------------------------

---------------------------------
-- テストプログラム (ここから) --
---------------------------------
test_program_1 = "f x = let g = \\y. x*x + y in (g 3 + g 4) ; " ++
                 "main = f 6"

test_program_1_ = "f x = let g = \\y. (x*x) + y in (g 3 + g 4) ; " ++
                  "main = f 6"

test_program_1__ = "f x = let g = \\y. y + x*x in (g 3 + g 4) ; " ++
                   "main = f 6"

test_program_2 = "f x = letrec g = \\y. cons (x*y) (g y) in g 3 ; " ++
                 "main = f 6"

test_program_3 = "pair x y f = f x y ; " ++
                 "f x y = letrec " ++
                 "          fst = \\p. p K ; " ++
                 "          snd = \\p. p K1 ; " ++
                 "          a = pair x b ; " ++
                 "          b = pair y a " ++
                 "        in " ++
                 "          fst (snd (snd (snd a))) ; " ++
                 "main = f 3 4"

test_program_4 = "cons  = Pack{2, 2} ; " ++
                 "nil   = Pack{1, 0} ; " ++
                 "true  = Pack{2, 0} ; " ++
                 "false = Pack{1, 0} ; " ++
                 "between n m = let " ++
                 -- "                if = \\cond tbranch fbranch. case cond of " ++  -- 変数の出現順序を合わせる必要がありそう。
                 "                if = \\cond fbranch tbranch. case cond of " ++
                 "                                               <1> -> fbranch ; " ++
                 "                                               <2> -> tbranch  " ++
                 "              in " ++
                 "                if (n > m) nil (cons n (between (n + 1) m)) ; " ++
                 "main = between 1 4"

test_program_4' = "f n m = let " ++
                  "          g = \\x y. x + y " ++
                  "        in " ++
                  "          g n m ; " ++
                  "main = f 1 4"

test_program_4'' = "f n m = let " ++
                   "          g = \\y x. x + y " ++  -- ここは変数の出現順序が合ってなくても大丈夫そう。
                   "        in " ++
                   "          g n m ; " ++
                   "main = f 1 4"

test_program_4''' = "f n m = let " ++
                    "          g = \\x. \\y. x + y " ++
                    "        in " ++
                    "          g n m ; " ++
                    "main = f 1 4"

test_program_5 = "f = \\x. x+1 ; " ++
                 "main = f 4"

test_program_6 = "f x = let " ++
                 "        g = (\\y. y+1) " ++
                 "      in " ++
                 "        g (g x) ; " ++
                 "main = f 4"

ex_4_23_2__ = "cons  = Pack{2, 2} ; " ++
              "nil   = Pack{1, 0} ; " ++
              "true  = Pack{2, 0} ; " ++
              "false = Pack{1, 0} ; " ++
              -- "if cond tbranch fbranch = case cond of " ++  -- 変数の出現順序を合わせる必要がありそう。
              "if cond fbranch tbranch = case cond of " ++
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

test_program_for_case = "cnstr1 = Pack{1, 1} ; " ++
                        "cnstr2 = Pack{2, 0} ; " ++
                        "f x y = case x of " ++
                        "        <1> x -> x " ++
                        "        <2>   -> y ; " ++
                        "main = f 1 2"

test_program_caseLam =
  "f x = case x of " ++
  "        <1> -> (\\y. cons (x*y) nil) ; " ++
  "        <2> -> (\\z. cons z (f z)) ; " ++
  "main = f 2"

test_program_caseFree =
  "f x y = case (x > y) of " ++
  "          <1> -> cons x nil ; " ++
  "          <2> -> cons y nil ; " ++
  "main = f 3 5"

test_program_caseLetrec =
  "f x = letrec g = \\y. case y of " ++
  "                     <1> -> cons (x*y) (g y) ; " ++
  "                     <2> -> nil " ++
  "       in g 1 ; " ++
  "main = f 4"

test_program_7
  = "f x y = letrec " ++
    "          g = \\p. h p + x ; " ++
    -- "          h = \\q. k + y + q ; " ++
    "          h = \\q. (k + y) + q ; " ++
    "          k = g y " ++
    "        in " ++
    "          g 4 ; " ++
    "main = f 3 4"

test_program_sc_arity
  = "f x = let " ++
    "        h = sc2 x " ++   -- ★ sc2 は arity 2 なので、ここは部分適用
    "      in " ++
    "        (h 3) + (h 4) ; " ++
    "sc2 a b = a * b ; " ++
    "main = f 10"

test_program_sc3
 = "f x = let " ++
   "        h = sc3 x " ++   -- ★ arity 3 の SC を部分適用（引数1個）
   "      in " ++
   "        (h 2 3) + (h 4 5) ; " ++
   "sc3 a b c = a + b * c ; " ++
   "main = f 10"

test_program_nested_partial
  = "f x = letrec " ++
    "        h1 = sc2 x ; " ++     -- arity 2 → 部分適用
    "        h2 = h1 " ++          -- さらに部分適用のまま伝播
    "      in " ++
    "        (h2 7) + (h2 8) ; " ++
    "sc2 a b = a - b ; " ++
    "main = f 20"

test_program_mfe_complex :: CoreProgram
test_program_mfe_complex
  = [ ("f", ["x","y"],
        ELet nonRecursive
          [ ("z", EAp (EAp (EVar "+") (EVar "x")) (EVar "y")) ]
          (EAp (EAp (EVar "*") (EVar "z")) (EVar "z"))
      )
    , ("main", [],
        EAp (EAp (EVar "f") (ENum 3)) (ENum 4)
      )
    ]
  -- f x y = let z = x + y in z * z
  -- main  = f 3 4
  -- f の中に「自由変数を含む let」があり、それが MFE として抽出される。

test_program_sc_as_arg :: CoreProgram
test_program_sc_as_arg
  = [ ("twice", ["f","x"],
        EAp (EVar "f") (EAp (EVar "f") (EVar "x"))
      )
    , ("inc", ["x"],
        EAp (EAp (EVar "+") (EVar "x")) (ENum 1)
      )
    , ("main", [],
        EAp (EAp (EVar "twice") (EVar "inc")) (ENum 10)
      )
    ]
  -- twice f x = f (f x)
  -- inc   x   = x + 1
  -- main      = twice inc 10
  -- 高階関数 twice に SC を渡す。
  -- 渡される側・受け取る側の両方で MFE の候補が出る。
---------------------------------
-- テストプログラム (ここまで) --
---------------------------------
