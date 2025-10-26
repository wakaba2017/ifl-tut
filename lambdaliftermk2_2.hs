module Lambda where
import Utils
import Language

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
  = (ns1, ELam args' body')
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
        = (sc_name, args, rhs') : scs
          where
            (scs, rhs') = collectSCs_e rhs

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
      (rhss_scs, defns') = mapAccuml collectSCs_d [] defns
      scs'     = [(name, rhs) | (name, rhs) <- defns', isELam rhs]
      non_scs' = [(name, rhs) | (name, rhs) <- defns', not (isELam rhs)]
      local_scs = [(name, args, body) | (name, ELam args body) <- scs']
      (body_scs, body') = collectSCs_e body
      collectSCs_d scs (name, rhs) = (scs ++ rhs_scs, (name, rhs'))
                                     where
                                       (rhs_scs, rhs') = collectSCs_e rhs

mkELet is_rec defns body
  | is_rec == nonRecursive && length defns == 0 = body
  | otherwise = ELet is_rec defns body

isELam :: Expr a -> Bool
isELam (ELam args body) = True
isELam other = False
-------------------------------
-- collectSC 関連 (ここまで) --
-------------------------------

---------------------------------
-- テストプログラム (ここから) --
---------------------------------
test_program_1 = "f x = let g = \\y. x*x + y in (g 3 + g 4) ; " ++
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
---------------------------------
-- テストプログラム (ここまで) --
---------------------------------
