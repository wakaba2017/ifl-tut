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

type AnnDefn a b = (a, AnnExpr a b)

type AnnAlt a b = (Int, [a], (AnnExpr a b))

type AnnProgram a b = [(Name, [a], AnnExpr a b)]
-------------------------------
-- データ型の定義 (ここまで) --
-------------------------------

----------------------------
-- pprint 関連 (ここまで) --
----------------------------
-- pprint :: CoreProgram -> String
pprint prog = pprintGen iStr prog

{-
pprintGen :: (Name -> Iseq)   -- function from binders to iseq
             -> Program Name  -- the program to be formatted
             -> String        -- result string
-}
pprintGen :: (a -> Iseq)   -- function from binders to iseq
             -> Program a  -- the program to be formatted
             -> [Char]     -- result string
pprintGen ppr prog = iDisplay (pprProgramGen ppr prog)

pprProgramGen :: (a -> Iseq)   -- function from binders to iseq
                 -> Program a
                 -> Iseq
pprProgramGen ppr prog = iInterleave sep scDefns
                         where
                           sep = iConcat [iStr " ; ", iNewline]
                           scDefns = map (pprScDefnGen ppr) prog

pprScDefnGen :: (a -> Iseq)   -- function from binders to iseq
                -> ScDefn a  -- CoreScDefn では型不一致となる模様。
                -> Iseq
pprScDefnGen ppr (name, args, expr) = iConcat [ iStr name,
                                                iStr " ",
                                                iInterleave (iStr " ") (map ppr args),
                                                iStr " = ",
                                                pprExprGen ppr expr
                                              ]

pprExprGen :: (a -> Iseq)   -- function from binders to iseq
              -> Expr a  -- CoreExpr では型不一致となる模様。
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
               -> Expr a  -- CoreExpr では型不一致となる模様。
               -> Iseq
pprAExprGen ppr e | isAtomicExpr e = pprExprGen ppr e
                  | otherwise      = iConcat [ iStr "(",
                                               pprExprGen ppr e,
                                               iStr ")"
                                             ]

pprDefnsGen :: (a -> Iseq)   -- function from binders to iseq
               -> [(a, Expr a)]  -- [(Name, CoreExpr)] では型不一致となる模様。
               -> Iseq
pprDefnsGen ppr defns = iInterleave sep (map (pprDefnGen ppr) defns)
                        where
                          sep = iConcat [ iStr ";", iNewline ]

pprDefnGen :: (a -> Iseq)   -- function from binders to iseq
              -> (a, Expr a)  -- (Name, CoreExpr) では型不一致となる模様。
              -> Iseq
pprDefnGen ppr (name, expr) = iConcat [ -- iStr name, iStr " = ",
                                        ppr name, iStr " = ",
                                        iIndent (pprExprGen ppr expr)
                                      ]

pprCoreAltsGen :: (a -> Iseq)   -- function from binders to iseq
                  -> [Alter a]  -- [CoreAlt] では型不一致となる模様。
                  -> Iseq
pprCoreAltsGen ppr alts = iInterleave sep (map (pprCoreAltGen ppr) alts)
                          where
                           sep = iNewline

pprCoreAltGen :: (a -> Iseq)   -- function from binders to iseq
                 -> Alter a  -- CoreAlt では型不一致となる模様。
                 -> Iseq
pprCoreAltGen ppr (tag, lvs, expr) = iConcat [ iStr "<", iNum tag, iStr "> ",
                                               iInterleave (iStr " ") (map ppr lvs),
                                               iStr " -> ",
                                               pprExprGen ppr expr
                                             ]

{-
pprintAnn :: (Name -> Iseq)                 -- function from binders to iseq
             -> ((Set Name) -> Iseq)        -- function from annotations to iseq
             -> AnnProgram Name (Set Name)  -- program to be displayed
             -> String                      -- result string
-}
pprintAnn :: (a -> Iseq)        -- function from binders to iseq
             -> (b -> Iseq)     -- function from annotations to iseq
             -> AnnProgram a b  -- program to be displayed
             -> [Char]          -- result string
pprintAnn pprbnd pprant prog = undefined
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
-- runS = Language.pprint . lambdaLift . parse
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
  = error "freeVars_e: no case for constructors"

freeVars_case lv e alts
  = error "freeVars_case: not yet written"

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
  = error "abstract_e: no case for Constr"
abstract_e (free, ACase e alts)
  = abstract_case free e alts

abstract_case free e alts
  = error "abstract_case: not yet written"
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
rename_e env ns (EConstr t a) = error "rename_e: no case for constructors"
rename_e env ns (ECase e alts) = rename_case env ns e alts
rename_case env ns e alts = error "rename_case: not yet written"
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

mkELet is_rec defns body = ELet is_rec defns body

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

ex_2_11 = "pair x y f = f x y ; " ++
          "fst p = p K ; " ++
          "snd p = p K1 ; " ++
          "f x y = letrec " ++
          "          a = pair x b ; " ++
          "          b = pair y a " ++
          "        in " ++
          "          fst (snd (snd (snd a))) ; " ++
          "main = f 3 4"
---------------------------------
-- テストプログラム (ここまで) --
---------------------------------
