------------------------------------------
-- 1.3 Data types for the Core language --
------------------------------------------

module Language where
import Data.Char ( isDigit, isAlpha, isSpace )
import Utils

data Expr a
  =  EVar Name                    -- Variables
   | ENum Int                     -- Numbers
   | EConstr Int Int              -- Constructor tag arity
   | EAp (Expr a) (Expr a)        -- Applications
   | ELet                         -- Let(rec) expressions
        IsRec                     --   boolean with True = recursive,
        [(a, Expr a)]             --   Definitions
        (Expr a)                  --   Body of let(rec)
   | ECase                        -- Case expression
        (Expr a)                  --   Expression to scrutinise
        [Alter a]                 --   Alternatives
   | ELam [a] (Expr a)            -- Lambda abstraction
    --deriving (Text)
    deriving (Show)

type CoreExpr = Expr Name

type Name = String

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

----------------------------------
-- 1.4 A small standard prelude --
----------------------------------

preludeDefs :: CoreProgram
preludeDefs
  = [ ("I",       ["x"],           EVar "x"),
      ("K",       ["x", "y"],      EVar "x"),
      ("K1",      ["x", "y"],      EVar "y"),
      ("S",       ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                                       (EAp (EVar "g") (EVar "x"))),
      ("compose", ["f", "g", "x"], EAp (EVar "f")
                                       (EAp (EVar "g")
                                            (EVar "x"))),
      ("twice",   ["f"],           EAp (EAp (EVar "compose")
                                            (EVar "f"))
                                       (EVar "f"))]

--------------------------------------
-- 1.5 コア言語のためのプリティプリンタ --
--------------------------------------

--
-- 1.5.1 文字列をつかったプリティプリンティング
--

pprExpr :: CoreExpr -> String
pprExpr (ENum n) = show n
pprExpr (EVar v) = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprExpr e2
{-
*Language> :t EVar "x"
EVar "x" :: Expr a
*Language> pprExpr (EVar "x")
"x"
*Language> pprExpr (ENum 1)
"1"
*Language> pprExpr (EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
"f x g x"
-}

pprAExpr :: CoreExpr -> String
pprAExpr e | isAtomicExpr e = pprExpr e
           | otherwise      = "(" ++ pprExpr e ++ ")"
{-
*Language> pprAExpr (EVar "x")
"x"
*Language> pprAExpr (ENum 1)
"1"
*Language> pprAExpr (EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
"(f x g x)"
-}

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldll EAp e1 (take n e2s)
                    where
                      e2s = e2 : e2s
{-
*Language> mkMultiAp 0 (EVar "f") (EVar "g")
EVar "f"
*Language> mkMultiAp 1 (EVar "f") (EVar "g")
EAp (EVar "f") (EVar "g")
*Language> mkMultiAp 2 (EVar "f") (EVar "g")
EAp (EAp (EVar "f") (EVar "g")) (EVar "g")
*Language> mkMultiAp 3 (EVar "f") (EVar "g")
EAp (EAp (EAp (EVar "f") (EVar "g")) (EVar "g")) (EVar "g")
-}

--
-- 1.5.2 プリティプリンタ用の抽象データ型
--

{-
iNil     :: Iseq                 -- The empty iseq
iStr     :: String -> Iseq       -- Turn a string into an iseq
iAppend  :: Iseq -> Iseq -> Iseq -- Append two iseqs
iNewline :: Iseq                 -- New line with indentation
iIndent  :: Iseq -> Iseq         -- Indent an iseq
iDisplay :: Iseq -> String       -- Turn an iseq into a string
-}

{- このようなデータ型が与えられた場合、文字のリストの代わりにiseqを返すようにpprExprを書き直します。 -}
{-
pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = iStr (show n) -- とりあえず追加してみた。
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr) = iConcat [ iStr keyword,
                                            iNewline,
                                            iStr " ",
                                            iIndent (pprDefns defns),
                                            iNewline,
                                            iStr "in ",
                                            pprExpr expr
                                          ]
                                  where
                                    keyword | not isrec = "let"
                                            | isrec     = "letrec"
-}
{-
*Language> pprExpr (EVar "x")
IStr "x"
*Language> pprExpr (ENum 1)
IStr "1"
*Language> pprExpr (EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
IAppend (IAppend (IAppend (IAppend (IStr "f") (IStr " ")) (IStr "x")) (IStr " ")) (IAppend (IAppend (IStr "(") (IAppend (IAppend (IStr "g") (IStr " ")) (IStr "x"))) (IStr ")"))

IAppend
  (IAppend
    (IAppend
      (IAppend (IStr "f") (IStr " "))
      (IStr "x")
    )
    (IStr " ")
  )
  (IAppend
    (IAppend
      (IStr "(")
      (IAppend
        (IAppend (IStr "g") (IStr " "))
        (IStr "x")
      )
    )
    (IStr ")")
  )
-}

{-
pprAExpr :: CoreExpr -> Iseq
pprAExpr e | isAtomicExpr e = pprExpr e
           | otherwise      = iConcat [ iStr "(",
                                        pprExpr e,
                                        iStr ")"
                                      ]
-}

{-
pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
                 where
                   sep = iConcat [ iStr ";", iNewline ]
-}

{-
pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]
-}

iConcat :: [Iseq] -> Iseq
-- Exercise 1.2 (iConcat)
-- BEGIN
iConcat []             = iNil
iConcat [x]            = x
iConcat (x1 : x2 : xs) = iAppend (iAppend x1 x2) (iConcat xs)
-- END

iInterleave :: Iseq -> [Iseq] -> Iseq
-- Exercise 1.2 (iInterleave)
-- BEGIN
iInterleave sep [] = iNil
iInterleave sep [x] = x
iInterleave sep (x1 : [x2]) = x1 `iAppend` sep `iAppend` x2
iInterleave sep (x1 : x2 : xs) = x1 `iAppend` sep `iAppend` x2 `iAppend` sep `iAppend` (iInterleave sep xs)
-- END

{-
--pprint :: CoreProgram -> String
--pprint prog = iDisplay (pprProgram prog)
-}

{-
-- Exercise 1.3 (pprExpr, pprAExpr, pprProgram)
pprExpr に式を追加してcase 式とラムダ式を処理し、pprAExprとpprProgram の定義を同じスタイルで記述します。
-}

--
-- 1.5.3 iseqの実装
--

{-
ここで、iseqタイプの実装について説明します。
まず、すべてのインデントを無視する実装を作成します。
抽象データ型を実装するには、iseqを表すためにどの型が使用されているかを指定する必要があります。
-}

{-
data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
-}


{-
この特定の表現の一般的な考え方は、iDisplayの最終的なすべてまですべての作業を延期することです。
操作iNil、iStr、およびiAppendはすべて、関連するコンストラクタを使用するだけです。
-}
iNil :: Iseq -- The empty iseq
iNil = INil

iStr :: String -> Iseq -- Turn a string into an iseq
-- iStr str = IStr str

iAppend :: Iseq -> Iseq -> Iseq -- Append two iseqs
iAppend seq1 seq2 = IAppend seq1 seq2

{-
インデントを無視しているので、iIndentとiNewlineは簡単に定義されます。
次のセクションでそれらを改善します。
-}
iNewline :: Iseq -- New line with indentation
-- iNewline = IStr "\n"

iIndent :: Iseq -> Iseq -- Indent an iseq
-- iIndent seq = seq

iDisplay :: Iseq -> String -- Turn an iseq into a string
-- iDisplay seq = flatten [seq]

{-
関数flattenは、iseqRepのリストを取得し、リスト内の各iseqRepsを連結した結果を返します。
このリストがある理由は、すぐにわかるように、保留中の作業のリストを蓄積できるようにするためです。
flattenは、抽象型iseqではなく、表現型iseqRepを操作することに注意してください。

私たちは、ワークリストと呼ばれるその引数のケース分析によってflattenを実行します。
-}
-- flatten :: [Iseq] -> String
{-
ワークリストが空の場合、これで完了です。
-}
-- flatten [] = ""
{-
それ以外の場合は、ワークリストの最初の要素でケース分析を行うことで作業します。
INilの場合は、ワークリストからアイテムをポップするだけです。
-}
-- flatten (INil : seqs) = flatten seqs
{-
IStrの場合は、指定された文字列を追加して、残りのワークリストをフラット化することで機能します。
-}
-- flatten (IStr s : seqs) = s ++ (flatten seqs)
{-
これまでのところ、flattenがリストを取得するという事実は、私たちにはあまり役に立ちませんでした。
IAppendを扱うと、list引数の正当性がより明確にわかります。
実行する必要があるのは、もう1つのアイテムをワークリストの先頭にプッシュすることだけです。
-}
-- flatten (IAppend seq1 seq2 : seqs) = flatten (seq1 : seq2 : seqs)
{-
*Language> flatten [pprExpr (EVar "x"), pprExpr (ENum 1)]
"x1"
*Language> flatten [pprExpr (EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))]
"f x (g x)"
-}

{-
-- Exercise 1.4
iseq のサイズに関して平坦化のコストはいくらですか?
上記のようにiseq を使用するようにpprExpr を変更し、前の演習と同じ実験を使用して新しい実装の効果を測定します。
pprExpr の結果にiDisplayを適用することを忘れないでください。

-- Exercise 1.5
抽象データ型を使用する主な利点は、インターフェイスに影響を与えずにADT の実装を変更できることです。
この例として、引数のいずれかがINil の場合に単純化された結果を返すようにiAppend を再定義します。
-}

--
-- 1.5.4 レイアウトとインデント
--

{-
これまでのところ、iIndent操作についてはかなり些細な解釈しか与えていませんが、今度はそれを改善することにします。
以前と同じ精神で、最初に2つのコンストラクタIIndentとINewlineを追加してiseqRep型を拡張し、
これらのコンストラクタを使用するように操作を再定義します。
-}
data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline

iIndent seq = IIndent seq
iNewline    = INewline

{-
次に、flattenをより強力にする必要があります。
まず、現在のカラムを追跡する必要があります。
次に、そのワークリストは(iseq, num)ペアで構成されている必要があります。
ここで、数値は対応するiseqに必要なインデントを示します。
-}
flatten :: Int              -- Current column; 0 for first column
           -> [(Iseq, Int)] -- Work list
           -> String        -- Result

{-
flatten を適切に初期化するには、iDisplay を変更する必要があります。
-}
iDisplay seq = flatten 0 [(seq, 0)]

{-
flattenの興味深いケースは、INewlineを処理する場合です。
これは、インデントを実行する必要がある場所だからです。
-}
flatten col ((INewline, indent) : seqs) = '\n' : (space indent) ++ (flatten indent seqs)
{-
新しい行に移動してインデントスペースを追加したため、
flattenの再帰呼び出しには現在のカラムのインデント引数があることに注意してください。
これは、新しい行に移動してインデントスペースを追加したためです。

IIndent ケースは、現在の列から現在のインデントを設定するだけです。
-}
flatten col ((IIndent seq, indent) : seqs) = flatten col ((seq, col) : seqs)

-- Exercise 1.6 (add equation for flatten for INil)
flatten col ((INil, indent) : seqs) = flatten col seqs
-- Exercise 1.6 (add equation for flatten for IStr)
-- flatten col ((IStr s, indent) : seqs) = (space indent) ++ s ++ (flatten col seqs)  -- (思い切り間違えた自分のコード・・・文字列の前に空白を入れると勘違いしている。)
flatten col ((IStr s, indent) : seqs) = s ++ (flatten (col + length s) seqs)  -- (nobsun様の実装をカンニング!)
-- Exercise 1.6 (add equation for flatten for IAppend)
flatten col ((IAppend seq1 seq2, indent) : seqs) = flatten col ((seq1, indent) : (seq2, indent) : seqs)  -- 

flatten col [] = ""

-- Exercise 1.7 (modify iStr to check for a '\n' is embedded in a string given to IStr)
iStr [] = IStr ""
iStr (x : xs) | x == '\n' = IAppend INewline (iStr xs)
              | otherwise = IAppend (IStr [x]) (iStr xs)  -- うっかり (iStr [x]) としてしまうと、止まらなくなる模様。

--
-- 1.5.5 演算子の優先順位
--

{-
pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = iStr (show n) -- とりあえず追加してみた。
pprExpr (EVar v) = iStr v
pprExpr (EAp (EAp (EVar "+") e1) e2) = iConcat [ pprAExpr e1,
                                                 iStr " + ",
                                                 pprAExpr e2
                                               ]
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr) = iConcat [ iStr keyword,
                                            iNewline,
                                            iStr " ",
                                            iIndent (pprDefns defns),
                                            iNewline,
                                            iStr "in ",
                                            pprExpr expr
                                          ]
                                  where
                                    keyword | not isrec = "let"
                                            | isrec     = "letrec"
-}

-- Exercise 1.8 (change pprExpr)
{-
pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v) = iStr v
-}
{- 工事中
pprExpr (EAp (EAp (EVar "+") e1) e2) = iConcat [ pprAExpr e1,
                                                 iStr " + ",
                                                 pprAExpr e2
                                               ]
-}
{-
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr) = iConcat [ iStr keyword,
                                            iNewline,
                                            iStr " ",
                                            iIndent (pprDefns defns),
                                            iNewline,
                                            iStr "in ",
                                            pprExpr expr
                                          ]
                                  where
                                    keyword | not isrec = "let"
                                            | isrec     = "letrec"
-}

--
-- 1.5.6 iseqのその他の便利な機能
--

iNum :: Int -> Iseq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseq
iFWNum width n
  = iStr (space (width - length digits) ++ digits)
    where
      digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs
  = iConcat (map lay_item (zip [1..] seqs))
    where
      lay_item (n, seq)
        = iConcat [ iFWNum 4 n,
                    iStr ")",
                    iIndent seq,
                    iNewline
                  ]

{-
iParen :: Iseq iseq => iseq -> iseq -- オリジナルの型シグネチャ
-}

--------------------------
-- 1.6 コア言語用のパーサ --
--------------------------

clex :: String -> [Token]

syntax :: [Token] -> CoreProgram

parse :: String -> CoreProgram
parse = syntax . clex
-- In Gofer I propose to compose this with some function
-- CoreProgram -> String, which will illustrate some sort of
-- execution machine, and then give this composition to catWith
-- from my utils

{-
*Language> ex1_21'
"f = 3 ; g x y = let z = x in z ; h x = case (let y = x in y) of <1> -> 2 ; <2> -> 5"

*Language> parse ex1_21'
[("f",[],ENum 3),("g",["x","y"],ELet False [("z",EVar "x")] (EVar "z")),("h",["x"],ECase (ELet False [("y",EVar "x")] (EVar "y")) [(1,[],ENum 2),(2,[],ENum 5)])]
-}

--
-- 1.6.1 字句解析
--

type Token = String -- A token is never empty

clex []       = []
clex (c1 : c2 : cs) | elem (c1 : c2 : []) twoCharOps = (c1 : c2 : []) : clex cs  -- 1.9
clex (c : cs) | isWhiteSpace c = clex cs
clex (c : cs) | isDigit c = num_token : clex rest_cs
                where
                  num_token = c : takeWhile isDigit cs
                  rest_cs   = dropWhile isDigit cs
clex (c : cs) | isAlpha c = var_tok : clex rest_cs
                where
                  var_tok = c : takeWhile isIdChar cs
                  rest_cs = dropWhile isIdChar cs
clex (c : cs) | otherwise = [c] : clex cs

isIdChar, isWhiteSpace :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')
isWhiteSpace c = c `elem` " \t\n"

-- Exercise 1.9
-- コメントと空白を無視するように字句アナライザを変更します。
-- コメントが二重の縦棒||によって導入されるのと同じ規則を使用し、行の終わりまで延長します。

-- Exercise 1.10
-- 字句解析プログラムは現在、<=や==などの2文字の演算子を単一のトークンとして認識しません。
-- それらのリストを提供することにより、そのような演算子を定義します。
-- twoCharOps :: [String]
-- twoCharOps = ["==", "~=", ">=", "<=", "->"]
-- twoCharOpsのメンバーをトークンとして認識するようにlexを変更します。
-- (標準関数メンバーが役立つ場合があります。)
twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

-- Exercise 1.11
-- 字句解析では空白が破棄されるため、パーサーは構文エラーの行番号を報告できません。
-- この問題を解決する1つの方法は、各トークンに行番号を付けることです。
-- つまり、タイプtokenは次のようになります。
-- token == (num, [char])
-- これを行うように字句アナライザを変更します。
-- これを行うには、現在の行番号であるパラメータをlexに追加する必要があります。

--
-- 1.6.2 Basic tools for parsing
--

type Parser a = [Token] -> [(a, [Token])]

-- 関数pLit（ `lit 'は` literal'の略）は文字列を受け取り、
-- その文字列を含むトークンのみを認識するパーサーを提供し、
-- その文字列を解析の値として返します。

-- 文字列を受け取り、その文字列を含むトークンのみを認識するパーサを返す関数
pLit :: String -> Parser String
{-
pLit s []           = []
pLit s (tok : toks) | s == tok  = [(s, toks)]
                    | otherwise = []
-}

{-
*Language> pLit "hello" ["hello", "James", "!"]
[("hello",["James","!"])]

*Language> :t pLit "hello"
pLit "hello" :: Parser String

*Language> :t pLit "hello" ["hello", "James", "!"]
pLit "hello" ["hello", "James", "!"] :: [(String, [Token])]
-}

-- 入力の先頭から変数を解析するために、パーサーpVarを定義します。
pVar :: Parser String
{-
pVar [] = []
-}

-- 2つのパーサー(たとえば、p1とp2)を組み合わせた関数pAlt(`alt'は`alternative'の略)を定義します。
-- 最初にp1を使用して入力を解析し、次にp2を使用して同じ入力を解析します。
-- p1またはp2のいずれかによって返されたすべての成功した解析を返します。
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

{-
*Language> pHelloOrGoodbye ["hello", "world", "!"]
[("hello",["world","!"])]

*Language> pHelloOrGoodbye ["goodbye", "world", "!"]
[("goodbye",["world","!"])]

*Language> pHelloOrGoodbye ["hello", "goodbye", "!"]
[("hello",["goodbye","!"])]

*Language> pHelloOrGoodbye ["goodbye", "hello", "!"]
[("goodbye",["hello","!"])]
-}

-- 次に、p1とp2などの2つのパーサーを組み合わせて、次のように動作するより大きなパーサーを返します。
-- まず、p1を使用して入力から値を解析し、次にp2を使用して残りの入力から2番目の値を解析します。
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [(combine v1 v2, toks2) | (v1, toks1) <- p1 toks,
                              (v2, toks2) <- p2 toks1]
{-
pGreeting :: Parser (String, String)
pGreeting = pThen mk_pair pHelloOrGoodbye pVar
            where
              mk_pair hg name = (hg, name)
-}

--
-- 1.6.3 Sharpening the tools
--

pGreeting = pThen keep_first (pThen mk_pair pHelloOrGoodbye pVar) (pLit "!")
            where
              keep_first hg_name exclamation = hg_name
              mk_pair    hg      name        = (hg, name)

-- Exercise 1.12
-- pThen3のタイプを指定し、その定義を書き留めて、新しいバージョンのpGreetingをテストします。
-- 同様に、後で必要になるpThen4を記述します。
pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks
  = [(combine v1 v2 v3, toks3) | (v1, toks1) <- p1 toks,
                                 (v2, toks2) <- p2 toks1,
                                 (v3, toks3) <- p3 toks2]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks
  = [(combine v1 v2 v3 v4, toks4) | (v1, toks1) <- p1 toks,
                                    (v2, toks2) <- p2 toks1,
                                    (v3, toks3) <- p3 toks2,
                                    (v4, toks4) <- p4 toks3]

{-
-- Pack{ <tag> , <arity> } をパースするために追加してみた。
pThen6 :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g
pThen6 combine p1 p2 p3 p4 p5 p6 toks
  = [(combine v1 v2 v3 v4 v5 v6, toks6) | (v1, toks1) <- p1 toks,
                                          (v2, toks2) <- p2 toks1,
                                          (v3, toks3) <- p3 toks2,
                                          (v4, toks4) <- p4 toks3,
                                          (v5, toks5) <- p5 toks4,
                                          (v6, toks6) <- p6 toks5]
-}

-- 文法のもう1つの非常に一般的な機能は、記号の0回以上の繰り返しを要求することです。
-- これを反映するために、パーサpを受け取り、pが認識する0回以上の出現を認識する新しいパーサを返す関数pZeroOrMoreが必要です。
-- 成功した解析によって返される値は、pの継続的な使用によって返される値のリストである可能性があります。
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

{-
*Language> pZeroOrMore (pLit "x") ["x", "x"]
[(["x","x"],[]),(["x"],["x"]),([],["x","x"])]
-}

pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting

-- pEmptyは常に成功するパーサーであり、入力から何も削除せず、最初の引数として指定された値を返します。
pEmpty :: a -> Parser a

pOneOrMore :: Parser a -> Parser [a]

-- Exercese 1.13
-- pOneOrMoreとpEmptyの定義を記述します。
-- (ヒント：pOneOrMoreからpZeroOrMoreを呼び出すと便利です。)
-- 1つまたは複数の挨拶を認識するパーサーを定義するためにそれらを使用して、定義をテストします。 
pEmpty val toks = [(val, toks)]
pOneOrMore p = pThen mk_list p (pZeroOrMore p)
               where
                 mk_list hd tl = hd : tl

{-
*Language> pOneOrMore (pLit "x") ["x", "x"]
[(["x","x"],[]),(["x"],["x"])]
-}

pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting) `pApply` length

pApply :: Parser a -> (a -> b) -> Parser b

-- Exercese 1.14
-- pApplyの定義を記述し、それをテストします。
-- (ヒント：リスト内包表記を使用してください。)
pApply p f toks = [(f v1, toks1) | (v1, toks1) <- p toks]

{-
*Language> import Data.Char

*Language Data.Char> :t toUpper
toUpper :: Char -> Char

*Language Data.Char> toUppers = map toUpper

*Language Data.Char> :t toUppers
toUppers :: [Char] -> [Char]

*Language Data.Char> :t pApply
pApply :: Parser a -> (a -> b) -> Parser b

*Language Data.Char> :t (pLit "x") `pApply` toUppers
(pLit "x") `pApply` toUppers :: Parser [Char]

*Language Data.Char> ((pLit "x") `pApply` toUppers) ["x", "x"]
[("X",["x"])]
-}

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]

-- Exercese 1.15
-- pOneOrMoreWithSepを定義してテストします。
-- プログラムの次の文法を考えると役立つ場合があります。
-- program → sc programRest
-- programRest → ; program
--              | ε
-- ここで、εは空の文字列(pEmptyパーサーに対応)です。
pOneOrMoreWithSep p1 p2 = pThen mk_list p1 (pZeroOrMore (pThen sel_snd p2 p1))
                          where
                            mk_list hd  tl  = hd : tl
                            sel_snd fst snd = snd

{-
*Template Data.Char> :t pOneOrMoreWithSep (pLit "x") (pLit ";")
pOneOrMoreWithSep (pLit "x") (pLit ";") :: Parser [String]

*Template Data.Char> (pOneOrMoreWithSep (pLit "x") (pLit ";")) ["x", ";", "x", ";", "x"]
[(["x","x","x"],[]),(["x","x"],[";","x"]),(["x"],[";","x",";","x"])]
-}

pSat :: (String -> Bool) -> Parser String

pLit s = pSat (== s)

{-
*Language Data.Char> pLit "x" ["x", "x"]
[("x",["x"])]

*Language Data.Char> :t pSat
pSat :: (String -> Bool) -> Parser String

*Language Data.Char> (pOneOrMoreWithSep (pLit "x") (pLit ";")) ["x", ";", "x", ";", "x"]
[(["x","x","x"],[]),(["x","x"],[";","x"]),(["x"],[";","x",";","x"])]
-}

-- Exercese 1.16
-- pSatを定義し、テストします。
-- pLitと同様の方法で、pSatの観点からpVarを記述します。
pSat pred []                       = []
pSat pred (tok : toks) | pred tok  = [(tok, toks)]
                       | otherwise = []

{-
--pVar toks = pSat (all isAlpha) toks  -- これでは、変数名は全文字英字に限定されてしまう。
pVar toks = pSat (\tok -> (isAlpha (hd tok)) && ((all isIdChar (tl tok)) || ((tl tok) == ""))) toks
-}

{-
*Language Data.Char> :t (all isAlpha)
(all isAlpha) :: Foldable t => t Char -> Bool

*Language Data.Char> pVar ["abc", "def", "123"]
[("abc",["def","123"])]
-}

-- Exercese 1.17
-- 上記のpVarの定義でpSatに渡される関数を変更して、リストキーワードの文字列を変数として扱わないようにします。
-- > keywords :: [String]
-- > keywords = ["let", "letrec", "case", "in", "of", "Pack"]

keywords :: [String]
--keywords = ["let", "letrec", "case", "in", "of", "Pack"]
keywords = ["let", "letrec", "case", "in", "of", "Pack", "Pack{"]

--pVar toks = pSat (\tok -> (all isAlpha tok) && (not (elem tok keywords))) toks  -- これでは、変数名は全文字英字に限定されてしまう。
pVar toks = pSat (\tok -> ((isAlpha (hd tok)) && ((all isIdChar (tl tok)) || ((tl tok) == ""))) && (not (elem tok keywords))) toks

{-
*Language Data.Char> *Language Data.Char> pVar ["abc", "def", "123"]
[("abc",["def","123"])]

*Language Data.Char> pVar ["let", "def", "123"]
[]

*Language Data.Char> pVar ["abc", "letrec", "123"]
[("abc",["letrec","123"])]

*Template> pVar ["K1", "def", "123"]
[("K1",["def","123"])]

*Language Data.Char> pLit "x" ["y", "y"]
[]

*Language Data.Char> pLit "x" ["x", "x"]
[("x",["x"])]
-}

-- Exercese 1.18

pNum :: Parser Int
pNum toks = ((pSat (all isDigit)) `pApply` read) toks

{-
*Language Data.Char> :t read
read :: Read a => String -> a

*Language Data.Char> read "123" :: Int
123

*Language Data.Char> :t (pSat (all isDigit))
(pSat (all isDigit)) :: Parser String

*Language Data.Char> (pSat (all isDigit)) ["123", "abc"]
[("123",["abc"])]

*Language Data.Char> pNum ["123", "abc"]
[(123,["abc"])]
-}

{-
-- Exercese 1.19
-- n = 5, 10, 15, 20などの場合、構文エラーが報告されるまでに必要なミランダステップの数を数えます。
-- (実行統計の表示を取得するには、Mirandaの/countディレクティブを使用します。)
-- nに関して、解析コストはどのくらいの速さで上昇しますか？
-- これが発生する理由を理解するには、以下を評価してみてください。
-- pOneOrMore (pLit "x") ["x", "x", "x", "x", "x", "x"]
-- 6つの可能な解析のリストを取得する必要があります。
-- これに基づいて、前の例の解析コストが非常に速く上昇する理由を理解できますか？
-- この問題はどのように解決できますか？
-- (ヒント：最初のものとは別に、pOneOrMoreによって返される解析のいずれかが役に立ちますか？
-- 余分なものをどのように排除できますか？)
-}

--
-- 1.6.4 Parsing the Core language
--

syntax = take_first_parse . pProgram
         where
           take_first_parse ((prog, []) : others) = prog
           take_first_parse (parse      : others) = take_first_parse others
--           take_first_parse other                 = error "Syntax error"
           take_first_parse other                 = error $ "Syntax error" ++ (show other)

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr

-- Exercise 1.20 (mk_sc)

mk_sc :: String -> [String] -> String -> CoreExpr -> CoreScDefn
mk_sc scName scArgs _ scBody = (scName, scArgs, scBody)

-- Exercise 1.21

{-
  f = 3 ;
  g x y = let z = x in z ;
  h x = case (let y = x in y) of
          <1> -> 2 ;
          <2> -> 5
-}

ex1_21 = ["f", "=", "3", ";", "g", "x", "y", "=", "let", "z", "=", "x", "in", "z", ";", "h", "x", "=", "case", "(", "let", "y", "=", "x", "in", "y", ")", "of", "<", "1", ">", "->", "2", ";", "<", "2", ">", "->", "5"]

ex1_21' = "f = 3 ; g x y = let z = x in z ; h x = case (let y = x in y) of <1> -> 2 ; <2> -> 5"

ex1_21'' = "f = 3 ; g x y = let z = x in z"

{-
*Main Heap Iseq Language Parser Paths_ifl_tut Stack Template.Mark1 Template.Mark2 Template.Mark3 Template.Mark4 Template.Mark5 Template.Mark5print Template.MultMachine Utils> *Main Heap Iseq Language Parser Paths_ifl_tut Stack Template.Mark1 Template.Mark2 Template.Mark3 Template.Mark4 Template.Mark5 Template.Mark5print Template.MultMachine Utils> parse ex1_21''
[("f",[],ENum 3),("g",["x","y"],ELet False [("z",EVar "x")] (EVar "z"))]
-}

{-
*Language> clex ex1_21'
["f","=","3",";","g","x","y","=","let","z","=","x","in","z",";","h","x","=","case","(","let","y","=","x","in","y",")","of","<","1",">","-",">","2",";","<","2",">","-",">","5"]

*Language> clex ex1_21''
["f","=","3",";","g","x","y","=","let","z","=","x","in","z"]
-}

--pExpr :: Parser (Expr a)
pExpr :: Parser (Expr String)
pExpr = ((pThen4 mk_letorletrec ((pLit "let") `pAlt` (pLit "letrec")) pDefns (pLit "in") pExpr) `pAlt`
         (pThen4 mk_lambda (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr) `pAlt`
         (pThen4 mk_case (pLit "case") pExpr (pLit "of") pAlters) `pAlt`
        pAexpr) `pAlt`  -- これをしないと、Pack{<tag>,<arity>}をパースできないので注意！
        pExpr1
        where
          mk_letorletrec letorletrec defns _ body = ELet (if letorletrec == "letrec" then True else False) defns (body)
          mk_case _ expr _ alters = ECase expr alters
          mk_lambda _ vars _ body = ELam vars (body)

--pDefns :: Parser [(String, Expr a)]
pDefns :: Parser [(String, Expr String)]
pDefns = pOneOrMoreWithSep pDefn (pLit ";")

--pDefn :: Parser (String, Expr a)
pDefn :: Parser (String, Expr String)
pDefn = pThen3 mk_defn pVar (pLit "=") pExpr
        where
          mk_defn var _ expr = (var, expr)

pAlters = pOneOrMoreWithSep pAlter (pLit ";")

pAlter = pThen4 mk_alter (pThen3 mk_int (pLit "<") pNum (pLit ">")) (pZeroOrMore pVar) (pLit "->") pExpr
         where
           mk_int _ num _ = num
           mk_alter num vars _ expr = (num, vars, expr)

pPackArgs = pZeroOrMore pExpr

{-
*Language> length "\\"
1

*Language> clex "\\ x. 2*x"
["\\","x",".","2","*","x"]

*Language> pLambda ["\\","x",".","2","*","x"]
[(ELam ["x"] (ENum 2),["*","x"])]  ※中置演算子に未対応のため、*xが残ってしまう。

*Language> pExpr ["\\","x",".","2","*","x"]
[(ELam ["x"] (ENum 2),["*","x"])]
-}

{-
pCase = pThen4 mk_case (pLit "case") pExpr (pLit "of") pAlters
        where
          mk_case _ expr _ alters = ECase expr alters
-}

{-
*Language> pCase ["case","c","of","<","1",">","->","2",";","<","2",">","->","5"]
[(ECase (EVar "c") [(1,[],ENum 2),(2,[],ENum 5)],[]),(ECase (EVar "c") [(1,[],ENum 2)],[";","<","2",">","->","5"])]

*Language> pExpr ["case","c","of","<","1",">","->","2",";","<","2",">","->","5"]
[(ECase (EVar "c") [(1,[],ENum 2),(2,[],ENum 5)],[]),(ECase (EVar "c") [(1,[],ENum 2)],[";","<","2",">","->","5"])]
-}

-- EApと中置演算子の処理はまだできない。

{-
*Language Data.Char> pExpr ["x", "y"]
[(EVar "x",["y"])]

*Language Data.Char> pExpr ["123", "def"]
[(ENum 123,["def"])]

*Language> pExpr ["1", "2", "3"]
[(EConstr 1 2,["3"]),(ENum 1,["2","3"])]

*Language> pLetOrLetrec ["let", "z", "=", "x", "in", "z"]
[(ELet False [("z",EVar "x")] (EVar "z"),[])]

*Language> pExpr ["let", "z", "=", "x", "in", "z"]
[(ELet False [("z",EVar "x")] (EVar "z"),[])]
-}

-- Exercise 1.22

--
-- 1.6.5 Left recursion
--

{-
pExpr = pThen EAp pExpr pAexpr
-}

pAexpr = (pThen4 mk_constr (pLit "Pack") (pLit "{") (pThen3 mk_tag_arity pNum (pLit ",") pNum) (pLit "}")) `pAlt`
         (pThen3 mk_expr (pLit "(") pExpr (pLit ")")) `pAlt`
         (pVar `pApply` EVar) `pAlt`
         (pNum `pApply` ENum)
         where
           mk_tag_arity tag _ arity = (tag, arity)
           mk_constr _ _ (tag, arity) _ = EConstr tag arity
           mk_expr _ expr _ = expr

{-
*Language> pAexpr ["Pack{", "1", "2", "}"]
[(EConstr 1 2,[])]

*Language> pAexpr ["(", "c", ")"]
[(EVar "c",[])]

*Language> pAexpr ["c"]
[(EVar "c",[])]

*Language> pAexpr ["1"]
[(ENum 1,[])]
-}

--pExpr = (pOneOrMore pAexpr) $ pApply mk_ap_chain

-- Exercise 1.23 (mk_ap_chain)

--
-- 1.6.6 Adding infix operators
--

data PartialExpr = NoOp
                 | FoundOp Name CoreExpr

pExpr1c :: Parser PartialExpr
pExpr1c = (pThen FoundOp (pLit "|") pExpr1) `pAlt` (pEmpty NoOp)

pExpr1 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c

pExpr2c :: Parser PartialExpr
pExpr2c = (pThen FoundOp (pLit "&") pExpr2) `pAlt` (pEmpty NoOp)

pExpr2 :: Parser CoreExpr
pExpr2 = pThen assembleOp pExpr3 pExpr2c

pExpr3c :: Parser PartialExpr
pExpr3c = (pThen FoundOp pRelop pExpr4) `pAlt` (pEmpty NoOp)
          where
            pRelop = (pSat (== "<")) `pAlt` (pSat (== "<=")) `pAlt` (pSat (== "==")) `pAlt`
                     (pSat (== "~=")) `pAlt` (pSat (== ">=")) `pAlt` (pSat (== ">"))

pExpr3 :: Parser CoreExpr
pExpr3 = pThen assembleOp pExpr4 pExpr3c

pExpr4c :: Parser PartialExpr
pExpr4c = (pThen FoundOp (pLit "+") pExpr5) `pAlt` (pThen FoundOp (pLit "-") pExpr5) `pAlt` (pEmpty NoOp)

pExpr4 :: Parser CoreExpr
pExpr4 = pThen assembleOp pExpr5 pExpr4c

pExpr5c :: Parser PartialExpr
pExpr5c = (pThen FoundOp (pLit "*") pExpr6) `pAlt` (pThen FoundOp (pLit "/") pExpr6) `pAlt` (pEmpty NoOp)

pExpr5 :: Parser CoreExpr
pExpr5 = pThen assembleOp pExpr6 pExpr5c

pExpr6 :: Parser (Expr String)
pExpr6 = (pOneOrMore pAexpr) `pApply` mk_ap_chain
         where
           mk_ap_chain xs = aux_fun (reverse xs)
                            where
                              aux_fun [x] = x
                              aux_fun (x : xs) = EAp (aux_fun xs) x

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

-- Exercise 1.24
-- 提案された行に沿って文法を変換し、変更をミランダコードに直訳して、結果のパーサーをテストします。

{-
*Language> pExpr ["1", "+", "2"]
[(EAp (EAp (EVar "+") (ENum 1)) (ENum 2),[]),(ENum 1,["+","2"])]

*Language> pExpr ["\\","x",".","2","*","x"]
[(ELam ["x"] (EAp (EAp (EVar "*") (ENum 2)) (EVar "x")),[]),(ELam ["x"] (ENum 2),["*","x"])]

*Language> pExpr ["case","c","of","<","1",">","->","2",";","<","2",">","->","5"]
[(ECase (EVar "c") [(1,[],ENum 2),(2,[],ENum 5)],[]),(ECase (EVar "c") [(1,[],ENum 2)],[";","<","2",">","->","5"])]

*Language> pExpr ["x", "y"]
[(EAp (EVar "x") (EVar "y"),[]),(EVar "x",["y"])]

*Language> pExpr ["123", "def"]
[(EAp (ENum 123) (EVar "def"),[]),(ENum 123,["def"])]

*Language> pExpr ["let", "z", "=", "x", "in", "z"]
[(ELet False [("z",EVar "x")] (EVar "z"),[])]

*Language> pAexpr ["(", "c", ")"]
[(EVar "c",[])]

*Language> pAexpr ["c"]
[(EVar "c",[])]

*Language> pAexpr ["1"]
[(ENum 1,[])]

*Language> pExpr ["case", "(", "let", "y", "=", "x", "in", "y", ")", "of", "<", "1", ">", "->", "2", ";", "<", "2", ">", "->", "5"]
[(ECase (ELet False [("y",EVar "x")] (EVar "y")) [(1,[],ENum 2),(2,[],ENum 5)],[]),(ECase (ELet False [("y",EVar "x")] (EVar "y")) [(1,[],ENum 2)],[";","<","2",">","->","5"])]

*Language> pExpr ["Pack{", "1", "2", "}"]
[(EConstr 1 2,[])]
-}

{-
*Template> (putStrLn . runProg) "main = K 1 2"
   1)stk [   1: NSupercom main ]

   2)stk [  11: NAp    9   10 (NNum 2) ]

   3)stk [   9: NAp    3    8 (NNum 1)
  11: NAp    9   10 (NNum 2) ]

   4)stk [   3: NSupercom K
   9: NAp    3    8 (NNum 1)  11: NAp    9   10 (NNum 2) ]

   5)stk [   8: NNum 1 ]  -- ついにMark1が動いた！！！



Total number of steps = 4
-}

{-
*Template> (putStrLn . runProg) "main = S K K 3"
   1)stk [   1: NSupercom main ]

   2)stk [  11: NAp    9   10 (NNum 3) ]

   3)stk [   9: NAp    8    3 (NSupercom K)
  11: NAp    9   10 (NNum 3) ]

   4)stk [   8: NAp    5    3 (NSupercom K)
   9: NAp    8    3 (NSupercom K)  11: NAp    9   10 (NNum 3) ]

   5)stk [   5: NSupercom S
   8: NAp    5    3 (NSupercom K)   9: NAp    8    3 (NSupercom K)
  11: NAp    9   10 (NNum 3) ]

   6)stk [  14: NAp   12   13 (NAp 3 10) ]

   7)stk [  12: NAp    3   10 (NNum 3)
  14: NAp   12   13 (NAp 3 10) ]

   8)stk [   3: NSupercom K
  12: NAp    3   10 (NNum 3)  14: NAp   12   13 (NAp 3 10) ]

   9)stk [  10: NNum 3 ]  -- ついにMark1が動いた！！！



Total number of steps = 8
-}
