# ifl-tut ( Implementing Functional Languages: a tutorial)

## コア言語

* **Language.hs**

## テンプレートインスタンス化マシン

### Mark 1: 最小限のテンプレートインスタンス化グラフ簡約器

* **mark1.hs**

### Mark 2: let(rec)式

* **mark2.hs**

### Mark 3: 更新機能の追加

### Mark 4: 算術演算機能の追加

### Mark 5: 構造化データ型の追加

### 代替実装

### ガベージコレクション

## Gマシン (グラフ簡約マシン)

### Mark 1: 最小限のGマシン

* **gmachinemk1.hs**

* **gmachinemk1_2.hs**

  演習 3.6 まで対応。

### Mark 2: 遅延評価させる

* **gmachinemk2.hs**

### Mark 3: let(rec)式

* **gmachinemk3.hs**

### Mark 4: プリミティブの追加

* **gmachinemk4.hs**

### Mark 5: 算術演算のより良い処理に向けて

* **gmachinemk5.hs**

### Mark 6: データ構造の追加

* **gmachinemk6.hs**

  演習 3.36 まで対応。

* **gmachinemk6_2.hs**

  演習 3.37 まで対応。

* **gmachinemk6_3.hs**

  演習 3.38 まで対応。

### Mark 7: さらなる改善

* **gmachinemk7.hs**

  演習 3.43 まで対応。

* **gmachinemk7_2.hs**

  演習 3.44 まで対応。

* **gmachinemk7_3.hs**

  演習 3.45 まで対応。

* **gmachinemk7_4.hs**

  演習 3.46 まで対応。

* **gmachinemk7_5.hs**

  演習 3.47 まで対応。

## TIM (3命令マシン)

### Mark 1: 最小限のTIM

* **timmk1.hs**

  演習 4.1 まで対応。

* **timmk1_2.hs**

  演習 4.2 まで対応。

* **timmk1_3.hs**

  演習 4.3 まで対応。

* **timmk1_4.hs**

  ガベージコレクション対応。(4.2.6)

### Mark 2: 算術演算の追加

* **timmk2.hs**

  演習 4.4 まで対応。

* **timmk2_2.hs**

  演習 4.6 まで対応。

* **timmk2_3.hs**

  演習 4.8 まで対応。

### Mark 3: let(rec)式

* **timmk3.hs**

  演習 4.15 まで対応。

  ガベージコレクション対応。(4.4.4)

### Mark 4: 更新

* **timmk4.hs**

  演習 4.16 まで対応。

* **timmk4_2.hs**

  演習 4.17 まで対応。

* **timmk4_3.hs**

  演習 4.18 まで対応。

* **timmk4_4.hs**

  演習 4.19 まで対応。

* **timmk4_5.hs**

  演習 4.20 まで対応。

* **timmk4_6.hs**

  演習 4.21 まで対応。

* **timmk4_7.hs**

  演習 4.22 まで対応。

### Mark 5: 構造化データ

* **timmk5.hs**

  演習 4.23 まで対応。

* **timmk5_2.hs**

  演習 4.24 まで対応。

* **timmk5_3.hs**

  演習 4.25 まで対応。

* **timmk5_4.hs**

  演習 4.26 まで対応。

* **timmk5_5.hs**

  データ構造を直接使用する対応。(4.6.5)

### Mark 6: Constant Applicative Forms (定作用形)とコードストア

* **timmk6.hs**

  演習 4.27 まで対応。

* **timmk6_2.hs**

  演習 4.28 まで対応。

* **timmk6_3.hs**

  演習 4.29 まで対応。

* **timmk6_4.hs**

  演習 4.30 まで対応。

## 並列Gマシン

### Mark 1: 最小限の並列Gマシン

* **pgmmk1.hs**

  演習 5.5 まで対応。

  (Mark 7: シーケンシャルGマシンをベースに実装。)

### Mark 2: 評価して終了するモデル

* **pgmmk2.hs**

  演習 5.9 まで対応。

  (Mark 7: シーケンシャルGマシンをベースに実装。)

### Mark 3: 現実的な並列Gマシン

* **pgmmk3.hs**

  演習 5.13 の手前(スケジューリングがラウンドロビン方式で行われていることの確認)まで対応。

  (Mark 7: シーケンシャルGマシンをベースに実装。)

* **pgmmk3_2.hs**

  演習 5.14 まで対応。

  (Mark 7: シーケンシャルGマシンをベースに実装。)

* **pgmmk3_3.hs**

  演習 5.15 まで対応。

  (Mark 7: シーケンシャルGマシンをベースに実装。)

* **pgmmk3_4.hs**

  演習 5.16 まで対応。

  (Mark 7: シーケンシャルGマシンをベースに実装。)

### Mark 4: ブロックを処理するためのより良い方法

* **pgmmk4.hs**

  演習 5.17 から演習 5.23 まで対応。

  (Mark 7: シーケンシャルGマシンをベースに実装。)

## ラムダリフティング

### Mark 1: シンプルなラムダリフター

* lambdaliftermk1.hs

### Mark 2: シンプルなラムダリフターの改良

* lambdaliftermk2_1.hs

  演習 6.3 まで対応。

* lambdaliftermk2_2.hs

  演習 6.4 まで対応。

### Mark 3: Johnsson-style lambda lifter

### Mark 4: A separate full laziness pass

### Mark 5: Improvements to full laziness

### Mark 6: Dependency analysis

## ユーティリティモジュール

* **Utils.hs**
