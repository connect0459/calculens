# calculens

## 概要

**calculens** は、Haskellで実装された数式評価REPLツールです。  
コマンドライン上で数式を入力し、即座に計算結果を得ることができます。  
変数や定数（例: `pi`, `e`）も利用可能です。

## インストール

### 必要条件

- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)  
  または  
- [Cabal](https://www.haskell.org/cabal/)

### インストール手順

```sh
git clone https://github.com/githubuser/calculens.git
cd calculens
stack build
```

または

```sh
cabal build
```

## 使い方

ビルド後、以下のコマンドでREPLを起動できます。

```sh
stack exec calculens-exe
```

または

```sh
cabal run calculens-exe
```

REPLが起動したら、数式を入力してください。  
終了するには `quit` または `exit` と入力します。

例:

```bash
calculens> 1 + 2 * 3
結果: 7.0
calculens> pi * 2
結果: 6.283185307179586
calculens> quit
```

## ライセンス

本プロジェクトはBSD 3-Clauseライセンスの下で公開されています。  
詳細は [LICENSE](./LICENSE) ファイルをご覧ください。

## 著者

[connect0459](https://github.com/connect0459)
