module Untyped where

data Term
  = TmVar Int
  | TmAbs Term
  | TmApp Term Term
  deriving (Show, Eq)

termShift :: Int -> Term -> Term
termShift = termShift' 0
  where
    termShift' :: Int -> Int -> Term -> Term
    termShift' c d term = case term of
      TmVar k ->
        if k < c
          then TmVar k
          else TmVar $ k + d
      TmAbs subTerm -> TmAbs $ termShift' (c + 1) d subTerm
      TmApp t1 t2 -> TmApp (termShift' c d t1) (termShift' c d t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s = termSubst' 0
  where
    termSubst' :: Int -> Term -> Term
    termSubst' c term = case term of
      TmVar k ->
        if k == j + c
          then termShift c s
          else TmVar k
      TmAbs subTerm -> TmAbs $ termSubst' (c + 1) subTerm
      TmApp t1 t2 -> TmApp (termSubst' c t1) (termSubst' c t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t =
  termShift (-1) $ termSubst 0 (termShift 1 s) t

data Binding

type Context = [(String, Binding)]

-- これ使うと値レベルの比較になるから微妙
-- eval1 の中でパターンマッチしたほうがきれいに書ける
-- あとあと ctx が効いてくるときには isval を定義しておいたほうがいいのかもしれないけど
isval :: Context -> Term -> Bool
isval _ term = case term of
  TmAbs _ -> True
  _ -> False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx term = case term of
  TmApp (TmAbs t1) v2@(TmAbs t2) -> Just $ termSubstTop v2 t1
  TmApp x@(TmAbs t1) t2 -> TmApp x <$> eval1 ctx t2
  TmApp t1 t2 -> flip TmApp t2 <$> eval1 ctx t1
  x -> Nothing

eval :: Context -> Term -> Term
eval ctx term = maybe term (eval ctx) (eval1 ctx term)

-- Exercise 5.3.8 の結果を参考に
evalBigStep :: Context -> Term -> Term
evalBigStep ctx term = case term of
  TmApp t1 t2 -> case evalBigStep ctx t1 of
    TmAbs v1 ->
      let t2' = evalBigStep ctx t2
       in evalBigStep ctx $ termSubstTop t2' v1
    _ -> term
  x -> x
