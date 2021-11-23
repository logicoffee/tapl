module Arith where

data Term
  = TTrue
  | TFalse
  | TIf Term Term Term
  | TZero
  | TSucc Term
  | TPred Term
  | TIsZero Term

isNumericVal :: Term -> Bool
isNumericVal t = case t of
  TZero -> True
  TSucc t' -> isNumericVal t'
  _ -> False

isVal :: Term -> Bool
isVal t = case t of
  TTrue -> True
  TFalse -> True
  t -> isNumericVal t

eval1 :: Term -> Maybe Term
eval1 t = case t of
  TIf TTrue t2 _ -> Just t2
  TIf TFalse _ t3 -> Just t3
  TIf t1 t2 t3 -> do
    t1' <- eval1 t1
    pure $ TIf t1' t2 t3
  TSucc t -> TSucc <$> eval1 t
  TPred TZero -> Just TZero
  TPred t -> TPred <$> eval1 t
  TIsZero TZero -> Just TTrue
  TIsZero (TSucc t) ->
    if isNumericVal t
      then Just TFalse
      else Nothing
  TIsZero t -> TIsZero <$> eval1 t
  _ -> Nothing

eval :: Term -> Term
eval t = maybe t eval (eval1 t)

evalBigStep :: Term -> Term
evalBigStep t = case t of
  x@(TIf t1 t2 t3) -> case evalBigStep t1 of
    TTrue -> evalBigStep t2
    TFalse -> evalBigStep t3
    _ -> x
  x@(TSucc t) ->
    let t' = evalBigStep t
     in if isNumericVal t' then TSucc t' else x
  x@(TPred t) -> case evalBigStep t of
    TZero -> TZero
    TSucc t' -> if isNumericVal t' then t' else x
    _ -> x
  x@(TIsZero t) -> case evalBigStep t of
    TZero -> TTrue
    TSucc t' -> if isNumericVal t' then TFalse else x
    _ -> x
  t -> t
