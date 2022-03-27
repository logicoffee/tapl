module Let where

data Type
  = TyBool
  | TyArr Type Type
  deriving (Show, Eq)

data Term
  = TmVar Int
  | TmLet VarName Term Term
  | TmAbs VarName Type Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  deriving (Show, Eq)

data Binding
  = NameBind
  | VarBind Type

type VarName = String

type Context = [(VarName, Binding)]

addBinding :: Context -> VarName -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx

getBinding :: Context -> Int -> Maybe Binding
getBinding ctx n = do
  (_, bind) <- ctx `at` n
  pure bind

isNameBound :: Context -> VarName -> Bool
isNameBound ctx x = case ctx of
  [] -> False
  (y, _) : rest -> y == x || isNameBound rest x

pickFreshName :: Context -> VarName -> (Context, VarName)
pickFreshName ctx x
  | isNameBound ctx x = pickFreshName ctx (x <> "'")
  | otherwise = ((x, NameBind) : ctx, x)

index2name :: Context -> Int -> Maybe VarName
index2name ctx n = do
  (x, _) <- ctx `at` n
  pure x

name2index :: Context -> VarName -> Maybe Int
name2index ctx x = case ctx of
  [] -> Nothing
  (y, _) : rest ->
    if y == x
      then Just 0
      else fmap (1 +) (name2index rest x)

tmMap :: (Int -> Int -> Term) -> Int -> Term -> Term
tmMap onvar c term = walk c term
  where
    walk c term = case term of
      TmVar n -> onvar c n
      TmLet x t1 t2 -> TmLet x (walk c t1) (walk (c + 1) t2)
      TmTrue -> TmTrue
      TmFalse -> TmFalse
      TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
      TmAbs x ty t2 -> TmAbs x ty (walk (c + 1) t2)
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)

termShift :: Int -> Term -> Term
termShift d term = tmMap (\c x -> if x >= c then TmVar (x + d) else TmVar x) 0 term

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = tmMap (\c x -> if x == j then termShift j s else TmVar x) j t

termSubstTop :: Term -> Term -> Term
termSubstTop s t =
  termShift (-1) $ termSubst 0 (termShift 1 s) t

isval :: Term -> Bool
isval term = case term of
  TmTrue -> True
  TmFalse -> True
  TmAbs {} -> True
  _ -> False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx term = case term of
  TmLet x t1 t2
    | isval t1 -> Just $ termSubstTop t1 t2
    | otherwise -> do
      t1' <- eval1 ctx t1
      pure $ TmLet x t1' t2
  TmIf TmTrue t2 _ -> Just t2
  TmIf TmFalse _ t3 -> Just t3
  TmIf t1 t2 t3 -> do
    t1' <- eval1 ctx t1
    pure $ TmIf t1' t2 t3
  TmApp lambda@(TmAbs x ty t12) t2 ->
    if isval t2
      then Just $ termSubstTop t2 t12
      else TmApp lambda <$> eval1 ctx t2
  TmApp t1 t2
    | isval t1 -> TmApp t1 <$> eval1 ctx t2
    | otherwise -> flip TmApp t2 <$> eval1 ctx t1
  _ -> Nothing

eval :: Context -> Term -> Term
eval ctx term = maybe term (eval ctx) (eval1 ctx term)

getTypeFromContext :: Context -> Int -> Maybe Type
getTypeFromContext ctx i = do
  bind <- getBinding ctx i
  case bind of
    VarBind ty -> pure ty
    _ -> Nothing

typeof :: Context -> Term -> Maybe Type
typeof ctx term = case term of
  TmVar i -> getTypeFromContext ctx i
  TmLet x t1 t2 -> do
    ty1 <- typeof ctx t1
    let ctx' = addBinding ctx x (VarBind ty1)
    typeof ctx' t2
  TmAbs x ty1 tm ->
    let ctx' = addBinding ctx x (VarBind ty1)
        ty2 = typeof ctx' tm
     in ty2 >>= Just . TyArr ty1
  TmApp tm1 tm2 -> do
    ty1 <- typeof ctx tm1
    ty2 <- typeof ctx tm2
    case ty1 of
      TyArr ty11 ty12 ->
        if ty11 == ty2
          then pure ty12
          else Nothing
      _ -> Nothing
  TmTrue -> Just TyBool
  TmFalse -> Just TyBool
  TmIf tm1 tm2 tm3 -> do
    ty1 <- typeof ctx tm1
    ty2 <- typeof ctx tm2
    ty3 <- typeof ctx tm3
    if ty1 == TyBool && ty2 == ty3
      then pure ty2
      else Nothing

at :: [a] -> Int -> Maybe a
at arr i
  | length arr >= i = Just (arr !! i)
  | otherwise = Nothing
