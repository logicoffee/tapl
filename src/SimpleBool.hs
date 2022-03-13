module SimpleBool where

data Type
  = TyArr Type Type
  | TyBool
  deriving (Show, Eq)

data Term
  = TmVar Int
  | TmAbs String Type Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  deriving (Show, Eq)

type Context = [(String, Binding)]

data Binding = NameBind | VarBind Type

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx

getBinding :: Context -> Int -> Maybe Binding
getBinding ctx i =
  if length ctx > i
    then Just $ let (_, bind) = ctx !! i in bind
    else Nothing

getTypeFromContext :: Context -> Int -> Maybe Type
getTypeFromContext ctx i = do
  bind <- getBinding ctx i
  case bind of
    VarBind ty -> pure ty
    _ -> Nothing

-- 戻り値の型を Either String Type とかにして、型エラーの情報を保持しても良い
typeof :: Context -> Term -> Maybe Type
typeof ctx term = case term of
  TmVar i -> getTypeFromContext ctx i
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
