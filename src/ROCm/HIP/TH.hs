{-# LANGUAGE TemplateHaskell #-}

module ROCm.HIP.TH where

import Data.List.NonEmpty (NonEmpty ((:|)), appendList)
import Language.Haskell.TH

checked :: Name -> Q [Dec]
checked func = do
  info <- reify func
  case info of
    VarI _ typ _ -> do
      let (args, ret) = unfoldArrow typ
      (new_ret, ret_cnt) <-
        case chopRC ret of
          Nothing -> fail $ show func ++ " doesn't return code. " ++ show ret
          Just rt -> return rt
      let new_typ = foldArrow args new_ret
      declareChecked ret_cnt args new_typ func
    _ -> fail $ show func ++ " isn't a function."
  where
    -- note, we don't check the reason code is really typed HipError, because
    -- importing the internal module will impose the dependency on amdhip64.so
    -- for template haskell. As result, we check rc == toEnum 0 instead of HipSuccess

    chopRC ty =
      case ty of
        AppT (ConT io) (ConT _) | io == ''IO -> Just (AppT (ConT io) (ConT ''()), 0)
        AppT (ConT io) tup
          | io == ''IO ->
              let _ :| ts = unfoldTuple tup
                  rt = foldTuple ts
               in Just (AppT (ConT io) rt, length ts)
        _ -> Nothing

    declareChecked cnt args new_typ name = do
      arg_names <- mapM (\i -> newName ("x" ++ show i)) [1 .. length args]

      let call_orig = return $ foldl AppE (VarE name) (map VarE arg_names)
          handle_err = [e|\rc rt -> if rc == toEnum 0 then return rt else error ("HIP error: " ++ show rc)|]

      body <- case cnt of
        0 ->
          [e|
            do
              rc <- $(call_orig)
              $(handle_err) rc ()
            |]
        1 ->
          [e|
            do
              (rc, rt1) <- $(call_orig)
              $(handle_err) rc rt1
            |]
        2 ->
          [e|
            do
              (rc, rt1, rt2) <- $(call_orig)
              $(handle_err) rc (rt1, rt2)
            |]

      let fun_clause = Clause (map VarP arg_names) (NormalB body) []
          new_name = mkName (nameBase name)

      return [SigD new_name new_typ, FunD new_name [fun_clause]]

unfoldArrow :: Type -> ([Type], Type)
unfoldArrow (AppT (AppT ArrowT a) b) = let (args, res) = unfoldArrow b in (a : args, res)
unfoldArrow t = ([], t)

foldArrow :: [Type] -> Type -> Type
foldArrow [] t = t
foldArrow (a : args) t = AppT (AppT ArrowT a) (foldArrow args t)

unfoldTuple :: Type -> NonEmpty Type
unfoldTuple (AppT (AppT (TupleT _) a) b) = a :| [b]
unfoldTuple (AppT inner n) = unfoldTuple inner `appendList` [n]
unfoldTuple t = error $ "failed to unfold a non-tuple type: " ++ show t

foldTuple :: [Type] -> Type
foldTuple [] = error "not enough types to form a tuple"
foldTuple [t] = t
foldTuple ts = let size = length ts in foldl AppT (TupleT size) ts
