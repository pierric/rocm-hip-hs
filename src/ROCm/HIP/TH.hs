{-# LANGUAGE TemplateHaskell #-}

module ROCm.HIP.TH where

import Language.Haskell.TH

checked :: Name -> Q [Dec]
checked func = do
    info <- reify func
    case info of
        VarI _ typ _ -> do
            let (args, ret) = unfoldArrow typ
            (new_ret, is_void) <- 
                case chopRC ret of
                    Nothing -> fail $ show func ++ " doesn't return code. " ++ show ret
                    Just rt -> return rt
            let new_typ = foldArrow args new_ret
            declareChecked is_void args new_typ func
        _ -> fail $ show func ++ " isn't a function."
    where

    -- note, we don't check the reason code is really typed HipError, because
    -- importing the internal module will impose the dependency on amdhip64.so
    -- for template haskell. As result, we check rc == toEnum 0 instead of HipSuccess

    chopRC ty =
        case ty of
            AppT (ConT io) (ConT _) | io == ''IO -> Just (AppT (ConT io) (ConT ''()), True)
            AppT (ConT io) (AppT (AppT (TupleT 2) _) rt) | io == ''IO -> Just (AppT (ConT io) rt, False)
            _ -> Nothing
    
    declareChecked is_void args new_typ name = do
        arg_names <- mapM (\i -> newName ("x" ++ show i)) [1 .. length args]

        let call_orig = return $ foldl AppE (VarE name) (map VarE arg_names)

        body <- if not is_void then 
                    [e| do (rc, rt) <- $(call_orig)
                           if rc == toEnum 0 then return rt else error ("HIP error: " ++ show rc)
                    |]
                else
                    [e| do rc <- $(call_orig)
                           if rc == toEnum 0 then return () else error ("HIP error: " ++ show rc)
                    |]
        
        let fun_clause = Clause (map VarP arg_names) (NormalB body) []
            new_name = mkName (nameBase name)

        return [SigD new_name new_typ, FunD new_name [fun_clause]]


unfoldArrow :: Type -> ([Type], Type)
unfoldArrow (AppT (AppT ArrowT a) b) = let (args, res) = unfoldArrow b in (a : args, res)
unfoldArrow t = ([], t)

foldArrow :: [Type] -> Type -> Type
foldArrow [] t = t
foldArrow (a:args) t = AppT (AppT ArrowT a) (foldArrow args t)
