module Template where

import Data.List (intercalate)

type VarId = Int

-- * Prolog terms
-- ----------------------------------------------------------------------------

data Term            -- Corresponding term:
  = F String [Term]  -- f(t1,...,tn)
  | Var VarId        -- Xn
  deriving (Eq,Ord)

instance Show Term where
  show (F name []) = name
  show (F name terms) = name ++ "(" ++ intercalate "," (map show terms) ++ ")"
  show (Var n) = "X" ++ show n

-- | Occurs check.
--
-- @occurs n t@ returns 'True' if and only if @Var n@ occurs in @t@
occurs :: VarId -> Term -> Bool
occurs = error "not implemented"

-- * Substitutions
-- ----------------------------------------------------------------------------

type Substitution = [(VarId,Term)]

-- | Apply as substitution to a Term
applySubst :: Substitution -> Term -> Term
applySubst = error "not implemented"

-- | Concatenate two substitutions:
-- Apply the first substitution to all the terms in the second substitution,
-- then concatenate the lists.
conc :: Substitution -> Substitution -> Substitution
conc = error "not implemented"

-- | Unify two terms
unify1 :: Term -> Term -> Maybe Substitution
unify1 t1 t2 = unify [(t1,t2)]

-- | Solve a set of equations.
--
-- @unify eqs@ returns @Just t@ where @t@ is the most general unifier of
-- @eqs@ if it exists, otherwise it returns @Nothing@
unify :: [(Term,Term)] -> Maybe Substitution
unify = error "not implemented"
