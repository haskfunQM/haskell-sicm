module SICM.Symbolic.Function
    ( literalFunction
    ) where

import SICM.Core.Expr (Expr(..), Name)

-- | Construct an unknown symbolic function of one argument.
--
-- The result is an ordinary Haskell function:
--
--     q :: Expr -> Expr
--     q = literalFunction "q"
--
-- so ordinary Haskell application and composition work directly:
--
--     q t
--     (u . q) t
--
-- No new Function datatype is introduced.
literalFunction :: Name -> Expr -> Expr
literalFunction name argument =
    Apply name [argument]
