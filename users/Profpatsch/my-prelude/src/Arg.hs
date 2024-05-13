module Arg where

import Data.String (IsString)
import GHC.Exts (IsList)
import GHC.TypeLits (Symbol)

-- | Wrap a function argument into this helper to give it a better description for the caller without disturbing the callsite too much.
--
-- This has instances for IsString and Num, meaning if the caller is usually a string or number literal, it should Just Work.
--
-- e.g.
--
-- @
-- myFoo :: Arg "used as the name in error message" Text -> IO ()
-- myFoo (Arg name) = …
-- @
--
-- Will display the description in the inferred type of the callsite.
--
-- Due to IsString you can call @myFoo@ like
--
-- @myFoo "name in error"@
--
-- This is mostly intended for literals, if you want to wrap arbitrary data, use @Label@.
newtype Arg (description :: Symbol) a = Arg {unArg :: a}
  deriving newtype
    ( Show,
      Eq,
      IsString,
      IsList,
      Num,
      Monoid,
      Semigroup
    )
