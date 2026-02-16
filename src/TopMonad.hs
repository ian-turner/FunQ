module TopMonad where

import Nominal
import Control.Monad.Except
import Control.Monad.Identity
import Control.Exception
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Text.Parsec hiding (count)

import Syntax as A
import ConcreteSyntax as C
import Parser
import Resolve
import Utils


-- | Top-level errors
data Error
  = ParseErr ParseError
  | ScopeErr ScopeError
  deriving (Show)
