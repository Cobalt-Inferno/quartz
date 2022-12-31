module Errors
  ( ErrorType(..)
  , Error(..)
  , constructError
  ) where

import Data.Text (Text)

data ErrorType
  = BannedWord
  | UndefinedError
  deriving (Show, Eq)

-- ErrorType -> ErrorMSG -> ErrorID
data Error =
  Error ErrorType Text Int
  deriving (Show, Eq)

constructError :: ErrorType -> Text -> Error
constructError _error str =
  case _error of
    BannedWord -> Error _error str 0
    UndefinedError -> Error _error str 420
