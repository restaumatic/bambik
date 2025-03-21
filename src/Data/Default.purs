module Data.Default
  ( class Default
  , class RecordDefault
  , defaultRecord
  , default
  ) where

import Prelude

import Prim.RowList as RowList
import Record as Record
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Prim.Row as Row

class Default f where
  default :: f

instance Default String where
  default = ""

instance Default Unit where
  default = unit

instance Default (Array a) where
  default = []

class RecordDefault :: forall k. k -> Row Type -> Constraint
class RecordDefault rl r | rl -> r where
  defaultRecord :: Proxy rl -> Record r

instance ( IsSymbol name
         , Default value
         , Row.Cons name value tailRow row
         , RecordDefault tailRowList tailRow -- type level recursion is here
         , Row.Lacks name tailRow
         )
      => RecordDefault (RowList.Cons name value tailRowList) row where
  defaultRecord _ = Record.insert (Proxy @name) default (defaultRecord (Proxy @tailRowList))

instance RecordDefault RowList.Nil () where
  defaultRecord _ = {}

instance (RowList.RowToList r rl
         , RecordDefault rl r)
        =>  Default (Record r) where
  default = defaultRecord (Proxy @rl)