module Data.Default
  ( class Default
  , class RecordDefault
  , defaultRecord
  , default
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList as RowList
import Record as Record
import Type.Proxy (Proxy(..))

class Default f where
  default :: f

instance Default String where
  default = ""

instance Default Unit where
  default = unit

instance Default (Array a) where
  default = []

instance Default (Maybe a) where
  default = Nothing

newtype OptIn = OptIn { optIn :: Boolean }

instance Default OptIn where
  default = OptIn { optIn: false }

newtype OptOut = OptOut { optOut :: Boolean }

instance Default OptOut where
  default = OptOut { optOut: true }

-- without default
newtype Foo a = Foo { foo :: Maybe a }

instance Default (Foo a) where
  default = Foo { foo: Nothing }

-- with default
newtype Bar a = Bar { bar :: Maybe a }

instance Default a => Default (Bar a) where
  default = Bar { bar: default }



class RecordDefault :: forall k. k -> Row Type -> Constraint
class RecordDefault rl r | rl -> r where
  defaultRecord :: Proxy rl -> Record r

instance ( IsSymbol name
         , Default value
         , Row.Cons name value tailRow row
         , RecordDefault tailRowList tailRow
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