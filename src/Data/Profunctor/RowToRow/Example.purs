module Data.Profunctor.RowToRow.Example where

import Data.Profunctor (class Profunctor)
import Data.Profunctor.RowToRow.RecordToRecord as RecordToRecord
import Data.Profunctor.RowToRow.RecordToVariant (RecordToVariantPrim)
import Data.Profunctor.RowToRow.RecordToVariant as RecordToVariant
import Data.Profunctor.RowToRow.VariantToRecord as VariantToRecord
import Data.Profunctor.RowToRow.VariantToVariant as VariantToVariant
import Data.Unit (Unit)
import Data.Variant (Variant)
import Effect.Exception.Unsafe (unsafeThrow)

data MyRowToRowProfunctor :: forall k1 k2. k1 -> k2 -> Type
data MyRowToRowProfunctor a b = MyRowToRowProfunctor

instance Profunctor MyRowToRowProfunctor where
  dimap _ _ MyRowToRowProfunctor = MyRowToRowProfunctor

instance RecordToRecord.RecordToRecord MyRowToRowProfunctor where
  recordToRecord MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor

instance RecordToVariant.RecordToVariant MyRowToRowProfunctor where
  recordToVariant MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor

instance VariantToRecord.VariantToRecord MyRowToRowProfunctor where
  variantToRecord MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor

instance VariantToVariant.VariantToVariant MyRowToRowProfunctor where
  variantToVariant MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor

-- A button named by `name`, from a dynamic `r` to an event `name`
button :: forall @casename r. RecordToVariantPrim MyRowToRowProfunctor casename (Record r) r
button = unsafeThrow "not implemented"

-- see inferred type
foo = RecordToVariant.do
  button @"save"
  button @"delete"

-- Basic examples from each module

recordsToRecordsExample :: MyRowToRowProfunctor (Record ( field1 :: String , field2 :: Boolean )) (Record ( field3 :: Int , field4 :: Number ))
recordsToRecordsExample = RecordToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "field1" :: String )) (Record ( "field3" :: Int )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "field2" :: Boolean )) (Record ( "field4" :: Number )))

recordsToVariantsExample :: MyRowToRowProfunctor (Record ( field1 :: String , field2 :: Boolean )) (Variant ( case1 :: Int , case2 :: Number ))
recordsToVariantsExample = RecordToVariant.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "field1" :: String )) (Variant ( "case1" :: Int )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "field2" :: Boolean )) (Variant ( "case2" :: Number )))

variantsToRecordsExample :: MyRowToRowProfunctor (Variant ( case1 :: String , case2 :: Boolean )) (Record ( field1 :: Int , field2 :: Number ))
variantsToRecordsExample = VariantToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "case1" :: String )) (Record ( "field1" :: Int )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "case2" :: Boolean )) (Record ( "field2" :: Number )))

variantsToVariantsExample :: MyRowToRowProfunctor (Variant ( case1 :: String , case2 :: Boolean )) (Variant ( case3 :: Int , case4 :: Number ))
variantsToVariantsExample = VariantToVariant.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "case1" :: String )) (Variant ( "case3" :: Int )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "case2" :: Boolean )) (Variant ( "case4" :: Number )))

-- Larger compositions

recordsToRecordsLarge :: MyRowToRowProfunctor
  (Record ( name :: String , age :: Int , email :: String , active :: Boolean, note :: String ))
  (Record ( id :: Int , created :: String , updated :: String , version :: Number, versionSimple :: Number, note :: String, badge :: String ))
recordsToRecordsLarge = RecordToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "name" :: String, "age" :: Int )) (Record ( "id" :: Int )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "age" :: Int )) (Record ( "created" :: String )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ()) (Record ( "note" :: String )))
  -- (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ()) (Record ( "note" :: String ))) -- does not compile "note" already exists
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "email" :: String )) (Record ( "updated" :: String )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "active" :: Boolean )) (Record ( "version" :: Number, "versionSimple" :: Number)))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "note" :: String )) (Record ())) -- ignoring "note"
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ()) (Record ("badge" :: String))) -- constant "badge"

recordsToVariantsLarge :: MyRowToRowProfunctor
  (Record ( username :: String , password :: String , password2 :: String, token :: String, token2 :: String ))
  (Variant ( success :: Int , invalidCredentials :: String , expired :: Boolean, exit :: Unit ))
recordsToVariantsLarge = RecordToVariant.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "username" :: String, "password" :: String )) (Variant ( "success" :: Int, "invalidCredentials" :: String )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "username" :: String, "password" :: String )) (Variant ( "invalidCredentials" :: String )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "password2" :: String )) (Variant ( "invalidCredentials" :: String )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "token" :: String )) (Variant ( "expired" :: Boolean )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "token2" :: String )) (Variant ())) -- ignoring "token2"
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ()) (Variant (exit :: Unit)))

variantsToVariantsLarge :: MyRowToRowProfunctor
  (Variant ( click :: Int , hover :: String , hover2 :: String, scroll :: Number, press :: Number ))
  (Variant ( navigate :: String , highlight :: Boolean , resize :: Int, resize2 :: Int ))
variantsToVariantsLarge = VariantToVariant.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "click" :: Int )) (Variant ( "navigate" :: String )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "hover" :: String, "hover2" :: String )) (Variant ( "highlight" :: Boolean, "navigate" :: String )))
  -- (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "click" :: Int )) (Variant ( "highlight" :: Boolean ))) -- does not compile "click" already handled
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "scroll" :: Number )) (Variant ( "resize" :: Int, "resize2" :: Int, "navigate" :: String )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "press" :: Number )) (Variant ())) -- ignoring "press"
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ()) (Variant ("navigate" :: String))) -- will be never chosen

variantsToRecordsLarge :: MyRowToRowProfunctor
  (Variant ( get :: String , post :: String , delete :: Int ))
  (Record ( status :: Int , body :: String , headers :: String ))
variantsToRecordsLarge = VariantToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "get" :: String )) (Record ( "status" :: Int, "body" :: String )))
  -- (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "post" :: String )) (Record ( "body" :: String ))) -- does not compile, "body" already exists
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "delete" :: Int )) (Record ( "headers" :: String )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "post" :: String )) (Record ())) -- ignoring "post"
  -- (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ()) (Record ("headers" :: String))) -- will be never chosen

-- Single field/case examples

singleRecordToRecord :: MyRowToRowProfunctor (Record ( only :: String )) (Record ( result :: Int ))
singleRecordToRecord = RecordToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "only" :: String )) (Record ( "result" :: Int )))

singleVariantToVariant :: MyRowToRowProfunctor (Variant ( input :: String )) (Variant ( output :: Int ))
singleVariantToVariant = VariantToVariant.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "input" :: String )) (Variant ( "output" :: Int )))
