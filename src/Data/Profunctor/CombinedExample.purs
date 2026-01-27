module Data.Profunctor.CombinedExample where

import Data.Profunctor (class Profunctor)
import Data.Profunctor.RecordsToRecords as RecordsToRecords
import Data.Profunctor.RecordsToVariants as RecordsToVariants
import Data.Profunctor.VariantsToRecords as VariantsToRecords
import Data.Profunctor.VariantsToVariants as VariantsToVariants
import Data.Unit (Unit)
import Data.Variant (Variant)

data Foo :: forall k1 k2. k1 -> k2 -> Type
data Foo a b = Foo

instance Profunctor Foo where
  dimap _ _ Foo = Foo

instance RecordsToRecords.RecordsToRecords Foo where
  recordsToRecords Foo Foo = Foo

instance RecordsToVariants.RecordsToVariants Foo where
  recordsToVariants Foo Foo = Foo

instance VariantsToRecords.VariantsToRecords Foo where
  variantsToRecords Foo Foo = Foo

instance VariantsToVariants.VariantsToVariants Foo where
  variantsToVariants Foo Foo = Foo

-- Basic examples from each module

recordsToRecordsExample :: Foo (Record ( field1 :: String , field2 :: Boolean )) (Record ( field3 :: Int , field4 :: Number ))
recordsToRecordsExample = RecordsToRecords.do
  (Foo :: Foo (Record ( "field1" :: String )) (Record ( "field3" :: Int )))
  (Foo :: Foo (Record ( "field2" :: Boolean )) (Record ( "field4" :: Number )))

recordsToVariantsExample :: Foo (Record ( field1 :: String , field2 :: Boolean )) (Variant ( case1 :: Int , case2 :: Number ))
recordsToVariantsExample = RecordsToVariants.do
  (Foo :: Foo (Record ( "field1" :: String )) (Variant ( "case1" :: Int )))
  (Foo :: Foo (Record ( "field2" :: Boolean )) (Variant ( "case2" :: Number )))

variantsToRecordsExample :: Foo (Variant ( case1 :: String , case2 :: Boolean )) (Record ( field1 :: Int , field2 :: Number ))
variantsToRecordsExample = VariantsToRecords.do
  (Foo :: Foo (Variant ( "case1" :: String )) (Record ( "field1" :: Int )))
  (Foo :: Foo (Variant ( "case2" :: Boolean )) (Record ( "field2" :: Number )))

variantsToVariantsExample :: Foo (Variant ( case1 :: String , case2 :: Boolean )) (Variant ( case3 :: Int , case4 :: Number ))
variantsToVariantsExample = VariantsToVariants.do
  (Foo :: Foo (Variant ( "case1" :: String )) (Variant ( "case3" :: Int )))
  (Foo :: Foo (Variant ( "case2" :: Boolean )) (Variant ( "case4" :: Number )))

-- Larger compositions

recordsToRecordsLarge :: Foo
  (Record ( name :: String , age :: Int , email :: String , active :: Boolean, note :: String ))
  (Record ( id :: Int , created :: String , updated :: String , version :: Number, versionSimple :: Number, note :: String, badge :: String ))
recordsToRecordsLarge = RecordsToRecords.do
  (Foo :: Foo (Record ( "name" :: String, "age" :: Int )) (Record ( "id" :: Int )))
  (Foo :: Foo (Record ( "age" :: Int )) (Record ( "created" :: String )))
  (Foo :: Foo (Record ()) (Record ( "note" :: String )))
  -- (Foo :: Foo (Record ()) (Record ( "note" :: String ))) -- does not compile "note" already exists
  (Foo :: Foo (Record ( "email" :: String )) (Record ( "updated" :: String )))
  (Foo :: Foo (Record ( "active" :: Boolean )) (Record ( "version" :: Number, "versionSimple" :: Number)))
  (Foo :: Foo (Record ( "note" :: String )) (Record ())) -- ignoring "note"
  (Foo :: Foo (Record ()) (Record ("badge" :: String))) -- constant "badge"

recordsToVariantsLarge :: Foo
  (Record ( username :: String , password :: String , password2 :: String, token :: String, token2 :: String ))
  (Variant ( success :: Int , invalidCredentials :: String , expired :: Boolean, exit :: Unit ))
recordsToVariantsLarge = RecordsToVariants.do
  (Foo :: Foo (Record ( "username" :: String, "password" :: String )) (Variant ( "success" :: Int, "invalidCredentials" :: String )))
  (Foo :: Foo (Record ( "password" :: String )) (Variant ( "invalidCredentials" :: String )))
  (Foo :: Foo (Record ( "password2" :: String )) (Variant ( "invalidCredentials" :: String )))
  (Foo :: Foo (Record ( "token" :: String )) (Variant ( "expired" :: Boolean )))
  (Foo :: Foo (Record ( "token2" :: String )) (Variant ())) -- ignoring "token2"
  (Foo :: Foo (Record ()) (Variant (exit :: Unit)))

variantsToVariantsLarge :: Foo
  (Variant ( click :: Int , hover :: String , hover2 :: String, scroll :: Number, press :: Number ))
  (Variant ( navigate :: String , highlight :: Boolean , resize :: Int, resize2 :: Int ))
variantsToVariantsLarge = VariantsToVariants.do
  (Foo :: Foo (Variant ( "click" :: Int )) (Variant ( "navigate" :: String )))
  (Foo :: Foo (Variant ( "hover" :: String, "hover2" :: String )) (Variant ( "highlight" :: Boolean, "navigate" :: String )))
  -- (Foo :: Foo (Variant ( "click" :: Int )) (Variant ( "highlight" :: Boolean ))) -- does not compile "click" already handled
  (Foo :: Foo (Variant ( "scroll" :: Number )) (Variant ( "resize" :: Int, "resize2" :: Int, "navigate" :: String )))
  (Foo :: Foo (Variant ( "press" :: Number )) (Variant ())) -- ignoring "press"
  (Foo :: Foo (Variant ()) (Variant ("navigate" :: String))) -- will be never chosen

variantsToRecordsLarge :: Foo
  (Variant ( get :: String , post :: String , delete :: Int ))
  (Record ( status :: Int , body :: String , headers :: String ))
variantsToRecordsLarge = VariantsToRecords.do
  (Foo :: Foo (Variant ( "get" :: String )) (Record ( "status" :: Int, "body" :: String )))
  -- (Foo :: Foo (Variant ( "post" :: String )) (Record ( "body" :: String ))) -- does not compile, "body" already exists
  (Foo :: Foo (Variant ( "delete" :: Int )) (Record ( "headers" :: String )))
  (Foo :: Foo (Variant ( "post" :: String )) (Record ())) -- ignoring "post"
  -- (Foo :: Foo (Variant ()) (Record ("headers" :: String))) -- will be never chosen

-- Single field/case examples

singleRecordToRecord :: Foo (Record ( only :: String )) (Record ( result :: Int ))
singleRecordToRecord = RecordsToRecords.do
  (Foo :: Foo (Record ( "only" :: String )) (Record ( "result" :: Int )))

singleVariantToVariant :: Foo (Variant ( input :: String )) (Variant ( output :: Int ))
singleVariantToVariant = VariantsToVariants.do
  (Foo :: Foo (Variant ( "input" :: String )) (Variant ( "output" :: Int )))
